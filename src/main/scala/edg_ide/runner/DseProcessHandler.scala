package edg_ide.runner

import com.intellij.execution.process.ProcessHandler
import com.intellij.execution.ui.{ConsoleView, ConsoleViewContentType}
import com.intellij.openapi.progress.{ProgressIndicator, ProgressManager, Task}
import com.intellij.openapi.project.Project
import de.siegmar.fastcsv.writer.CsvWriter
import edg.ElemBuilder
import edg.compiler._
import edg.util.{StreamUtils, timeExec}
import edg.wir.Refinements
import edg_ide.dse.{DseConfigElement, DseDerivedConfig, DseRefinementElement, DseResult}
import edg_ide.ui.{BlockVisualizerService, EdgCompilerService}
import edg_ide.util.CrossProductUtils.crossProduct
import edgir.schema.schema

import java.io.{FileWriter, OutputStream, PrintWriter, StringWriter}
import java.nio.file.Paths
import scala.collection.{SeqMap, mutable}
import scala.jdk.CollectionConverters.IterableHasAsJava


/** Utility class that allows one running instance at any time, additional runIfIdle requests are discarded.
  * Useful for background-able tasks that can return an outdated result, eg UI updates.
  *
  * This in itself is not thread-safe!
  */
class SingleThreadRunner() {
  var thread: Option[Thread] = None

  def runIfIdle(runnable: => Unit): Unit = {
    if (thread.isEmpty || thread.get.getState == Thread.State.TERMINATED) {
      val newThread = new Thread() {
        override def run(): Unit = {
          runnable
        }
      }
      newThread.start()
      thread = Some(newThread)
    }
  }

  def join(): Unit = {
    thread.map { _.join() }
  }
}


class DseProcessHandler(project: Project, options: DseRunConfigurationOptions, val console: ConsoleView)
    extends ProcessHandler with HasConsoleStages {
  var runThread: Option[Thread] = None

  override def destroyProcessImpl(): Unit = {
    runThread.foreach(_.interrupt())
  }
  override def detachProcessImpl(): Unit = {
    throw new NotImplementedError()
  }
  override def detachIsDefault(): Boolean = false
  override def getProcessInput: OutputStream = null

  def terminatedNotify(exitCode: Int): Unit = {
    notifyProcessTerminated(exitCode)
  }

  ProgressManager.getInstance().run(new Task.Backgroundable(project, "EDG compiling") {
    override def run(indicator: ProgressIndicator): Unit = runCompile(indicator)
  })

  private def runCompile(indicator: ProgressIndicator): Unit = {
    runThread = Some(Thread.currentThread())
    startNotify()
    console.print(s"Starting compilation of ${options.designName}\n", ConsoleViewContentType.SYSTEM_OUTPUT)

    // the UI update is in a thread so it doesn't block the main search loop
    val uiUpdater = new SingleThreadRunner()
    uiUpdater.runIfIdle {
      BlockVisualizerService(project).setDseResults(Seq(), true)  // show searching in UI
    }

    // Open a CSV file (if desired) and write result rows as they are computed.
    // This is done first to empty out the result file, if one already exists.
    val objectiveNames = options.objectives.keys.toSeq
    val configNames = options.searchConfigs.map(_.configToString)
    val csvFile = if (options.resultCsvFile.nonEmpty) {
      val fileWriter = new FileWriter(options.resultCsvFile) // need a separate fileWriter to be flushable
      Option(CsvWriter.builder().build(fileWriter)) match {
        case Some(csv) =>
          csv.writeRow((Seq("index", "errors") ++ configNames ++ objectiveNames).asJava) // write header row
          fileWriter.flush()

          console.print(s"Opening results CSV at ${options.resultCsvFile}\n",
            ConsoleViewContentType.SYSTEM_OUTPUT)
          Some((fileWriter, csv))
        case None =>
          console.print(s"Failed to open results CSV at ${options.resultCsvFile}\n",
            ConsoleViewContentType.ERROR_OUTPUT)
          None
      }
    } else {
      None
    }

    // This structure is quite nasty, but is needed to give a stream handle in case something crashes,
    // in which case pythonInterface is not a valid reference
    var pythonInterface: Option[PythonInterface] = None
    var exitCode: Int = -1
    def forwardProcessOutput(): Unit = {
      pythonInterface.foreach { pyIf => StreamUtils.forAvailable(pyIf.processOutputStream) { data =>
        console.print(new String(data), ConsoleViewContentType.NORMAL_OUTPUT)
      }}
      pythonInterface.foreach { pyIf => StreamUtils.forAvailable(pyIf.processErrorStream) { data =>
        console.print(new String(data), ConsoleViewContentType.ERROR_OUTPUT)
      }}
    }

    val (staticConfigs, derivedConfigs) = options.searchConfigs.partitionMap {
      case config: DseRefinementElement[Any] => Left(config)
      case config: DseDerivedConfig => Right(config)
    }

    val staticSearchRefinements = crossProduct(staticConfigs.map { searchConfig =>  // generate values for each config
      searchConfig.getValues.map { case (value, refinement) =>
        (searchConfig -> value, refinement)  // tag config onto the value
      }
    }).map { valueRefinements =>  // combine values across the configs
      val (values, refinements) = valueRefinements.unzip
      (values.to(SeqMap), refinements.reduce(_ ++ _))
    }

    try {
      val (pythonCommand, sdkName) = CompileProcessHandler.getPythonInterpreter(project, options.designName).mapErr(
        msg => s"while getting Python interpreter path: $msg"
      ).get
      console.print(s"Using interpreter from configured SDK '$sdkName': $pythonCommand\n",
        ConsoleViewContentType.LOG_INFO_OUTPUT)
      pythonInterface = Some(new LoggingPythonInterface(
        Paths.get(project.getBasePath).resolve("HdlInterfaceService.py").toFile,
        pythonCommand,
        console))

      EdgCompilerService(project).pyLib.withPythonInterface(pythonInterface.get) {
        // compared to the single design compiler the debug info is a lot sparser here
        runFailableStage("discard stale", indicator) {
          val discarded = EdgCompilerService(project).discardStale()
          s"${discarded.size} library elements"
        }

        runFailableStage("rebuild libraries", indicator) {
          val designModule = options.designName.split('.').init.mkString(".")
          val (indexed, _, _) = EdgCompilerService(project).rebuildLibraries(designModule, None).get
          f"${indexed.size} elements"
        }

        val designType = ElemBuilder.LibraryPath(options.designName)
        val (block, refinementsPb) = EdgCompilerService(project).pyLib.getDesignTop(designType)
            .mapErr(msg => s"invalid top-level design: $msg").get // TODO propagate Errorable
        val design = schema.Design(contents = Some(block))
        val partialCompile = options.searchConfigs.map(_.getPartialCompile).reduce(_ ++ _)
        val (removedRefinements, refinements) = Refinements(refinementsPb).partitionBy(
          partialCompile.blocks.toSet, partialCompile.params.toSet
        )
        if (!removedRefinements.isEmpty) {
          console.print(s"Discarded conflicting refinements $removedRefinements\n", ConsoleViewContentType.SYSTEM_OUTPUT)
        }

        val commonCompiler = runRequiredStage("compile base design", indicator) {
          val commonCompiler = new Compiler(design, EdgCompilerService(project).pyLib,
            refinements = refinements, partial = partialCompile)
          commonCompiler.compile()
          (commonCompiler, "")
        }

        val results = mutable.ListBuffer[DseResult]()
        var resultIndex: Int = 0
        runFailableStage("design space search", indicator) {
          // Outer (static configuration) loop
          for (((staticSearchValues, staticSearchRefinement), staticIndex) <- staticSearchRefinements.zipWithIndex) {
            indicator.setIndeterminate(false)
            indicator.setFraction(staticIndex.toFloat / staticSearchRefinements.size)

            // for the derived case, compile with the static refinements, but hold back the derived configs
            val staticOuterCompiler = commonCompiler.fork(staticSearchRefinement,
              partial=derivedConfigs.map(_.getPartialCompile).reduce(_ ++ _))
            staticOuterCompiler.compile()
            // if there are derived configs, do a full compile to obtain the derived search space
            val ((generatingCompiler, generatingCompiled), compileTime) = timeExec {
              val generatingCompiler = staticOuterCompiler.fork()
              val generatingCompiled = generatingCompiler.compile()
              (generatingCompiler, generatingCompiled)
            }

            // Inner loop: derived configs and record results
            val derivedConfigVals = derivedConfigs.map { _.configFromDesign(generatingCompiler).get }  // TODO handle errors better
            val derivedSearchRefinements = crossProduct(derivedConfigVals.map { searchConfig => // TODO dedup w/ static case
              searchConfig.getValues.map { case (value, refinement) =>
                (searchConfig -> value, refinement) // tag config onto the value
              }
            }).map { valueRefinements => // combine values across the configs
              val (values, refinements) = valueRefinements.unzip
              (values.to(SeqMap), refinements.reduce(_ ++ _))
            }
            if (derivedConfigs.nonEmpty) {
              console.print(s"${derivedSearchRefinements.size} derived points\n", ConsoleViewContentType.SYSTEM_OUTPUT)
            }

            // TODO support non-derived case
            for (((derivedSearchValues, derivedSearchRefinement), derivedIndex) <- derivedSearchRefinements.zipWithIndex) {
              indicator.setFraction((staticIndex.toFloat + derivedIndex.toFloat / derivedSearchRefinements.size)
                  / staticSearchRefinements.size)
              val ((compiler, compiled, errors), compileTime) = timeExec {
                val compiler = staticOuterCompiler.fork(derivedSearchRefinement)
                val compiled = compiler.compile()
                val errors = compiler.getErrors() ++ new DesignAssertionCheck(compiler).map(compiled) ++
                    new DesignStructuralValidate().map(compiled) ++ new DesignRefsValidate().validate(compiled)
                (compiler, compiled, errors)
              }

              val solvedValues = compiler.getAllSolved
              val objectiveValues = options.objectives.map { case (name, objective) =>
                name -> objective.calculate(compiled, solvedValues)
              }

              val resultValues = (staticSearchValues ++ derivedSearchValues).map { case (config, value) => config.asInstanceOf[DseConfigElement] -> value }
              val result = DseResult(resultIndex, resultValues,
                staticSearchRefinement ++ derivedSearchRefinement, refinements ++ staticSearchRefinement ++ derivedSearchRefinement,
                compiler, compiled, errors, objectiveValues, compileTime)
              results.append(result)

              if (errors.nonEmpty) {
                console.print(s"Result $resultIndex, ${errors.size} errors ($compileTime ms): ${result.objectiveToString}\n",
                  ConsoleViewContentType.ERROR_OUTPUT)
              } else {
                console.print(s"Result $resultIndex ($compileTime ms): ${result.objectiveToString}\n",
                  ConsoleViewContentType.SYSTEM_OUTPUT)
              }

              // Write to CSV
              csvFile.foreach { case (fileWriter, csv) =>
                csv.writeRow((Seq(staticIndex.toString, errors.length.toString) ++
                    staticSearchValues.map { case (config, value) => DseConfigElement.valueToString(value) } ++
                    objectiveNames.map(name => DseConfigElement.valueToString(objectiveValues(name)))).asJava)
                fileWriter.flush()
              }

              // only have one UI update in progress at any time
              uiUpdater.runIfIdle {
                System.gc() // clean up after this compile run
                BlockVisualizerService(project).setDseResults(results.toSeq, true) // show searching in UI
              }

              resultIndex += 1
            }
          }
          s"${results.length} configurations"
        }

        uiUpdater.join()  // wait for pending UI updates to finish before updating to final value

        runFailableStage("update visualization", indicator) {
          BlockVisualizerService(project).setDseResults(results.toSeq, false)  // plumb results to UI
          ""
        }
      }
    } catch {
      case e: Throwable =>
        pythonInterface.foreach { pyIf => exitCode = pyIf.shutdown() }
        forwardProcessOutput()  // dump remaining process output first

        console.print(s"Compiler internal error: ${e.toString}\n", ConsoleViewContentType.ERROR_OUTPUT)
        val stackWriter = new StringWriter()
        e.printStackTrace(new PrintWriter(stackWriter))
        console.print(stackWriter.toString, ConsoleViewContentType.ERROR_OUTPUT)
    }

    csvFile.foreach { case (fileWriter, csv) =>
      runFailableStage("closing CSV", indicator) {
        csv.close()
        f"closed ${options.resultCsvFile}"
      }
    }

    terminatedNotify(exitCode)
  }
}
