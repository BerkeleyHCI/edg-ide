package edg_ide.runner

import com.intellij.execution.process.ProcessHandler
import com.intellij.execution.ui.{ConsoleView, ConsoleViewContentType}
import com.intellij.openapi.progress.{ProgressIndicator, ProgressManager, Task}
import com.intellij.openapi.project.Project
import de.siegmar.fastcsv.writer.CsvWriter
import edg.EdgirUtils.SimpleLibraryPath
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
            // TODO: if there aren't derived configs, skip this
            val generatingCompiler = staticOuterCompiler.fork()
            val (generatingCompiled, compileTime) = timeExec {
              generatingCompiler.compile()
            }

            // Inner loop: derived configs and record results
            val derivedConfigVals = derivedConfigs.map { _.configFromDesign(generatingCompiler) }
            val derivedSearchRefinements = crossProduct(staticConfigs.map { searchConfig => // TODO dedup w/ static case
              searchConfig.getValues.map { case (value, refinement) =>
                (searchConfig -> value, refinement) // tag config onto the value
              }
            }).map { valueRefinements => // combine values across the configs
              val (values, refinements) = valueRefinements.unzip
              (values.to(SeqMap), refinements.reduce(_ ++ _))
            }


            for (((derivedSearchValues, derivedSearchRefinement), derivedIndex) <- derivedSearchRefinements.zipWithIndex) {
              // TODO FIXME
              val compiled = generatingCompiled
              val compiler = generatingCompiler

              val errors = compiler.getErrors() ++ new DesignAssertionCheck(compiler).map(compiled) ++
                  new DesignStructuralValidate().map(compiled) ++ new DesignRefsValidate().validate(compiled)

              val solvedValues = compiler.getAllSolved
              val objectiveValues = options.objectives.map { case (name, objective) =>
                name -> objective.calculate(compiled, solvedValues)
              }

              val resultValues = staticSearchValues.map { case (config, value) => config.asInstanceOf[DseConfigElement] -> value }
              val result = DseResult(staticIndex, resultValues,
                staticSearchRefinement, refinements ++ staticSearchRefinement,
                compiler, compiled, errors, objectiveValues, compileTime)
              results.append(result)

              if (errors.nonEmpty) {
                console.print(s"${errors.size} errors, ${result.objectiveToString} ($compileTime ms)\n",
                  ConsoleViewContentType.ERROR_OUTPUT)
              } else {
                console.print(s"${result.objectiveToString} ($compileTime ms)\n",
                  ConsoleViewContentType.SYSTEM_OUTPUT)
              }

              // Write to CSV
              csvFile.foreach { case (fileWriter, csv) =>
                csv.writeRow((Seq(staticIndex.toString, errors.length.toString) ++
                    staticSearchValues.map { case (config, value) => DseConfigElement.valueToString(value) } ++
                    objectiveNames.map(name => DseConfigElement.valueToString(objectiveValues(name)))).asJava)
                fileWriter.flush()
              }
            }
          }
          s"${results.length} configurations"
        }

        runFailableStage("update visualization", indicator) {
          BlockVisualizerService(project).setDseResults(results.toSeq)  // plumb results to UI
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
