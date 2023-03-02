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
import edg_ide.dse.{DseConfigElement, DseObjective, DseResult, DseSearchGenerator}
import edg_ide.ui.{BlockVisualizerService, EdgCompilerService}
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
    thread.foreach { _.join() }
  }
}


/** Utility class for writing CSV output for design space exploration
  */
object DseCsvWriter {
  def apply(writer: java.io.Writer, elts: Seq[DseConfigElement],
            objectives: Seq[DseObjective]): Option[DseCsvWriter] = {
    Option(CsvWriter.builder().build(writer)).map { csv =>
      new DseCsvWriter(writer, csv, elts, objectives)
    }
  }
}


class DseCsvWriter(writer: java.io.Writer, csv: CsvWriter, searchConfigs: Seq[DseConfigElement],
                   objectives: Seq[DseObjective]) {
  private val objectiveNames = objectives.map(_.objectiveToString)
  private val searchNames = searchConfigs.map(_.configToString)

  csv.writeRow((Seq("index", "errors") ++ searchNames ++ objectiveNames).asJava) // write header row
  writer.flush()

  def writeRow(result: DseResult): Unit = {
    val headerCols = Seq(result.index.toString, result.errors.length.toString)
    val searchValueCols = searchConfigs.map { config =>
      result.config.get(config).map(DseConfigElement.valueToString).getOrElse("")
    }
    val objectiveValueCols = objectives.map { objective =>
      result.objectives.get(objective).map(DseConfigElement.valueToString).getOrElse("")
    }

    csv.writeRow((headerCols ++ searchValueCols ++ objectiveValueCols).asJava)
    writer.flush()
  }

  def close(): Unit = {
    csv.close()
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
    uiUpdater.runIfIdle {  // show searching in UI
      BlockVisualizerService(project).setDseResults(Seq(), options.searchConfigs, options.objectives, true)
    }

    // Open a CSV file (if desired) and write result rows as they are computed.
    // This is done first to empty out the result file, if one already exists.
    val csvFile = if (options.resultCsvFile.nonEmpty) {
      DseCsvWriter(new FileWriter(options.resultCsvFile), options.searchConfigs, options.objectives) match {
        case Some(csv) =>
          console.print(s"Opening results CSV at ${options.resultCsvFile}\n",
            ConsoleViewContentType.SYSTEM_OUTPUT)
          Some(csv)
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

        val designType = ElemBuilder.LibraryPath(options.designName)
        val (block, refinementsPb) = EdgCompilerService(project).pyLib.getDesignTop(designType)
            .mapErr(msg => s"invalid top-level design: $msg").get // TODO propagate Errorable
        val design = schema.Design(contents = Some(block))
        val partialCompile = options.searchConfigs.map(_.getPartialCompile).fold(PartialCompile())(_ ++ _)
        val (removedRefinements, refinements) = Refinements(refinementsPb).partitionBy(
          partialCompile.blocks.toSet, partialCompile.params.toSet, partialCompile.classParams.toSet
        )
        if (!removedRefinements.isEmpty) {
          console.print(s"Discarded conflicting refinements $removedRefinements\n", ConsoleViewContentType.SYSTEM_OUTPUT)
        }

        val results = mutable.ListBuffer[DseResult]()
        runFailableStage("search", indicator) {
          val searchGenerator = new DseSearchGenerator(options.searchConfigs)
          var nextPoint = searchGenerator.nextPoint()
          while (nextPoint.nonEmpty) {
            val (baseCompilerOpt, partialCompile, pointValues, searchRefinements, incrRefinements, completedFraction) = nextPoint.get
            indicator.setIndeterminate(false)
            indicator.setFraction(completedFraction)

            val compiler = baseCompilerOpt match {
              case Some(baseCompiler) => baseCompiler.fork(
                additionalRefinements = incrRefinements, partial = partialCompile)
              case None => new Compiler(design, EdgCompilerService(project).pyLib,
                refinements = refinements ++ incrRefinements, partial = partialCompile)
            }

            runFailableStage(s"point ${results.length}", indicator) {
              val (compiled, compileTime) = timeExec {
                compiler.compile()
              }

              if (partialCompile.isEmpty) { // only evaluate the point if it's a full point
                val errors = compiler.getErrors() ++ new DesignAssertionCheck(compiler).map(compiled) ++
                    new DesignStructuralValidate().map(compiled) ++ new DesignRefsValidate().validate(compiled)

                val objectiveValues = SeqMap.from(options.objectives.map { objective =>
                  objective -> objective.calculate(compiled, compiler)
                })

                val result = DseResult(results.length, pointValues,
                  searchRefinements, compiler, compiled, errors, objectiveValues, compileTime)

                results.append(result)
                csvFile.foreach { csvFile =>
                  csvFile.writeRow(result)
                }

                uiUpdater.runIfIdle { // only have one UI update in progress at any time
                  System.gc() // clean up after this compile run
                  BlockVisualizerService(project).setDseResults(results.toSeq, options.searchConfigs, options.objectives, true)
                }

                if (errors.nonEmpty) {
                  f"${errors.size} errors, ${result.objectiveToString}"
                } else {
                  result.objectiveToString
                }
              } else {
                "intermediate point"
              }
            }

            searchGenerator.addEvaluatedPoint(compiler)
            nextPoint = searchGenerator.nextPoint()
          }
          s"${results.length} configurations"
        }

        runFailableStage("update visualization", indicator) {
          uiUpdater.join()  // wait for pending UI updates to finish before updating to final value
          BlockVisualizerService(project).setDseResults(results.toSeq, options.searchConfigs, options.objectives, false)
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

    csvFile.foreach { case csvFile =>
      runFailableStage("closing CSV", indicator) {
        csvFile.close()
        f"closed ${options.resultCsvFile}"
      }
    }

    terminatedNotify(exitCode)
  }
}
