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
import edg_ide.dse.{DseConfigElement, DseResult}
import edg_ide.ui.{BlockVisualizerService, EdgCompilerService}
import edg_ide.util.CrossProductUtils.crossProduct
import edgir.schema.schema

import java.io.{FileWriter, OutputStream, PrintWriter, StringWriter}
import java.nio.file.Paths
import scala.collection.{SeqMap, mutable}
import scala.jdk.CollectionConverters.IterableHasAsJava


class DseProcessHandler(project: Project, options: DseRunConfigurationOptions, console: ConsoleView)
    extends ProcessHandler {
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

    val allValueRefinements = crossProduct(options.searchConfigs.map {
      searchConfig => searchConfig.getValues.map{ case (value, refinement) =>
        (searchConfig.asInstanceOf[DseConfigElement] -> value, refinement)  // tag config onto the value
      }
    }).map { valueRefinements =>
      val (values, refinements) = valueRefinements.unzip
      val combinedRefinement = refinements.reduce(_ ++ _)
      val valuesMap = SeqMap.from(values)
      (valuesMap, combinedRefinement)
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
        indicator.setText("EDG searching: rebuilding libraries")
        indicator.setIndeterminate(true)
        EdgCompilerService(project).discardStale()
        val designModule = options.designName.split('.').init.mkString(".")
        val (indexed, indexTime, rebuildTime) = EdgCompilerService(project).rebuildLibraries(designModule, None)
            .mapErr {
          msg => s"while rebuilding libraries: $msg"
        }.get
        console.print(s"Rebuilt ${indexed.size} library elements\n",
          ConsoleViewContentType.SYSTEM_OUTPUT)

        indicator.setText("EDG searching: compile base design")
        val designType = ElemBuilder.LibraryPath(options.designName)
        val (block, refinementsPb) = EdgCompilerService(project).pyLib.getDesignTop(designType)
            .mapErr(msg => s"invalid top-level design: $msg").get // TODO propagate Errorable
        val design = schema.Design(contents = Some(block))

        console.print(s"Compile base design\n", ConsoleViewContentType.SYSTEM_OUTPUT)
        val partialCompile = options.searchConfigs.map(_.getPartialCompile).reduce(_ ++ _)
        val (removedRefinements, refinements) = Refinements(refinementsPb).partitionBy(
          partialCompile.blocks.toSet, partialCompile.params.toSet
        )
        if (!removedRefinements.isEmpty) {
          console.print(s"Discarded conflicting refinements $removedRefinements\n", ConsoleViewContentType.SYSTEM_OUTPUT)
        }

        val (commonCompiler, commonCompileTime) = timeExec {
          val commonCompiler = new Compiler(design, EdgCompilerService(project).pyLib,
            refinements = refinements, partial = partialCompile)
          commonCompiler.compile()
          commonCompiler
        }
        console.print(s"($commonCompileTime ms) compiled base design\n", ConsoleViewContentType.SYSTEM_OUTPUT)

        val results = mutable.ListBuffer[DseResult]()
        for (((searchValues, searchRefinement), searchIndex) <- allValueRefinements.zipWithIndex) {
          console.print(s"Compile ${DseConfigElement.configMapToString(searchValues)}\n",
            ConsoleViewContentType.SYSTEM_OUTPUT)
          indicator.setText(f"EDG searching: design space ${searchIndex + 1} / ${allValueRefinements.size}")
          indicator.setIndeterminate(false)
          indicator.setFraction(searchIndex.toFloat / allValueRefinements.size)

          val (compiler, forkTime) = timeExec {
            commonCompiler.fork(searchRefinement)
          }
          val (compiled, compileTime) = timeExec {
            compiler.compile()
          }

          val errors = compiler.getErrors() ++ new DesignAssertionCheck(compiler).map(compiled) ++
            new DesignStructuralValidate().map(compiled) ++ new DesignRefsValidate().validate(compiled)

          val solvedValues = compiler.getAllSolved
          val objectiveValues = options.objectives.map { case (name, objective) =>
            name -> objective.calculate(compiled, solvedValues)
          }

          val result = DseResult(searchIndex, searchValues, searchRefinement, refinements ++ searchRefinement,
            compiler, compiled, errors, objectiveValues, compileTime)
          results.append(result)

          if (errors.nonEmpty) {
            console.print(s"($forkTime + $compileTime ms) ${errors.size} errors, ${result.objectiveToString}\n",
              ConsoleViewContentType.ERROR_OUTPUT)
          } else {
            console.print(s"($forkTime + $compileTime ms) ${result.objectiveToString}\n",
              ConsoleViewContentType.SYSTEM_OUTPUT)
          }

          // Write to CSV
          csvFile.foreach { case (fileWriter, csv) =>
            csv.writeRow((Seq(searchIndex.toString, errors.length.toString) ++
                searchValues.map{case (config, value) => DseConfigElement.valueToString(value)} ++
                objectiveNames.map(name => DseConfigElement.valueToString(objectiveValues(name)))).asJava)
            fileWriter.flush()
          }
        }
        indicator.setText(f"EDG searching: finalizing")
        indicator.setFraction(1)

        csvFile.foreach { case (fileWriter, csv) =>
          csv.close()
        }

        BlockVisualizerService(project).setDseResults(results.toSeq)  // plumb results to UI
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
    terminatedNotify(exitCode)
  }
}
