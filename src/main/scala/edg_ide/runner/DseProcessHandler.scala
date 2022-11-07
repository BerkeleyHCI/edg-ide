package edg_ide.runner

import collection.{SeqMap, mutable}
import com.intellij.execution.process.ProcessHandler
import com.intellij.execution.ui.{ConsoleView, ConsoleViewContentType}
import com.intellij.openapi.progress.{ProgressIndicator, ProgressManager, Task}
import com.intellij.openapi.project.Project
import de.siegmar.fastcsv.writer.CsvWriter
import edg.ElemBuilder
import edg.compiler.{Compiler, DesignAssertionCheck, DesignRefsValidate, DesignStructuralValidate, PythonInterface, RangeValue}
import edg.util.{StreamUtils, timeExec}
import edg.wir.{DesignPath, Refinements}
import edg_ide.dse._
import edg_ide.ui.EdgCompilerService
import edg_ide.util.CrossProductUtils.crossProduct
import edgir.schema.schema

import java.io.{OutputStream, PrintWriter, StringWriter}
import java.nio.file.Paths
import scala.jdk.CollectionConverters.IterableHasAsJava


class DseProcessHandler(project: Project, options: DseRunConfigurationOptions, console: ConsoleView)
    extends ProcessHandler {
  // TODO a lot can be deduplicated from CompileProcessHandler, the non DSE version of this?
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

    val searchConfigs = Seq(
      DseSubclassSearch(DesignPath() + "reg_5v",
        Seq(
          "electronics_lib.BuckConverter_TexasInstruments.Tps561201",
          "electronics_lib.BuckConverter_TexasInstruments.Tps54202h",
        ).map(value => ElemBuilder.LibraryPath(value))
      ),
      DseParameterSearch(DesignPath() + "reg_5v" + "ripple_current_factor",
        Seq(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5).map(value => RangeValue(value - 0.05, value + 0.05))
      ),
    )
    val objectives = SeqMap(
      "inductor" -> DseObjectiveParameter(DesignPath() + "reg_5v" + "power_path" + "inductor" + "actual_part"),
      "inductor_val" -> DseObjectiveParameter(DesignPath() + "reg_5v" + "power_path" + "inductor" + "fp_value"),
      "inductance" -> DseObjectiveParameter(DesignPath() + "reg_5v" + "power_path" + "inductor" + "actual_inductance"),
      "5v_area" -> DseObjectiveFootprintArea(DesignPath() + "reg_5v"),
      "5v_count" -> DseObjectiveFootprintCount(DesignPath() + "reg_5v"),
    )

    val allRefinements = crossProduct(searchConfigs.map {
      searchConfig => searchConfig.getRefinements
    }).map { refinements =>
      refinements.reduce(_ ++ _)
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
        val refinements = Refinements(refinementsPb)

        val partialCompile = searchConfigs.map(_.getPartialCompile).reduce(_ ++ _)
        console.print(s"Compile base design\n", ConsoleViewContentType.SYSTEM_OUTPUT)
        val (commonCompiler, commonCompileTime) = timeExec {
          val commonCompiler = new Compiler(design, EdgCompilerService(project).pyLib,
            refinements = refinements, partial = partialCompile)
          commonCompiler.compile()
          commonCompiler
        }
        console.print(s"($commonCompileTime ms) compiled base design\n", ConsoleViewContentType.SYSTEM_OUTPUT)

        // Refinement input -> (error count, objectives)
        val results = mutable.SeqMap[Refinements, (Int, Map[String, Any])]()

        indicator.setText("EDG searching: design space")
        indicator.setFraction(0)
        indicator.setIndeterminate(false)
        for ((searchRefinement, searchIndex) <- allRefinements.zipWithIndex) {
          console.print(s"Compile $searchRefinement\n", ConsoleViewContentType.SYSTEM_OUTPUT)
          val ((compiler, compiled), compileTime) = timeExec {
            val compiler = commonCompiler.fork(searchRefinement)
            val compiled = compiler.compile()
            (compiler, compiled)
          }

          val errors = compiler.getErrors() ++ new DesignAssertionCheck(compiler).map(compiled) ++
            new DesignStructuralValidate().map(compiled) ++ new DesignRefsValidate().validate(compiled)

          val solvedValues = compiler.getAllSolved
          val objectiveValues = objectives.map { case (name, objective) =>
            name -> objective.calculate(compiled, solvedValues)
          }
          results.put(searchRefinement, (errors.size, objectiveValues))

          if (errors.nonEmpty) {
            console.print(s"($compileTime ms) ${errors.size} errors, $objectiveValues\n",
              ConsoleViewContentType.ERROR_OUTPUT)
          } else {
            console.print(s"($compileTime ms) $objectiveValues\n", ConsoleViewContentType.SYSTEM_OUTPUT)
          }

          indicator.setFraction((searchIndex + 1.0) / allRefinements.size)
        }

        if (options.resultCsvFile.nonEmpty) {
          indicator.setText("EDG searching: writing results")
          Option(CsvWriter.builder().build(Paths.get(options.resultCsvFile))) match {
            case Some(csv) =>
              val objectiveNames = objectives.keys.toSeq
              csv.writeRow((Seq("config", "errors") ++ objectiveNames).asJava)  // write header row

              results.foreach { case (refinement, (error, objectiveValues)) =>
                csv.writeRow((Seq(refinement.toString, error.toString) ++
                  objectiveNames.map(name => objectiveValues(name).toString)).asJava)
              }
              csv.close()

              console.print(s"Wrote results to ${options.resultCsvFile}\n",
                ConsoleViewContentType.SYSTEM_OUTPUT)
            case None =>
              console.print(s"Failed to write ${options.resultCsvFile}\n",
                ConsoleViewContentType.ERROR_OUTPUT)
          }
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
    terminatedNotify(exitCode)
  }
}
