package edg_ide.runner

import com.intellij.execution.process.ProcessHandler
import com.intellij.execution.ui.{ConsoleView, ConsoleViewContentType}
import com.intellij.openapi.progress.{ProgressIndicator, ProgressManager, Task}
import com.intellij.openapi.project.Project
import edg.EdgirUtils.SimpleLibraryPath
import edg.ElemBuilder
import edg.compiler.{Compiler, DesignStructuralValidate, ExprToString, ExprValue, FloatValue, PythonInterface}
import edg.util.{Errorable, StreamUtils, timeExec}
import edg.wir.{DesignPath, IndirectDesignPath, Refinements}
import edg_ide.dse.{DseObjectiveParameter, DseParameterSearch}
import edg_ide.ui.{BlockVisualizerService, EdgCompilerService}
import edgir.elem.elem
import edgir.ref.ref
import edgir.schema.schema
import edgrpc.hdl.{hdl => edgrpc}

import java.io.{OutputStream, PrintWriter, StringWriter}
import java.nio.file.Paths


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
      DseParameterSearch(DesignPath() + "reg_5v",
        Seq(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5).map(FloatValue(_)))
    )
    val objectives = Seq(DseObjectiveParameter(IndirectDesignPath() + "reg_5v"))
    // TODO more generalized cartesian product
    val allRefinements = searchConfigs.flatMap { searchConfig =>
      searchConfig.getRefinements
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
        indicator.setText("EDG compiling: rebuilding libraries")
        indicator.setIndeterminate(true)
        EdgCompilerService(project).discardStale()
        val designModule = options.designName.split('.').init.mkString(".")
        val (indexed, indexTime, rebuildTime) = EdgCompilerService(project).rebuildLibraries(designModule, None)
            .mapErr {
          msg => s"while rebuilding libraries: $msg"
        }.get
        console.print(s"Rebuilt ${indexed.size} library elements\n",
          ConsoleViewContentType.SYSTEM_OUTPUT)

        val (block, refinementsPb) = EdgCompilerService(project).pyLib.getDesignTop(designType)
            .mapErr(msg => s"invalid top-level design: $msg").get // TODO propagate Errorable
        val design = schema.Design(contents = Some(block))
        val refinements = Refinements(refinementsPb)

        indicator.setText("EDG compiling: design top")
        val designType = ElemBuilder.LibraryPath(options.designName)

        for (searchRefinement <- allRefinements) {
          console.print(s"Compile $searchRefinement\n", ConsoleViewContentType.SYSTEM_OUTPUT)
          val ((compiler, compiled), compileTime) = timeExec {
            val compiler = new Compiler(design, EdgCompilerService(project).pyLib,
              refinements = refinements ++ searchRefinement)
            val compiled = compiler.compile()
            (compiler, compiled)
          }
          val solvedValues = compiler.getAllSolved
          val objectiveValues = objectives.map { objective =>
            objective.calculate(compiled, solvedValues)
          }
          console.print(s"($compileTime ms) $objectiveValues\n", ConsoleViewContentType.SYSTEM_OUTPUT)
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
