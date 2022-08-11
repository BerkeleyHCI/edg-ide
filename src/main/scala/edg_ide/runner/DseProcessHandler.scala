package edg_ide.runner

import com.intellij.execution.process.ProcessHandler
import com.intellij.execution.ui.{ConsoleView, ConsoleViewContentType}
import com.intellij.openapi.progress.{ProgressIndicator, ProgressManager, Task}
import com.intellij.openapi.project.Project
import edg.EdgirUtils.SimpleLibraryPath
import edg.ElemBuilder
import edg.compiler.{DesignStructuralValidate, ExprToString, ExprValue, PythonInterface}
import edg.util.{Errorable, StreamUtils}
import edg_ide.ui.{BlockVisualizerService, EdgCompilerService}
import edgir.elem.elem
import edgir.ref.ref
import edgir.schema.schema
import edgrpc.hdl.{hdl => edgrpc}

import java.io.{OutputStream, PrintWriter, StringWriter}
import java.nio.file.Paths


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
      pythonInterface = Some(new PythonInterface(
        Paths.get(project.getBasePath).resolve("HdlInterfaceService.py").toFile) {

        override def onLibraryRequest(element: ref.LibraryPath): Unit = {
          console.print(s"Compile ${element.toSimpleString}\n", ConsoleViewContentType.LOG_INFO_OUTPUT)
        }
        override def onLibraryRequestComplete(element: ref.LibraryPath,
                                              result: Errorable[(schema.Library.NS.Val, Option[edgrpc.Refinements])]): Unit = {
          forwardProcessOutput()
          result match {
            case Errorable.Error(msg) => console.print(msg, ConsoleViewContentType.ERROR_OUTPUT)
            case _ =>
          }
        }
        override def onElaborateGeneratorRequest(element: ref.LibraryPath, values: Map[ref.LocalPath, ExprValue]): Unit = {
          val valuesString = values.map { case (path, value) => s"${ExprToString(path)}: ${value.toStringValue}" }
              .mkString(", ")
          console.print(s"Generate ${element.toSimpleString} ($valuesString)\n",
            ConsoleViewContentType.LOG_INFO_OUTPUT)
        }
        override def onElaborateGeneratorRequestComplete(element: ref.LibraryPath,
                                                         values: Map[ref.LocalPath, ExprValue],
                                                         result: Errorable[elem.HierarchyBlock]): Unit = {
          forwardProcessOutput()
          result match {
            case Errorable.Error(msg) => console.print(msg, ConsoleViewContentType.ERROR_OUTPUT)
            case _ =>
          }
        }
      })

      val (compiled, compiler, refinements, reloadTime, compileTime) = EdgCompilerService(project).pyLib
          .withPythonInterface(pythonInterface.get) {
            indicator.setText("EDG compiling")

            val designType = ElemBuilder.LibraryPath(options.designName)
            val designModule = options.designName.split('.').init.mkString(".")
            EdgCompilerService(project).compile(designModule, designType, Some(indicator))
          }
      exitCode = pythonInterface.get.shutdown()
      forwardProcessOutput()  // dump remaining process output (shouldn't happen)

      indicator.setText("EDG compiling: validating")
      val checker = new DesignStructuralValidate()
      val errors = compiler.getErrors() ++ checker.map(compiled)
      console.print(s"Compiled (reload: $reloadTime ms, compile: $compileTime ms)\n",
        ConsoleViewContentType.SYSTEM_OUTPUT)
      if (errors.nonEmpty) {
        console.print(s"Compiled design has ${errors.length} errors\n", ConsoleViewContentType.ERROR_OUTPUT)
      }

      BlockVisualizerService(project).setDesignTop(compiled, compiler, refinements, errors)
      BlockVisualizerService(project).setLibrary(EdgCompilerService(project).pyLib)
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
