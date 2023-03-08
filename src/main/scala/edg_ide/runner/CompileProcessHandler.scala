package edg_ide.runner

import com.intellij.execution.process.ProcessHandler
import com.intellij.execution.ui.{ConsoleView, ConsoleViewContentType}
import com.intellij.openapi.application.ReadAction
import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.openapi.progress.{ProgressIndicator, ProgressManager, Task}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiManager
import com.intellij.util.PathMappingSettings
import com.jetbrains.python.psi.PyPsiFacade
import com.jetbrains.python.run.{PythonCommandLineState, PythonRunParams}
import com.jetbrains.python.sdk.PythonSdkUtil
import edg.EdgirUtils.SimpleLibraryPath
import edg.ElemBuilder
import edg.compiler._
import edg.util.{Errorable, StreamUtils, timeExec}
import edg.wir.DesignPath
import edg_ide.edgir_graph.{ElkEdgirGraphUtils, HierarchyGraphElk}
import edg_ide.ui.{BlockVisualizerService, EdgCompilerService}
import edg_ide.util.ExceptionNotifyImplicits.ExceptNotify
import edg_ide.util.exceptable
import edgir.elem.elem
import edgir.ref.ref
import edgir.schema.schema
import edgrpc.hdl.{hdl => edgrpc}

import java.io._
import java.nio.file.Paths
import scala.jdk.CollectionConverters.MapHasAsJava


// a dummy-ish provider for PythonRunParams to get the Python interpreter executable
class DesignTopRunParams(workingDirectory: String, sdkHome: String, moduleName: String) extends PythonRunParams {
  override def getInterpreterOptions: String = ""

  override def setInterpreterOptions(s: String): Unit = throw new NotImplementedError()

  override def getWorkingDirectory: String = workingDirectory

  override def setWorkingDirectory(s: String): Unit = throw new NotImplementedError()

  override def getSdkHome: String = sdkHome

  override def setSdkHome(s: String): Unit = throw new NotImplementedError()

  override def setModule(newModule: Module): Unit = throw new NotImplementedError()

  override def getModuleName: String = moduleName

  override def isUseModuleSdk: Boolean = true

  override def setUseModuleSdk(b: Boolean): Unit = throw new NotImplementedError()

  override def isPassParentEnvs: Boolean = false

  override def setPassParentEnvs(b: Boolean): Unit = throw new NotImplementedError()

  override def getEnvs: java.util.Map[String, String] = Map[String, String]().asJava

  override def setEnvs(map: java.util.Map[String, String]): Unit = throw new NotImplementedError()

  override def getMappingSettings: PathMappingSettings = new PathMappingSettings()

  override def setMappingSettings(pathMappingSettings: PathMappingSettings): Unit = throw new NotImplementedError()

  override def shouldAddContentRoots(): Boolean = false

  override def shouldAddSourceRoots(): Boolean = false

  override def setAddContentRoots(b: Boolean): Unit = throw new NotImplementedError()

  override def setAddSourceRoots(b: Boolean): Unit = throw new NotImplementedError()
}


// a PythonInterface that uses the on-event hooks to log to the console
class LoggingPythonInterface(serverFile: File, pythonInterpreter: String, console: ConsoleView)
    extends PythonInterface(serverFile, pythonInterpreter) {
  def forwardProcessOutput(): Unit = {
    StreamUtils.forAvailable(processOutputStream) { data =>
      console.print(new String(data), ConsoleViewContentType.NORMAL_OUTPUT)
    }
    StreamUtils.forAvailable(processErrorStream) { data =>
      console.print(new String(data), ConsoleViewContentType.ERROR_OUTPUT)
    }
  }

  override def onLibraryRequest(element: ref.LibraryPath): Unit = {
    // this needs to be here to only print on requests that made it to Python (instead of just hit cache)
    console.print(s"Compile ${element.toSimpleString}\n", ConsoleViewContentType.LOG_DEBUG_OUTPUT)
  }

  override def onLibraryRequestComplete(element: ref.LibraryPath,
                                        result: Errorable[(schema.Library.NS.Val, Option[edgrpc.Refinements])]): Unit = {
    forwardProcessOutput()
    result match {
      case Errorable.Error(msg) => console.print(f"Error while compiling ${element.toSimpleString}: $msg\n",
        ConsoleViewContentType.ERROR_OUTPUT)
      case _ =>
    }
  }

  override def onElaborateGeneratorRequest(element: ref.LibraryPath, values: Map[ref.LocalPath, ExprValue]): Unit = {
    val valuesString = values.map { case (path, value) => s"${ExprToString(path)}: ${value.toStringValue}" }
        .mkString(", ")
    console.print(s"Generate ${element.toSimpleString} ($valuesString)\n",
      ConsoleViewContentType.LOG_DEBUG_OUTPUT)
  }

  override def onElaborateGeneratorRequestComplete(element: ref.LibraryPath,
                                                   values: Map[ref.LocalPath, ExprValue],
                                                   result: Errorable[elem.HierarchyBlock]): Unit = {
    forwardProcessOutput()
    result match {
      case Errorable.Error(msg) => console.print(f"Error while generating ${element.toSimpleString}: $msg\n",
        ConsoleViewContentType.ERROR_OUTPUT)
      case _ =>
    }
  }

  override def onRunRefinementPassComplete(refinementPass: ref.LibraryPath,
                                           result: Errorable[Map[DesignPath, ExprValue]]): Unit = {
    forwardProcessOutput()
    result match {
      case Errorable.Error(msg) => console.print(f"Error while running refinement ${refinementPass.toSimpleString}: $msg\n",
        ConsoleViewContentType.ERROR_OUTPUT)
      case _ =>
    }
  }

  override def onRunBackendComplete(backend: ref.LibraryPath,
                                    result: Errorable[Map[DesignPath, String]]): Unit = {
    forwardProcessOutput()
    result match {
      case Errorable.Error(msg) => console.print(f"Error while running backend ${backend.toSimpleString}: $msg\n",
        ConsoleViewContentType.ERROR_OUTPUT)
      case _ =>
    }
  }
}

object CompileProcessHandler {
  // Returns the Python SDK and interpreter executable from the SDK for the Python class associated with the design
  def getPythonInterpreter(project: Project, designName: String):
      Errorable[(String, String)] = exceptable {
    ReadAction.compute(() => {
      val pyPsi = PyPsiFacade.getInstance(project)
      val anchor = PsiManager.getInstance(project).findFile(project.getProjectFile)
      val pyClass = pyPsi.createClassByQName(designName, anchor)
          .exceptNull(s"can't find class $designName")
      val module = ModuleUtilCore.findModuleForPsiElement(pyClass).exceptNull("can't find project module")
      val sdk = PythonSdkUtil.findPythonSdk(module).exceptNull("can't find Python SDK")

      val runParams = new DesignTopRunParams(
        pyClass.getContainingFile.getVirtualFile.getPath, sdk.getHomePath, module.getName)
      val pythonCommand = PythonCommandLineState.getInterpreterPath(project, runParams)
          .exceptNull("can't get interpreter path")
      (pythonCommand, sdk.getName)
    })
  }
}

trait HasConsoleStages {
  val console: ConsoleView

  /** Logging and status wrappers for running a stage that returns some value.
    * Exceptions are not caught and propagated up.
    * The stage function returns both a type and a message (can be empty) that is printed to the console.
    */
  protected def runRequiredStage[ReturnType](name: String, indicator: ProgressIndicator)
                                          (fn: => (ReturnType, String)): ReturnType = {
    if (Thread.interrupted()) throw new InterruptedException  // TODO cleaner way to stop compile process?
    indicator.setText(f"EDG compiling: $name")
    indicator.setIndeterminate(true)
    val ((fnResult, fnResultStr), fnTime) = timeExec {
      fn
    }
    val addedStr = if (fnResultStr.nonEmpty) f": $fnResultStr" else ""
    console.print(s"Completed: $name$addedStr ($fnTime ms)\n", ConsoleViewContentType.LOG_INFO_OUTPUT)
    fnResult
  }

  /** Similar to (actually wraps) runRequiredStage, except errors are non-fatal and logs to console.
    * If the function fails, a specified default is returned.
    */
  protected def runFailableStage[ReturnType](name: String, default: ReturnType, indicator: ProgressIndicator)
                                          (fn: => (ReturnType, String)): ReturnType = {
    try {
      runRequiredStage(name, indicator) {
        fn
      }
    } catch {
      case e: Exception if !e.isInstanceOf[InterruptedException] =>
        console.print(s"Failed: $name: $e\n", ConsoleViewContentType.ERROR_OUTPUT)
        // By default, the stack trace isn't printed, since most of the internal details
        // (stack trace elements) aren't relevant for end users
        // TODO this should be plumbed to a toggle
        // val sw = new StringWriter
        // val pw = new PrintWriter(sw)
        // e.printStackTrace(pw)
        // console.print(sw.toString, ConsoleViewContentType.ERROR_OUTPUT)
        default
    }
  }

  /** Wrapper around runFailableStage for fns that don't return anything.
    */
  protected def runFailableStage(name: String, indicator: ProgressIndicator)
                              (fn: => String): Unit = {
    runFailableStage[Unit](name, (), indicator) {
      ((), fn)
    }
  }
}

class CompileProcessHandler(project: Project, options: DesignTopRunConfigurationOptions, val console: ConsoleView)
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
    console.print(s"Starting compilation of ${options.designName}\n", ConsoleViewContentType.LOG_INFO_OUTPUT)

    // This structure is quite nasty, but is needed to give a stream handle in case something crashes,
    // in which case pythonInterface is not a valid reference
    var pythonInterface: Option[LoggingPythonInterface] = None
    var exitCode: Int = -1

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
        BlockVisualizerService(project).setDesignStale()

        runFailableStage("discard stale", indicator) {
          val discarded = EdgCompilerService(project).discardStale()
          if (discarded.nonEmpty) {
            val discardedNames = discarded.toSeq.map { _.toSimpleString }.sorted
            s"${discarded.size} library elements: ${discardedNames.mkString(", ")}"
          } else {
            "no changed libraries"
          }
        }

        // this is mainly here to provide an index of library elements for the part browser
        // this can be skipped - library elements can be built dynamically as they are used during compile
        runFailableStage("rebuild libraries", indicator) {
          def rebuildProgressFn(library: ref.LibraryPath, index: Int, total: Int): Unit = {
            // this also includes requests that hit cache
            indicator.setFraction(index.toFloat / total)
          }

          indicator.setIndeterminate(false)
          val designModule = options.designName.split('.').init.mkString(".")
          val (indexed, _, _) = EdgCompilerService(project).rebuildLibraries(designModule, Some(rebuildProgressFn)).get
          f"${indexed.size} elements"
        }

        val (compiled, compiler, refinements) = runRequiredStage("compile", indicator) {
          val designType = ElemBuilder.LibraryPath(options.designName)
          val output = EdgCompilerService(project).compile(designType)
          (output, "")
        }

        val errors = runFailableStage("validate", Seq[CompilerError](), indicator) {
          val errors = compiler.getErrors() ++ new DesignAssertionCheck(compiler).map(compiled) ++
              new DesignStructuralValidate().map(compiled) ++ new DesignRefsValidate().validate(compiled)
          if (errors.nonEmpty) {
            console.print(s"Compiled design has ${errors.length} errors\n", ConsoleViewContentType.ERROR_OUTPUT)
          }
          (errors, f"${errors.length} errors")
        }

        runFailableStage("refdes", indicator) {
          val refdes = pythonInterface.get.runRefinementPass(
            ElemBuilder.LibraryPath("electronics_model.RefdesRefinementPass"),
            compiled, compiler.getAllSolved
          ).mapErr(msg => s"while refdesing: $msg").get
          compiler.addAssignValues(refdes, "refdes")
          f"${refdes.size} components"
        }

        runFailableStage("update visualization", indicator) {
          BlockVisualizerService(project).setDesignTop(compiled, compiler, refinements, errors)
          BlockVisualizerService(project).setLibrary(EdgCompilerService(project).pyLib)
          ""
        }

        if (options.pdfFile.nonEmpty) {
          runFailableStage("generate PDF", indicator) {
            PDFGeneratorUtil.generate(compiled.getContents, mappers=Seq(new ElkEdgirGraphUtils.TitleMapper(compiler)), options.pdfFile)
            f"wrote ${options.pdfFile}"
          }
        } else {
          console.print(s"Skip generating PDF, no PDF file specified in run options\n",
            ConsoleViewContentType.ERROR_OUTPUT)
        }

        if (options.netlistFile.nonEmpty) {
          runFailableStage("generate netlist", indicator) {
            val netlist = pythonInterface.get.runBackend(
              ElemBuilder.LibraryPath("electronics_model.NetlistBackend"),
              compiled, compiler.getAllSolved,
              Map("RefdesMode" -> options.toggle.toString)
            ).mapErr(msg => s"while netlisting: $msg").get
            require(netlist.size == 1)

            val writer = new FileWriter(options.netlistFile)
            writer.write(netlist.head._2)
            writer.close()
            f"wrote ${options.netlistFile}"
          }
        } else {
          console.print(s"Skip generating netlist, no netlist file specified in run options\n",
            ConsoleViewContentType.ERROR_OUTPUT)
        }

        if (options.bomFile.nonEmpty) {
          runFailableStage("generate BOM", indicator) {
            val bom = pythonInterface.get.runBackend(
              ElemBuilder.LibraryPath("electronics_model.BomBackend.GenerateBom"),
              compiled, compiler.getAllSolved,
              Map()
            ).mapErr(msg => s"while generating bom: $msg").get
            require(bom.size == 1)

            val writer = new FileWriter(options.bomFile)
            writer.write(bom.head._2)
            writer.close()
            f"wrote ${options.bomFile}"
          }
        } else {
          console.print(s"Skip generating BOM, no BOM file specified in run options\n",
            ConsoleViewContentType.ERROR_OUTPUT)
        }
      }
      exitCode = pythonInterface.get.shutdown()
      pythonInterface.get.forwardProcessOutput() // dump remaining process output (shouldn't happen)
    } catch {  // this generally shouldn't happen but is an overall catch-all and clean-up
      case e: Throwable =>
        pythonInterface.foreach { pyIf =>
          exitCode = pyIf.shutdown()
          pyIf.forwardProcessOutput()  // dump remaining process output before the final error message
        }

        val stackWriter = new StringWriter()
        e.printStackTrace(new PrintWriter(stackWriter))
        console.print(s"Compiler internal error, compilation stopped:\n", ConsoleViewContentType.ERROR_OUTPUT)
        console.print(stackWriter.toString, ConsoleViewContentType.ERROR_OUTPUT)
    }
    terminatedNotify(exitCode)
  }
}
