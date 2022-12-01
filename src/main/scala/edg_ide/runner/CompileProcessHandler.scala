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
import edg.compiler.{DesignAssertionCheck, DesignRefsValidate, DesignStructuralValidate, ElaborateRecord, ExprToString, ExprValue, PythonInterface}
import edg.util.{Errorable, StreamUtils, timeExec}
import edg.wir.DesignPath
import edg_ide.ui.{BlockVisualizerService, EdgCompilerService}
import edg_ide.util.ExceptionNotifyImplicits.ExceptNotify
import edg_ide.util.exceptable
import edgir.elem.elem
import edgir.ref.ref
import edgir.schema.schema
import edgrpc.hdl.{hdl => edgrpc}

import java.io.{File, FileWriter, OutputStream, PrintWriter, StringWriter}
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
    console.print(s"Compile ${element.toSimpleString}\n", ConsoleViewContentType.LOG_INFO_OUTPUT)
  }

  override def onLibraryRequestComplete(element: ref.LibraryPath,
                                        result: Errorable[(schema.Library.NS.Val, Option[edgrpc.Refinements])]): Unit = {
    forwardProcessOutput()
    result match {
      case Errorable.Error(msg) => console.print(msg + "Error compiling \n", ConsoleViewContentType.ERROR_OUTPUT)
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
      case Errorable.Error(msg) => console.print(msg + "\n", ConsoleViewContentType.ERROR_OUTPUT)
      case _ =>
    }
  }

  override def onRunBackend(backend: ref.LibraryPath): Unit = {
    console.print(s"Run backend ${backend.toSimpleString}\n", ConsoleViewContentType.LOG_INFO_OUTPUT)
  }

  override def onRunBackendComplete(backend: ref.LibraryPath,
                                    result: Errorable[Map[DesignPath, String]]): Unit = {
    forwardProcessOutput()
    result match {
      case Errorable.Error(msg) => console.print(msg + "\n", ConsoleViewContentType.ERROR_OUTPUT)
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

class CompileProcessHandler(project: Project, options: DesignTopRunConfigurationOptions, console: ConsoleView)
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


  def elaborateRecordToProgressString(record: ElaborateRecord): String = record match {
    case ElaborateRecord.ExpandBlock(blockPath) => s"block at $blockPath"
    case ElaborateRecord.Block(blockPath) => s"block at $blockPath"
    case ElaborateRecord.Link(linkPath) => s"link at $linkPath"
    case ElaborateRecord.LinkArray(linkPath) => s"link array at $linkPath"
    case ElaborateRecord.Connect(toLinkPortPath, fromLinkPortPath) => s"connect $toLinkPortPath - $fromLinkPortPath"
    case ElaborateRecord.ElaboratePortArray(portPath) => s"expand port array $portPath"
    case ElaborateRecord.AssignLinkElements(target, _, _) => s"link elements at $target"

    case ElaborateRecord.ExpandArrayConnections(parent, constrName) =>
      s"expand array connection $parent.$constrName"
    case ElaborateRecord.RewriteConnectAllocate(parent, portPath, _, _, _) =>
      s"rewrite connection allocates ${parent ++ portPath}"
    case ElaborateRecord.ResolveArrayIsConnected(parent, portPath, _, _, _) =>
      s"resolve array connectivity ${parent ++ portPath}"

    case record: ElaborateRecord.ElaborateDependency => s"unexpected dependency $record"
  }

  private def runCompile(indicator: ProgressIndicator): Unit = {
    runThread = Some(Thread.currentThread())
    startNotify()
    console.print(s"Starting compilation of ${options.designName}\n", ConsoleViewContentType.SYSTEM_OUTPUT)

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
        indicator.setText("EDG compiling: discarding stale")
        indicator.setIndeterminate(true)
        val discarded = EdgCompilerService(project).discardStale()
        if (discarded.nonEmpty) {
          val discardedNames = discarded.map {
            _.toSimpleString
          }.toSeq.sorted.mkString(", ")
          console.print(s"Discarded ${discarded.size} changed cached libraries: $discardedNames\n",
            ConsoleViewContentType.SYSTEM_OUTPUT)
        } else {
          console.print(s"No changed libraries detected, no libraries discarded\n",
            ConsoleViewContentType.SYSTEM_OUTPUT)
        }

        indicator.setText("EDG compiling: rebuilding libraries")
        indicator.setIndeterminate(false)
        val designModule = options.designName.split('.').init.mkString(".")
        def rebuildProgressFn(library: ref.LibraryPath, index: Int, total: Int): Unit = {
          indicator.setFraction(index.toFloat / total)
        }
        EdgCompilerService(project).rebuildLibraries(designModule, Some(rebuildProgressFn)) match {
          case Errorable.Success((indexed, indexTime, rebuildTime)) =>
            console.print(s"Rebuilt ${indexed.size} library elements " +
                s"(index: $indexTime ms, build: $rebuildTime ms)\n",
              ConsoleViewContentType.SYSTEM_OUTPUT)
          case Errorable.Error(errMsg) =>
            console.print(s"Failed to index: $errMsg\n", ConsoleViewContentType.ERROR_OUTPUT)
        }

        indicator.setText("EDG compiling: design top")
        indicator.setIndeterminate(true)
        val designType = ElemBuilder.LibraryPath(options.designName)
        def compileProgressFn(record: ElaborateRecord): Unit = {
          indicator.setText(s"EDG compiling: ${elaborateRecordToProgressString(record)}")
        }
        val ((compiled, compiler, refinements), compileTime) =
          EdgCompilerService(project).compile(designType, Some(compileProgressFn))
        console.print(s"Compiled ($compileTime ms)\n",
          ConsoleViewContentType.SYSTEM_OUTPUT)
        
        indicator.setText("EDG compiling: validating")
        val errors = compiler.getErrors() ++ new DesignAssertionCheck(compiler).map(compiled) ++
          new DesignStructuralValidate().map(compiled) ++ new DesignRefsValidate().validate(compiled)
        if (errors.nonEmpty) {
          console.print(s"Compiled design has ${errors.length} errors\n", ConsoleViewContentType.ERROR_OUTPUT)
        }

        indicator.setText("EDG compiling: updating visualization")
        BlockVisualizerService(project).setDesignTop(compiled, compiler, refinements, errors)
        BlockVisualizerService(project).setLibrary(EdgCompilerService(project).pyLib)

        if (options.netlistFile.nonEmpty) {
          indicator.setText("EDG compiling: netlisting")

          val arguments = Map("RefdesMode" -> options.toggle.toString)

          val (netlist, netlistTime) = timeExec {
            pythonInterface.get.runBackend(
              ElemBuilder.LibraryPath("electronics_model.NetlistBackend"),
              compiled, compiler.getAllSolved, arguments
            ).mapErr(msg => s"while netlisting: $msg").get
          }
          require(netlist.size == 1)

          val writer = new FileWriter(options.netlistFile)
          writer.write(netlist.head._2)
          writer.close()
          console.print(s"Wrote netlist to ${options.netlistFile} ($netlistTime ms)\n",
            ConsoleViewContentType.SYSTEM_OUTPUT)
        } else {
          console.print(s"Not generating netlist, no netlist file specified in run options\n",
            ConsoleViewContentType.ERROR_OUTPUT)
        }
      }
      exitCode = pythonInterface.get.shutdown()
      pythonInterface.get.forwardProcessOutput() // dump remaining process output (shouldn't happen)
    } catch {
      case e: Throwable =>
        pythonInterface.foreach { pyIf =>
          exitCode = pyIf.shutdown()
          pyIf.forwardProcessOutput()  // dump remaining process output before the final error message
        }

        val stackWriter = new StringWriter()
        e.printStackTrace(new PrintWriter(stackWriter))
        console.print(s"Compiler internal error\n", ConsoleViewContentType.ERROR_OUTPUT)
        console.print(stackWriter.toString, ConsoleViewContentType.ERROR_OUTPUT)
    }
    terminatedNotify(exitCode)
  }
}
