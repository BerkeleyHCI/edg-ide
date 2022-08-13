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
import edg.compiler.{DesignStructuralValidate, ExprToString, ExprValue, PythonInterface}
import edg.util.{Errorable, StreamUtils, timeExec}
import edg.wir.DesignPath
import edg_ide.ui.{BlockVisualizerService, EdgCompilerService}
import edg_ide.util.ExceptionNotifyImplicits.ExceptNotify
import edg_ide.util.exceptable
import edgir.elem.elem
import edgir.ref.ref
import edgir.schema.schema
import edgrpc.hdl.{hdl => edgrpc}

import java.io.{FileWriter, OutputStream, PrintWriter, StringWriter}
import java.nio.file.Paths
import scala.jdk.CollectionConverters.MapHasAsJava


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


  // a dummy-ish provider for PythonRunParams to get the Python interpreter
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
      val discarded = EdgCompilerService(project).discardStale()
      if (discarded.nonEmpty) {
        val discardedNames = discarded.map { _.toSimpleString }.toSeq.sorted.mkString(", ")
        console.print(s"Discarded ${discarded.size} changed cached libraries: $discardedNames\n",
          ConsoleViewContentType.SYSTEM_OUTPUT)
      } else {
        console.print(s"No changed libraries detected, no libraries discarded\n",
          ConsoleViewContentType.SYSTEM_OUTPUT)
      }

      val (sdkName, pythonCommand) = ReadAction.compute(() => exceptable {
        val pyPsi = PyPsiFacade.getInstance(project)
        val anchor = PsiManager.getInstance(project).findFile(project.getProjectFile)
        val pyClass = pyPsi.createClassByQName(options.designName, anchor)
            .exceptNull(s"can't find class ${options.designName}")
        val module = ModuleUtilCore.findModuleForPsiElement(pyClass).exceptNull("can't find project module")
        val sdk = PythonSdkUtil.findPythonSdk(module).exceptNull("can't find Python SDK")

        val runParams = new DesignTopRunParams(
          pyClass.getContainingFile.getVirtualFile.getPath, sdk.getHomePath, module.getName)
        val pythonCommand = PythonCommandLineState.getInterpreterPath(project, runParams)
            .exceptNull("can't get interpreter path")
        (sdk.getName, pythonCommand)
      }).mapErr(msg => s"while trying to get Python interpreter path").get
      console.print(s"Using interpreter from configured SDK '$sdkName': $pythonCommand\n",
        ConsoleViewContentType.LOG_INFO_OUTPUT)

      pythonInterface = Some(new PythonInterface(
        Paths.get(project.getBasePath).resolve("HdlInterfaceService.py").toFile,
        pythonCommand) {

        override def onLibraryRequest(element: ref.LibraryPath): Unit = {
          console.print(s"Compile ${element.toSimpleString}\n", ConsoleViewContentType.LOG_INFO_OUTPUT)
        }

        override def onLibraryRequestComplete(element: ref.LibraryPath,
                                              result: Errorable[(schema.Library.NS.Val, Option[edgrpc.Refinements])]): Unit = {
          forwardProcessOutput()
          result match {
            case Errorable.Error(msg) => console.print(msg + "\n", ConsoleViewContentType.ERROR_OUTPUT)
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

        override def onRunBackend(backend: String): Unit = {
          console.print(s"Run backend $backend\n", ConsoleViewContentType.LOG_INFO_OUTPUT)
        }

        override def onRunBackendComplete(backend: String,
                                 result: Errorable[Map[DesignPath, String]]): Unit = {
          forwardProcessOutput()
          result match {
            case Errorable.Error(msg) => console.print(msg + "\n", ConsoleViewContentType.ERROR_OUTPUT)
            case _ =>
          }
        }
      })

      EdgCompilerService(project).pyLib
          .withPythonInterface(pythonInterface.get) {
            indicator.setText("EDG compiling: compiling")

            val designType = ElemBuilder.LibraryPath(options.designName)
            val designModule = options.designName.split('.').init.mkString(".")
            val (compiled, compiler, refinements, reloadTime, compileTime) =
              EdgCompilerService(project).compile(designModule, designType, Some(indicator))
            console.print(s"Compiled (reload: $reloadTime ms, compile: $compileTime ms)\n",
              ConsoleViewContentType.SYSTEM_OUTPUT)

            if (options.netlistFile.nonEmpty) {
              indicator.setText("EDG compiling: netlisting")
              val (netlist, netlistTime) = timeExec {
                 pythonInterface.get.runBackend("electronics_model.NetlistBackend",
                   compiled, compiler.getAllSolved).mapErr(msg => s"while netlisting: $msg").get
              }
              require(netlist.size == 1)

              val writer = new FileWriter(options.netlistFile)
              writer.write(netlist.head._2)
              writer.close()
              console.print(s"Wrote netlist to ${options.netlistFile} (netlist: $netlistTime ms)\n",
                ConsoleViewContentType.SYSTEM_OUTPUT)
            } else {
              console.print(s"Not generating netlist, no netlist file specified in run options\n",
                ConsoleViewContentType.NORMAL_OUTPUT)
            }

            indicator.setText("EDG compiling: validating")
            val checker = new DesignStructuralValidate()
            val errors = compiler.getErrors() ++ checker.map(compiled)
            if (errors.nonEmpty) {
              console.print(s"Compiled design has ${errors.length} errors\n", ConsoleViewContentType.ERROR_OUTPUT)
            }

            BlockVisualizerService(project).setDesignTop(compiled, compiler, refinements, errors)
            BlockVisualizerService(project).setLibrary(EdgCompilerService(project).pyLib)
          }
      exitCode = pythonInterface.get.shutdown()
      forwardProcessOutput() // dump remaining process output (shouldn't happen)
    } catch {
      case e: Throwable =>
        pythonInterface.foreach { pyIf => exitCode = pyIf.shutdown() }
        forwardProcessOutput()  // dump remaining process output first

        val stackWriter = new StringWriter()
        e.printStackTrace(new PrintWriter(stackWriter))
        console.print(s"Compiler internal error\n", ConsoleViewContentType.ERROR_OUTPUT)
        console.print(stackWriter.toString, ConsoleViewContentType.ERROR_OUTPUT)
    }
    terminatedNotify(exitCode)
  }
}