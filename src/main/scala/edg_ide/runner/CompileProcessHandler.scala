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
import edg_ide.edgir_graph.ElkEdgirGraphUtils
import edg_ide.ui.{BlockVisualizerService, EdgCompilerService, EdgSettingsState}
import edg_ide.util.ExceptionNotifyImplicits.ExceptNotify
import edg_ide.util.exceptable
import edgir.elem.elem
import edgir.ref.ref
import edgir.schema.schema
import edgrpc.hdl.{hdl => edgrpc}

import java.io._
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.{CollectionHasAsScala, MapHasAsJava}

// a dummy-ish provider for PythonRunParams to get the Python interpreter executable
class DesignTopRunParams(workingDirectory: String, sdkHome: String, moduleName: String)
    extends PythonRunParams {
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

  override def setMappingSettings(pathMappingSettings: PathMappingSettings): Unit =
    throw new NotImplementedError()

  override def shouldAddContentRoots(): Boolean = false

  override def shouldAddSourceRoots(): Boolean = false

  override def setAddContentRoots(b: Boolean): Unit = throw new NotImplementedError()

  override def setAddSourceRoots(b: Boolean): Unit = throw new NotImplementedError()
}

// a PythonInterface that uses the on-event hooks to log to the console
class LoggingPythonInterface(interpreter: String, cwd: Option[File], pythonPaths: Seq[String], console: ConsoleView)
    extends ProtobufStdioSubprocess(interpreter = interpreter, cwd = cwd, pythonPaths = pythonPaths) {
  def forwardProcessOutput(): Unit = {
    StreamUtils.forAvailable(outputStream) { data =>
      console.print(new String(data), ConsoleViewContentType.NORMAL_OUTPUT)
    }
    StreamUtils.forAvailable(errorStream) { data =>
      console.print(new String(data), ConsoleViewContentType.ERROR_OUTPUT)
    }
  }
}

class LoggingCompilerInterface(interface: LoggingPythonInterface, console: ConsoleView)
    extends PythonInterface(interface) {
  override def onLibraryRequest(element: ref.LibraryPath): Unit = {
    // this needs to be here to only print on requests that made it to Python (instead of just hit cache)
    console.print(s"Compile ${element.toSimpleString}\n", ConsoleViewContentType.LOG_DEBUG_OUTPUT)
  }

  override def onLibraryRequestComplete(
      element: ref.LibraryPath,
      result: Errorable[(schema.Library.NS.Val, Option[edgrpc.Refinements])]
  ): Unit = {
    interface.forwardProcessOutput()
    result match {
      case Errorable.Error(msg) =>
        console.print(
          f"Error while compiling ${element.toSimpleString}: $msg\n",
          ConsoleViewContentType.ERROR_OUTPUT
        )
      case _ =>
    }
  }

  override def onElaborateGeneratorRequest(
      element: ref.LibraryPath,
      values: Map[ref.LocalPath, ExprValue]
  ): Unit = {
    val valuesString = values
      .map { case (path, value) => s"${ExprToString(path)}: ${value.toStringValue}" }
      .mkString(", ")
    console.print(
      s"Generate ${element.toSimpleString} ($valuesString)\n",
      ConsoleViewContentType.LOG_DEBUG_OUTPUT
    )
  }

  override def onElaborateGeneratorRequestComplete(
      element: ref.LibraryPath,
      values: Map[ref.LocalPath, ExprValue],
      result: Errorable[elem.HierarchyBlock]
  ): Unit = {
    interface.forwardProcessOutput()
    result match {
      case Errorable.Error(msg) =>
        console.print(
          f"Error while generating ${element.toSimpleString}: $msg\n",
          ConsoleViewContentType.ERROR_OUTPUT
        )
      case _ =>
    }
  }

  override def onRunRefinementPassComplete(
      refinementPass: ref.LibraryPath,
      result: Errorable[Map[DesignPath, ExprValue]]
  ): Unit = {
    interface.forwardProcessOutput()
    result match {
      case Errorable.Error(msg) =>
        console.print(
          f"Error while running refinement ${refinementPass.toSimpleString}: $msg\n",
          ConsoleViewContentType.ERROR_OUTPUT
        )
      case _ =>
    }
  }

  override def onRunBackendComplete(
      backend: ref.LibraryPath,
      result: Errorable[Map[DesignPath, String]]
  ): Unit = {
    interface.forwardProcessOutput()
    result match {
      case Errorable.Error(msg) =>
        console.print(
          f"Error while running backend ${backend.toSimpleString}: $msg\n",
          ConsoleViewContentType.ERROR_OUTPUT
        )
      case _ =>
    }
  }
}

object CompileProcessHandler {
  // Returns the interpreter executable from the SDK, working directory, Python paths, and the SDK name,
  // for the Python class associated with the design
  def getPythonInterpreter(project: Project, designName: String): Errorable[(String, String, Seq[String], String)] =
    exceptable {
      ReadAction.compute(() => {
        val pyPsi = PyPsiFacade.getInstance(project)
        val anchor = PsiManager.getInstance(project).findFile(project.getProjectFile)
        val pyClass = pyPsi
          .createClassByQName(designName, anchor)
          .exceptNull(s"can't find class $designName")
        val module = ModuleUtilCore.findModuleForPsiElement(pyClass).exceptNull("can't find project module")
        val sdk = PythonSdkUtil.findPythonSdk(module).exceptNull("can't find Python SDK")

        val workingDir = pyClass.getContainingFile.getContainingDirectory.getVirtualFile.getPath
        val runParams = new DesignTopRunParams(
          workingDir,
          sdk.getHomePath,
          module.getName
        )
        val pythonCommand = PythonCommandLineState
          .getInterpreterPath(project, runParams)
          .exceptNull("can't get interpreter path")
        val pythonPaths = PythonCommandLineState.collectPythonPath(module)
        (pythonCommand, workingDir, pythonPaths.asScala.toSeq, sdk.getName)
      })
    }
}

trait HasConsoleStages {
  val console: ConsoleView

  /** Logging and status wrappers for running a stage that returns some value. Exceptions are not caught and propagated
    * up. The stage function returns both a type and a message (can be empty) that is printed to the console.
    *
    * If a progress fraction is specified, the progress indicator is set to it, otherwise it is set indeterminate.
    */
  protected def runRequiredStage[ReturnType](
      name: String,
      indicator: ProgressIndicator,
      progressFrac: Option[Float] = None
  )(fn: => (ReturnType, String)): ReturnType = {
    if (Thread.interrupted()) throw new InterruptedException // must poll this regularly
    indicator.setText(f"EDG compiling: $name")
    progressFrac match {
      case None => indicator.setIndeterminate(true)
      case Some(progressFrac) =>
        indicator.setFraction(progressFrac)
        indicator.setIndeterminate(false)
    }

    val ((fnResult, fnResultStr), fnTime) = timeExec {
      fn
    }
    val addedStr = if (fnResultStr.nonEmpty) f": $fnResultStr" else ""
    console.print(s"Completed: $name$addedStr ($fnTime ms)\n", ConsoleViewContentType.LOG_INFO_OUTPUT)
    fnResult
  }

  /** Similar to (actually wraps) runRequiredStage, except errors are non-fatal and logs to console. If the function
    * fails, a specified default is returned.
    */
  protected def runFailableStage[ReturnType](
      name: String,
      default: ReturnType,
      indicator: ProgressIndicator,
      progressFrac: Option[Float] = None
  )(fn: => (ReturnType, String)): ReturnType = {
    try {
      runRequiredStage(name, indicator, progressFrac) {
        fn
      }
    } catch {
      case e: Exception if !e.isInstanceOf[InterruptedException] =>
        if (EdgSettingsState.getInstance().showIdeErrors) {
          val sw = new StringWriter
          val pw = new PrintWriter(sw)
          e.printStackTrace(pw)
          console.print(sw.toString, ConsoleViewContentType.ERROR_OUTPUT)
        }
        console.print(s"Failed: $name: $e\n", ConsoleViewContentType.ERROR_OUTPUT)
        default
    }
  }

  /** Wrapper around runFailableStage for fns that don't return anything.
    *
    * Because for some reason overloaded alternatives can't all have defaults, this needs a different name.
    */
  protected def runFailableStageUnit(
      name: String,
      indicator: ProgressIndicator,
      progressFrac: Option[Float] = None
  )(fn: => String): Unit = {
    runFailableStage[Unit](name, (), indicator, progressFrac) {
      ((), fn)
    }
  }
}

class CompileProcessHandler(
    project: Project,
    options: DesignTopRunConfigurationOptions,
    val console: ConsoleView
) extends ProcessHandler
    with HasConsoleStages {
  var runThread: Option[Thread] = None
  var pythonProcessOpt: Option[LoggingPythonInterface] = None

  override def destroyProcessImpl(): Unit = {
    console.print(f"Stopping compilation...\n", ConsoleViewContentType.ERROR_OUTPUT)

    pythonProcessOpt.foreach { pythonInterface =>
      pythonInterface.destroy()
      pythonInterface.forwardProcessOutput()
      console.print(f"Python subprocess terminated.\n", ConsoleViewContentType.ERROR_OUTPUT)
      pythonProcessOpt = None
    }
    runThread.foreach { runThread =>
      runThread.interrupt()
      console.print(f"Compilation thread interrupted.\n", ConsoleViewContentType.ERROR_OUTPUT)
    }

    terminatedNotify(-1)
  }
  override def detachProcessImpl(): Unit = {
    throw new NotImplementedError()
  }
  override def detachIsDefault(): Boolean = false
  override def getProcessInput: OutputStream = null

  def terminatedNotify(exitCode: Int): Unit = {
    notifyProcessTerminated(exitCode)
  }

  ProgressManager
    .getInstance()
    .run(new Task.Backgroundable(project, "EDG compiling") {
      override def run(indicator: ProgressIndicator): Unit = runCompile(indicator)
    })

  private def runCompile(indicator: ProgressIndicator): Unit = {
    runThread = Some(Thread.currentThread())
    startNotify()
    console.print(s"Starting compilation of ${options.designName}\n", ConsoleViewContentType.LOG_INFO_OUTPUT)
    BlockVisualizerService(project).setDesignStale()

    require(pythonProcessOpt.isEmpty)
    var exitCode: Int = -1

    try {
      val (pythonCommand, workingDir, pythonPaths, sdkName) =
        CompileProcessHandler
          .getPythonInterpreter(project, options.designName)
          .mapErr(msg => s"while getting Python interpreter path: $msg")
          .get
      console.print(
        s"Using interpreter from configured SDK '$sdkName': $pythonCommand\n",
        ConsoleViewContentType.LOG_INFO_OUTPUT
      )
      val pythonProcess = new LoggingPythonInterface(pythonCommand, Some(new File(workingDir)), pythonPaths, console)
      val pythonInterface = new LoggingCompilerInterface(pythonProcess, console)
      pythonProcessOpt = Some(pythonProcess)

      val packagePrefix = pythonProcess.packagePrefix
      if (packagePrefix.nonEmpty) {
        console.print(s"Using core prefix ${packagePrefix}\n", ConsoleViewContentType.LOG_INFO_OUTPUT)
      }

      (pythonInterface.getProtoVersion() match {
        case Errorable.Success(pyVersion) if pyVersion == Compiler.kExpectedProtoVersion => None
        case Errorable.Success(pyMismatchVersion) => Some(pyMismatchVersion.toString)
        case Errorable.Error(errMsg) => Some(s"error $errMsg")
      }).foreach { pyMismatchVersion =>
        console.print(
          f"WARNING: Python HDL version mismatch, Python reported $pyMismatchVersion, " +
            f"IDE expected ${Compiler.kExpectedProtoVersion}.\n" +
            f"If you get unexpected errors or results, consider updating the Python library or the IDE.\n",
          ConsoleViewContentType.ERROR_OUTPUT
        )
        Thread.sleep(CompilerServerMain.kHdlVersionMismatchDelayMs)
      }

      EdgCompilerService(project).pyLib.withPythonInterface(pythonInterface) {
        runFailableStageUnit("discard stale", indicator) {
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
        runFailableStageUnit("rebuild libraries", indicator, Some(0.0f)) {
          def rebuildProgressFn(library: ref.LibraryPath, index: Int, total: Int): Unit = {
            indicator.setFraction(index.toFloat / total) // this also includes requests that hit cache
          }

          val designModule = options.designName.split('.').init.mkString(".")
          val (indexed, _, _) =
            EdgCompilerService(project).rebuildLibraries(designModule, Some(rebuildProgressFn)).get
          f"${indexed.size} elements"
        }

        val (compiled, compiler, refinements) = runRequiredStage("compile", indicator, Some(0.0f)) {
          def compileProgressFn(progress: Float): Unit = {
            if (Thread.interrupted()) throw new InterruptedException // must poll this regularly
            indicator.setFraction(progress)
          }

          val designType = ElemBuilder.LibraryPath(options.designName)
          val output = EdgCompilerService(project).compile(designType, Some(compileProgressFn))
          (output, "")
        }

        val errors = runFailableStage("validate", Seq[CompilerError](), indicator) {
          val errors = compiler.getErrors() ++ new DesignAssertionCheck(compiler).map(compiled) ++
            new DesignStructuralValidate().map(compiled) ++ new DesignRefsValidate().validate(compiled)
          if (errors.nonEmpty) {
            console.print(
              s"Compiled design has ${errors.length} errors\n",
              ConsoleViewContentType.ERROR_OUTPUT
            )
          }
          (errors, f"${errors.length} errors")
        }

        runFailableStageUnit("refdes", indicator) {
          val refdes = pythonInterface.runRefinementPass(
            ElemBuilder.LibraryPath(packagePrefix + "edg.electronics_model.RefdesRefinementPass"),
            compiled,
            compiler.getAllSolved
          ).mapErr(msg => s"while refdesing: $msg")
            .get
          compiler.addAssignValues(refdes, "refdes")
          f"${refdes.size} components"
        }

        runFailableStageUnit("update visualization", indicator) {
          BlockVisualizerService(project).setDesignTop(compiled, compiler, refinements, errors)
          BlockVisualizerService(project).setLibrary(EdgCompilerService(project).pyLib)
          ""
        }

        if (options.netlistFile.nonEmpty) {
          runFailableStageUnit("generate netlist", indicator) {
            val netlist = pythonInterface.runBackend(
              ElemBuilder.LibraryPath(packagePrefix + "edg.electronics_model.NetlistBackend"),
              compiled,
              compiler.getAllSolved,
              Map("RefdesMode" -> options.toggle.toString)
            ).mapErr(msg => s"while netlisting: $msg")
              .get
            require(netlist.size == 1)

            Files.createDirectories(Paths.get(options.netlistFile).getParent)
            val writer = new FileWriter(options.netlistFile)
            writer.write(netlist.head._2)
            writer.close()
            f"wrote ${options.netlistFile}"
          }
        } else {
          console.print(
            s"Skip generating netlist, no netlist file specified in run options\n",
            ConsoleViewContentType.LOG_INFO_OUTPUT
          )
        }

        if (options.bomFile.nonEmpty) {
          runFailableStageUnit("generate BOM", indicator) {
            val bom = pythonInterface.runBackend(
              ElemBuilder.LibraryPath(packagePrefix + "edg.electronics_model.BomBackend.GenerateBom"),
              compiled,
              compiler.getAllSolved,
              Map()
            ).mapErr(msg => s"while generating bom: $msg")
              .get
            require(bom.size == 1)

            Files.createDirectories(Paths.get(options.bomFile).getParent)
            val writer = new FileWriter(options.bomFile)
            writer.write(bom.head._2)
            writer.close()
            f"wrote ${options.bomFile}"
          }
        } else {
          console.print(
            s"Skip generating BOM, no BOM file specified in run options\n",
            ConsoleViewContentType.LOG_INFO_OUTPUT
          )
        }

        if (options.pdfFile.nonEmpty) {
          runFailableStageUnit("generate PDF", indicator) {
            Files.createDirectories(Paths.get(options.pdfFile).getParent)
            PDFGeneratorUtil.generate(
              compiled.getContents,
              topMappers = Seq(
                new ElkEdgirGraphUtils.TitleMapper(compiler),
                ElkEdgirGraphUtils.DesignPathMapper,
                ElkEdgirGraphUtils.PortArrayMapper,
                new ElkEdgirGraphUtils.WireColorMapper(compiler),
                new ElkEdgirGraphUtils.WireLabelMapper(compiler),
              ),
              childMappers = Seq( // because inner blocks are deduplicated, don't run instance-specific mappers
                new ElkEdgirGraphUtils.TitleMapper(compiler),
                ElkEdgirGraphUtils.DesignPathMapper,
                ElkEdgirGraphUtils.PortArrayMapper,
              ),
              options.pdfFile
            )
            f"wrote ${options.pdfFile}"
          }
        }
      }
      exitCode = pythonProcess.shutdown()
      pythonProcess.forwardProcessOutput() // dump remaining process output (shouldn't happen)
    } catch { // this generally shouldn't happen but is an overall catch-all and clean-up
      case e: Throwable =>
        pythonProcessOpt.foreach { pyIf =>
          exitCode = pyIf.shutdown()
          pyIf.forwardProcessOutput() // dump remaining process output before the final error message
        }

        val stackWriter = new StringWriter()
        e.printStackTrace(new PrintWriter(stackWriter))
        console.print(s"Compiler internal error, compilation stopped:\n", ConsoleViewContentType.ERROR_OUTPUT)
        console.print(stackWriter.toString, ConsoleViewContentType.ERROR_OUTPUT)
    }

    console.print(s"Done\n", ConsoleViewContentType.LOG_INFO_OUTPUT)
    terminatedNotify(exitCode)
  }
}
