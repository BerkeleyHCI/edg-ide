package edg_ide.runner

import com.intellij.execution.{DefaultExecutionResult, ExecutionResult, Executor}
import com.intellij.execution.configurations.{CommandLineState, ConfigurationFactory, ConfigurationType, GeneralCommandLine, RunConfiguration, RunConfigurationBase, RunConfigurationOptions, RunProfileState}
import com.intellij.execution.filters.{TextConsoleBuilder, TextConsoleBuilderFactory, UrlFilter}
import com.intellij.execution.process.{OSProcessHandler, ProcessHandler, ProcessHandlerFactory, ProcessTerminatedListener}
import com.intellij.execution.runners.{ExecutionEnvironment, ProgramRunner}
import com.intellij.execution.ui.{ConsoleView, ConsoleViewContentType}
import com.intellij.icons.AllIcons
import com.intellij.notification.NotificationType
import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.options.SettingsEditor
import com.intellij.openapi.progress.{ProgressIndicator, ProgressManager, Task}
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.LabeledComponent
import com.intellij.openapi.util.JDOMExternalizerUtil
import com.intellij.psi.search.ExecutionSearchScopes
import com.jetbrains.python.run.PythonTracebackFilter
import edg.ElemBuilder
import edg.compiler.{DesignStructuralValidate, PythonInterface}
import edg_ide.ui.{BlockVisualizerService, EdgCompilerService}
import org.jdom.Element

import java.io.{OutputStream, PrintWriter, StringWriter}
import java.net.URI
import java.nio.file.Paths
import javax.swing.{Icon, JComponent, JPanel, JTextField}


// Most of this file is boilerplate, based on
// https://plugins.jetbrains.com/docs/intellij/run-configurations.html#implement-a-run-configuration
// The main exception is *Configuration.getState, which defines the run execution
class DesignTopRunConfigurationType extends ConfigurationType {
  override def getDisplayName: String = "DesignTop"

  override def getConfigurationTypeDescription: String = "Build a DesignTop"

  override def getIcon: Icon = AllIcons.Toolwindows.ToolWindowHierarchy

  override def getId: String = getClass.getName

  override def getConfigurationFactories: Array[ConfigurationFactory] = {
    Seq(new DesignTopConfigurationFactory(this)).toArray
  }
}


class DesignTopConfigurationFactory(confType: ConfigurationType) extends ConfigurationFactory(confType) {
  override def getId: String = getClass.getName

  override def createTemplateConfiguration(project: Project): RunConfiguration = {
    new DesignTopRunConfiguration(project, this, "DesignTop")
  }

  override def getOptionsClass: Class[DesignTopRunConfigurationOptions] = classOf[DesignTopRunConfigurationOptions]
}


class DesignTopRunConfigurationOptions extends RunConfigurationOptions {
  var designName: String = ""
}


class DesignTopRunConfiguration(project: Project, factory: ConfigurationFactory, name: String)
    extends RunConfigurationBase[DesignTopRunConfigurationOptions](project, factory, name) {
  def options: DesignTopRunConfigurationOptions = getOptions.asInstanceOf[DesignTopRunConfigurationOptions]

  override def getConfigurationEditor: SettingsEditor[_ <: RunConfiguration] = new DesignTopSettingsEditor

  override def getState(executor: Executor, environment: ExecutionEnvironment): RunProfileState = {
    new CommandLineState(environment) {
      override def startProcess(): ProcessHandler = {
        val commandLine: GeneralCommandLine = new GeneralCommandLine("TODO Command name")
        val processHandler: OSProcessHandler = ProcessHandlerFactory.getInstance.createColoredProcessHandler(commandLine)
        ProcessTerminatedListener.attach(processHandler)
        processHandler
      }
    }

    new RunProfileState {
      override def execute(executor: Executor, runner: ProgramRunner[_]): ExecutionResult = {
        val searchScope = ExecutionSearchScopes.executionScope(project, environment.getRunProfile)
        val consoleBuilder = TextConsoleBuilderFactory.getInstance().createBuilder(project, searchScope)
        val console = consoleBuilder.getConsole
        console.addMessageFilter(new PythonTracebackFilter(project))
        console.addMessageFilter(new UrlFilter())

        val processHandler = startProcess(console)
        new DefaultExecutionResult(console, processHandler)
      }

      def startProcess(console: ConsoleView): ProcessHandler = {
        val processHandler = new ProcessHandler {
          override def destroyProcessImpl(): Unit = {}
          override def detachProcessImpl(): Unit = {}
          override def detachIsDefault(): Boolean = true
          override def getProcessInput: OutputStream = null

          def terminatedNotify(exitCode: Int) = {
            notifyProcessTerminated(exitCode)
          }
        }

        val documentManager = FileDocumentManager.getInstance()
        documentManager.saveAllDocuments()

        ProgressManager.getInstance().run(new Task.Backgroundable(project, "EDG compiling") {
          override def run(indicator: ProgressIndicator): Unit = {
            processHandler.startNotify()

            try {
              EdgCompilerService(project).pyLib.withPythonInterface(
                new PythonInterface(Paths.get(project.getBasePath).resolve("HdlInterfaceService.py").toFile)) {

                indicator.setText("EDG compiling")

                val designType = ElemBuilder.LibraryPath(options.designName)
                val designModule = options.designName.split('.').init.mkString(".")
                val (compiled, compiler, refinements, reloadTime, compileTime) = EdgCompilerService(project)
                    .compile(designModule, designType, Some(indicator))

                indicator.setText("EDG compiling: validating")
                val checker = new DesignStructuralValidate()
                val errors = compiler.getErrors() ++ checker.map(compiled)
                console.print(s"Compiled (reload: $reloadTime ms, compile: $compileTime ms)",
                  ConsoleViewContentType.SYSTEM_OUTPUT)
                if (errors.nonEmpty) {
                  console.print(s"Compiled design has ${errors.length} errors", ConsoleViewContentType.ERROR_OUTPUT)
                }

                BlockVisualizerService(project).setDesignTop(compiled, compiler, refinements, errors)
                BlockVisualizerService(project).setLibrary(EdgCompilerService(project).pyLib)
              }
            } catch {
              case e: Throwable =>
                console.print(s"Compiler internal error: ${e.toString}", ConsoleViewContentType.ERROR_OUTPUT)
                val stackWriter = new StringWriter()
                e.printStackTrace(new PrintWriter(stackWriter))
                console.print(stackWriter.toString, ConsoleViewContentType.ERROR_OUTPUT)
            }
            processHandler.terminatedNotify(0)
          }
        })

        processHandler
      }
    }
  }

  val kFieldDesignName = "DESIGN_NAME"
  override def readExternal(element: Element): Unit = {
    super.readExternal(element)
    options.designName = JDOMExternalizerUtil.readField(element, kFieldDesignName)
  }
  override def writeExternal(element: Element): Unit = {
    super.writeExternal(element)
    JDOMExternalizerUtil.writeField(element, kFieldDesignName, options.designName)
  }
}


class DesignTopSettingsEditor extends SettingsEditor[DesignTopRunConfiguration] {
  protected val panel = new JPanel();
  protected val designName = LabeledComponent.create(new JTextField(), "Design top name")
  panel.add(designName)

  override def resetEditorFrom(s: DesignTopRunConfiguration): Unit = {
    designName.getComponent.setText(s.options.designName)
  }

  override def applyEditorTo(s: DesignTopRunConfiguration): Unit = {
    s.options.designName = designName.getComponent.getText
  }

  override def createEditor(): JComponent = panel
}
