package edg_ide.runner

import com.intellij.execution.configurations._
import com.intellij.execution.filters.{TextConsoleBuilderFactory, UrlFilter}
import com.intellij.execution.runners.{ExecutionEnvironment, ProgramRunner}
import com.intellij.execution.{DefaultExecutionResult, ExecutionResult, Executor}
import com.intellij.icons.AllIcons
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.options.SettingsEditor
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.LabeledComponent
import com.intellij.openapi.util.JDOMExternalizerUtil
import com.intellij.psi.search.ExecutionSearchScopes
import com.jetbrains.python.run.PythonTracebackFilter
import org.jdom.Element

import javax.swing.{Icon, JComponent, JPanel, JTextField}


// Run configuration for design space exploration (DSE) / search, which tries lots of variations
// of a design and assesses tradeoffs and finds the pareto front.
class DseRunConfigurationType extends ConfigurationType {
  override def getDisplayName: String = "Design Space Search"

  override def getConfigurationTypeDescription: String = "Search a Design Space"

  override def getIcon: Icon = AllIcons.Actions.ShortcutFilter

  override def getId: String = getClass.getName

  override def getConfigurationFactories: Array[ConfigurationFactory] = {
    Seq(new DseConfigurationFactory(this)).toArray
  }
}


class DseConfigurationFactory(confType: ConfigurationType) extends ConfigurationFactory(confType) {
  override def getId: String = getClass.getName

  override def createTemplateConfiguration(project: Project): RunConfiguration = {
    new DseRunConfiguration(project, this, "Design Space Search")
  }

  override def getOptionsClass: Class[DseRunConfigurationOptions] = classOf[DseRunConfigurationOptions]
}


class DseRunConfigurationOptions extends RunConfigurationOptions {
  var designName: String = ""
}


class DseRunConfiguration(project: Project, factory: ConfigurationFactory, name: String)
    extends RunConfigurationBase[DseRunConfigurationOptions](project, factory, name) {
  def options: DseRunConfigurationOptions = getOptions.asInstanceOf[DseRunConfigurationOptions]

  override def getConfigurationEditor: SettingsEditor[_ <: RunConfiguration] = new DseSettingsEditor

  override def getState(executor: Executor, environment: ExecutionEnvironment): RunProfileState = {
    new RunProfileState {
      override def execute(executor: Executor, runner: ProgramRunner[_]): ExecutionResult = {
        val searchScope = ExecutionSearchScopes.executionScope(project, environment.getRunProfile)
        val consoleBuilder = TextConsoleBuilderFactory.getInstance().createBuilder(project, searchScope)
        val console = consoleBuilder.getConsole
        console.addMessageFilter(new PythonTracebackFilter(project))
        console.addMessageFilter(new UrlFilter())

        val documentManager = FileDocumentManager.getInstance()
        documentManager.saveAllDocuments()

        val processHandler = new DseProcessHandler(project, options, console)
        new DefaultExecutionResult(console, processHandler)
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


class DseSettingsEditor extends SettingsEditor[DseRunConfiguration] {
  protected val panel = new JPanel();
  protected val designName = LabeledComponent.create(new JTextField(), "Design top name")
  panel.add(designName)

  override def resetEditorFrom(s: DseRunConfiguration): Unit = {
    designName.getComponent.setText(s.options.designName)
  }

  override def applyEditorTo(s: DseRunConfiguration): Unit = {
    s.options.designName = designName.getComponent.getText
  }

  override def createEditor(): JComponent = panel
}
