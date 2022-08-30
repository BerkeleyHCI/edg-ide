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
import com.intellij.ui.components.JBLabel
import com.intellij.util.ui.FormBuilder
import com.jetbrains.python.run.PythonTracebackFilter
import org.jdom.Element

import java.awt.GridLayout
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
  var resultCsvFile: String = ""
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
  val kFieldResultCsvFile = "RESULT_CSV_FILE"
  override def readExternal(element: Element): Unit = {
    super.readExternal(element)
    options.designName = JDOMExternalizerUtil.readField(element, kFieldDesignName)
    options.resultCsvFile = JDOMExternalizerUtil.readField(element, kFieldResultCsvFile)
  }
  override def writeExternal(element: Element): Unit = {
    super.writeExternal(element)
    JDOMExternalizerUtil.writeField(element, kFieldDesignName, options.designName)
    JDOMExternalizerUtil.writeField(element, kFieldResultCsvFile, options.resultCsvFile)
  }
}


class DseSettingsEditor extends SettingsEditor[DseRunConfiguration] {
  protected val designName = new JTextField()
  protected val resultCsvFile = new JTextField()

  val panel = FormBuilder.createFormBuilder()
      .addLabeledComponent(new JBLabel("Design top name"), designName, false)
      .addLabeledComponent(new JBLabel("Result CSV file"), resultCsvFile, false)
      .addComponentFillVertically(new JPanel(), 0)
      .getPanel

  override def resetEditorFrom(s: DseRunConfiguration): Unit = {
    designName.setText(s.options.designName)
    resultCsvFile.setText(s.options.resultCsvFile)
  }

  override def applyEditorTo(s: DseRunConfiguration): Unit = {
    s.options.designName = designName.getText
    s.options.resultCsvFile = resultCsvFile.getText
  }

  override def createEditor(): JComponent = panel
}
