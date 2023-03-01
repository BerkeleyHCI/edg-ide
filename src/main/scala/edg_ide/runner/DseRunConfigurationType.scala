package edg_ide.runner

import com.intellij.execution.configurations._
import com.intellij.execution.filters.{TextConsoleBuilderFactory, UrlFilter}
import com.intellij.execution.runners.{ExecutionEnvironment, ProgramRunner}
import com.intellij.execution.{DefaultExecutionResult, ExecutionResult, Executor}
import com.intellij.icons.AllIcons
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.options.SettingsEditor
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.JDOMExternalizerUtil
import com.intellij.psi.search.ExecutionSearchScopes
import com.intellij.ui.components.JBLabel
import com.intellij.util.ui.FormBuilder
import com.jetbrains.python.run.PythonTracebackFilter
import edg.ElemBuilder
import edg.compiler.RangeValue
import edg.wir.DesignPath
import edg_ide.dse._
import edg_ide.util.ObjectSerializer
import org.jdom.Element

import javax.swing.{Icon, JComponent, JLabel, JPanel, JTextField}
import scala.collection.SeqMap


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

  var searchConfigs: Seq[DseConfigElement] = Seq()
  var objectives: Seq[DseObjective] = Seq()
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

  private val kFieldDesignName = "DESIGN_NAME"
  private val kFieldResultCsvFile = "RESULT_CSV_FILE"
  private val kFieldSearchConfigs = "SEARCH_CONFIGS"
  private val kFieldObjectives = "OBJECTIVES"
  override def readExternal(element: Element): Unit = {
    super.readExternal(element)
    options.designName = JDOMExternalizerUtil.readField(element, kFieldDesignName, "")
    options.resultCsvFile = JDOMExternalizerUtil.readField(element, kFieldResultCsvFile, "")
    Option(JDOMExternalizerUtil.readField(element, kFieldSearchConfigs))
        .flatMap(ObjectSerializer.deserialize)
        .flatMap(ObjectSerializer.optionInstanceOfSeq[DseConfigElement])
        .foreach(options.searchConfigs = _)  // only set if valid, otherwise leave as default
    Option(JDOMExternalizerUtil.readField(element, kFieldObjectives))
        .flatMap(ObjectSerializer.deserialize)
        .flatMap(ObjectSerializer.optionInstanceOfSeq[DseObjective])
        .foreach(options.objectives = _)  // only set if valid, otherwise leave as default
  }
  override def writeExternal(element: Element): Unit = {
    super.writeExternal(element)
    JDOMExternalizerUtil.writeField(element, kFieldDesignName, options.designName)
    JDOMExternalizerUtil.writeField(element, kFieldResultCsvFile, options.resultCsvFile)
    JDOMExternalizerUtil.writeField(element, kFieldSearchConfigs, ObjectSerializer.serialize(options.searchConfigs))
    JDOMExternalizerUtil.writeField(element, kFieldObjectives, ObjectSerializer.serialize(options.objectives))
  }
}


class DseSettingsEditor extends SettingsEditor[DseRunConfiguration] {
  protected val designName = new JTextField()
  protected val resultCsvFile = new JTextField()
  protected val searchConfigs = new JLabel()  // view only - set from DSE tab in BlockVisualizer panel
  protected val objectives = new JLabel()

  protected val panel = FormBuilder.createFormBuilder()
      .addLabeledComponent(new JBLabel("Design top name"), designName, false)
      .addLabeledComponent(new JBLabel("Result CSV file"), resultCsvFile, false)
      .addLabeledComponent(new JBLabel("Parameter search configs"), searchConfigs, false)
      .addLabeledComponent(new JBLabel("Objective functions"), objectives, false)
      .addComponentFillVertically(new JPanel(), 0)
      .getPanel

  override def resetEditorFrom(s: DseRunConfiguration): Unit = {
    designName.setText(s.options.designName)
    resultCsvFile.setText(s.options.resultCsvFile)
    searchConfigs.setText("<html>" + s.options.searchConfigs.map(_.configToString).mkString("<br/>") + "</html>")
    objectives.setText("<html>" + s.options.objectives.map(_.objectiveToString).mkString("<br/>") + "</html>")
  }

  override def applyEditorTo(s: DseRunConfiguration): Unit = {
    s.options.designName = designName.getText
    s.options.resultCsvFile = resultCsvFile.getText
  }

  override def createEditor(): JComponent = panel
}
