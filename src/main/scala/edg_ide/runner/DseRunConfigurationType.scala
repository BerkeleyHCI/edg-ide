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

  // TODO this should be the more generic DseConfigElement, but for now it's refinement types only
  var searchConfigs: Seq[DseRefinementElement] = Seq(
    DseSubclassSearch(DesignPath() + "reg_5v",
      Seq(
        "electronics_lib.BuckConverter_TexasInstruments.Tps561201",
        "electronics_lib.BuckConverter_TexasInstruments.Tps54202h",
      ).map(value => ElemBuilder.LibraryPath(value))
    ),
    DseParameterSearch(DesignPath() + "reg_5v" + "ripple_current_factor",
      Seq(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5).map(value => RangeValue(value - 0.05, value + 0.05))
    ),
  )
  var objectives: SeqMap[String, DseObjective[Any]] = SeqMap(
    "inductor" -> DseObjectiveParameter(DesignPath() + "reg_5v" + "power_path" + "inductor" + "actual_part"),
    "inductor_val" -> DseObjectiveParameter(DesignPath() + "reg_5v" + "power_path" + "inductor" + "fp_value"),
    "inductance" -> DseObjectiveParameter(DesignPath() + "reg_5v" + "power_path" + "inductor" + "actual_inductance"),
    "5v_area" -> DseObjectiveFootprintArea(DesignPath() + "reg_5v"),
    "5v_count" -> DseObjectiveFootprintCount(DesignPath() + "reg_5v"),
  )
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
  protected val searchConfigs = new JLabel()
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
    searchConfigs.setText("<html>" + s.options.searchConfigs.map(_.toString).mkString("<br/>") + "</html>")
    objectives.setText("<html>" + s.options.objectives.map(_.toString).mkString("<br/>") + "</html>")
  }

  override def applyEditorTo(s: DseRunConfiguration): Unit = {
    s.options.designName = designName.getText
    s.options.resultCsvFile = resultCsvFile.getText
  }

  override def createEditor(): JComponent = panel
}
