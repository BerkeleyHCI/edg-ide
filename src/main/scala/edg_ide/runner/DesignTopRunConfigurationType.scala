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
import com.intellij.ui.components.{JBLabel, JBRadioButton}
import com.intellij.util.ui.FormBuilder
import com.jetbrains.python.run.PythonTracebackFilter
import edgir.init.init.ValInit.Val.Boolean
import org.jdom.Element

import java.awt.GridLayout
import javax.swing._


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

object RefdesMode extends Enumeration {
  type selections = Value

  val refdes = Value(0, "refdes")
  val pathName = Value(1, "pathName")

  def toEnum(s: String): Option[Value] = {
    values.find(_.toString == s)
  }
}

class DesignTopRunConfigurationOptions extends RunConfigurationOptions {
  var designName: String = ""
  var netlistFile: String = ""
  var pdfFile: String = ""
  var toggle: RefdesMode.selections = RefdesMode.refdes
}

class DesignTopRunConfiguration(project: Project, factory: ConfigurationFactory, name: String)
    extends RunConfigurationBase[DesignTopRunConfigurationOptions](project, factory, name) {
  def options: DesignTopRunConfigurationOptions = getOptions.asInstanceOf[DesignTopRunConfigurationOptions]

  override def getConfigurationEditor: SettingsEditor[_ <: RunConfiguration] = new DesignTopSettingsEditor(project)

  // This is new
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

        val processHandler = new CompileProcessHandler(project, options, console)
        new DefaultExecutionResult(console, processHandler)
      }
    }
  }

  val kFieldDesignName = "DESIGN_NAME"
  val kFieldNetlistName = "NETLIST_NAME"
  val kPdfFileName = "PDF_NAME"
  val kFieldValueMode = "VALUEMODE_NAME"

  // Allows persistence of run configuration
  override def readExternal(element: Element): Unit = {
    super.readExternal(element)
    options.designName = JDOMExternalizerUtil.readField(element, kFieldDesignName, "")
    options.netlistFile = JDOMExternalizerUtil.readField(element, kFieldNetlistName, "")
    options.pdfFile = JDOMExternalizerUtil.readField(element, kPdfFileName, "")
    RefdesMode.toEnum(JDOMExternalizerUtil.readField(element, kFieldValueMode)).foreach(options.toggle = _)
  }

  override def writeExternal(element: Element): Unit = {
    super.writeExternal(element)
    JDOMExternalizerUtil.writeField(element, kFieldDesignName, options.designName)
    JDOMExternalizerUtil.writeField(element, kFieldNetlistName, options.netlistFile)
    JDOMExternalizerUtil.writeField(element, kPdfFileName, options.pdfFile)
    JDOMExternalizerUtil.writeField(element, kFieldValueMode, options.toggle.toString)
  }
}

class DesignTopSettingsEditor(project: Project) extends SettingsEditor[DesignTopRunConfiguration] {
  protected val designName = new JTextField()
  protected val netlistFile = new JTextField()  // no browse button b/c FileChooser can't create new files
  protected val pdfFile = new JTextField()
  protected val toggleRefdes = new JBRadioButton()
  protected val togglePathname = new JBRadioButton()
  protected val toggleButtons = new ButtonGroup()
  toggleButtons.add(toggleRefdes)
  toggleButtons.add(togglePathname)

  protected val toggleRefdes = new JBRadioButton()
  protected val togglePathname = new JBRadioButton()
  protected val toggleButtons = new ButtonGroup()
  toggleButtons.add(toggleRefdes)
  toggleButtons.add(togglePathname)

  protected val panel = FormBuilder.createFormBuilder()
      .addLabeledComponent(new JBLabel("Design top name"), designName, false)
      .addLabeledComponent(new JBLabel("Select Netlist Refdes value"), toggleRefdes)
      .addLabeledComponent(new JBLabel("Select Netlist Path Name"), togglePathname)
      .addLabeledComponent(new JBLabel("Netlist output file"), netlistFile, false)
      .addLabeledComponent(new JBLabel("PDF output file"), pdfFile, false)
      .addComponentFillVertically(new JPanel(), 0)
      .getPanel

  override def resetEditorFrom(s: DesignTopRunConfiguration): Unit = {
    designName.setText(s.options.designName)
    netlistFile.setText(s.options.netlistFile)
    pdfFile.setText(s.options.pdfFile)

    s.options.toggle match {
      case RefdesMode.refdes =>
        toggleRefdes.setSelected(true)
        togglePathname.setSelected(false)
      case RefdesMode.pathName =>
        toggleRefdes.setSelected(false)
        togglePathname.setSelected(true)
    }
  }

  override def applyEditorTo(s: DesignTopRunConfiguration): Unit = {
    s.options.designName = designName.getText
    s.options.netlistFile = netlistFile.getText
    s.options.pdfFile = pdfFile.getText
    if (toggleRefdes.isSelected){
      s.options.toggle = RefdesMode.refdes
    } else if (togglePathname.isSelected) {
      s.options.toggle = RefdesMode.pathName
    } else {
      // Ignore invalid user selection
    }
  }

  override def createEditor(): JComponent = panel
}
