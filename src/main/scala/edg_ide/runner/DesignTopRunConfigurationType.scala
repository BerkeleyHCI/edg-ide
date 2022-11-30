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

object Toggle extends Enumeration {
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
  var toggle: Toggle.selections = Toggle.refdes
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
  val kFieldToggle = "TOGGLE_NAME"

  // Allows persistence of run configuration
  override def readExternal(element: Element): Unit = {
    super.readExternal(element)
    options.designName = JDOMExternalizerUtil.readField(element, kFieldDesignName)
    options.netlistFile = JDOMExternalizerUtil.readField(element, kFieldNetlistName)
    Toggle.toEnum(JDOMExternalizerUtil.readField(element, kFieldToggle)).foreach(options.toggle = _)
  }

  override def writeExternal(element: Element): Unit = {
    super.writeExternal(element)
    JDOMExternalizerUtil.writeField(element, kFieldDesignName, options.designName)
    JDOMExternalizerUtil.writeField(element, kFieldNetlistName, options.netlistFile)
    JDOMExternalizerUtil.writeField(element, kFieldToggle, options.toggle.toString)
  }
}

class DesignTopSettingsEditor(project: Project) extends SettingsEditor[DesignTopRunConfiguration] {
  protected val designName = new JTextField()
  protected val netlistFile = new JTextField()  // no browse button b/c FileChooser can't create new files

  protected val toggleRefdes = new JBRadioButton("Refdes")
  protected val togglePathname = new JBRadioButton("Path Name")
  protected val toggleButtons = new ButtonGroup()
  toggleButtons.add(toggleRefdes)
  toggleButtons.add(togglePathname)

  protected val panel = FormBuilder.createFormBuilder()
      .addLabeledComponent(new JBLabel("Design top name"), designName, false)
      .addComponent(toggleRefdes)
      .addComponent(togglePathname)
      .addLabeledComponent(new JBLabel("Netlist output file"), netlistFile, false)
      .addComponentFillVertically(new JPanel(), 0)
      .getPanel

  override def resetEditorFrom(s: DesignTopRunConfiguration): Unit = {
    designName.setText(s.options.designName)
    netlistFile.setText(s.options.netlistFile)

    s.options.toggle match {
      case Toggle.refdes =>
        toggleRefdes.setSelected(true)
        togglePathname.setSelected(false)
      case Toggle.pathName =>
        toggleRefdes.setSelected(false)
        togglePathname.setSelected(true)
    }
  }

  override def applyEditorTo(s: DesignTopRunConfiguration): Unit = {
    s.options.designName = designName.getText
    s.options.netlistFile = netlistFile.getText
    if (toggleRefdes.isSelected){
      s.options.toggle = Toggle.refdes
    } else if (togglePathname.isSelected) {
      s.options.toggle = Toggle.pathName
    } else {
      // Ignore invalid user selection
    }
  }

  override def createEditor(): JComponent = panel
}
