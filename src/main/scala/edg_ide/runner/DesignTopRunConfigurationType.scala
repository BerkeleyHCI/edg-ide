package edg_ide.runner

import com.intellij.execution.configurations._
import com.intellij.execution.filters.{TextConsoleBuilderFactory, UrlFilter}
import com.intellij.execution.runners.{ExecutionEnvironment, ProgramRunner}
import com.intellij.execution.{DefaultExecutionResult, ExecutionResult, Executor}
import com.intellij.icons.AllIcons
import com.intellij.openapi.fileChooser.{FileChooser, FileChooserDescriptorFactory}
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.options.SettingsEditor
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.TextFieldWithBrowseButton
import com.intellij.openapi.util.JDOMExternalizerUtil
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile}
import com.intellij.psi.search.ExecutionSearchScopes
import com.intellij.util.Consumer
import com.jetbrains.python.run.PythonTracebackFilter
import org.jdom.Element

import java.awt.GridLayout
import java.awt.event.{ActionEvent, ActionListener}
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


class DesignTopRunConfigurationOptions extends RunConfigurationOptions {
  var designName: String = ""
  var netlistFile: String = ""
}


class DesignTopRunConfiguration(project: Project, factory: ConfigurationFactory, name: String)
    extends RunConfigurationBase[DesignTopRunConfigurationOptions](project, factory, name) {
  def options: DesignTopRunConfigurationOptions = getOptions.asInstanceOf[DesignTopRunConfigurationOptions]

  override def getConfigurationEditor: SettingsEditor[_ <: RunConfiguration] = new DesignTopSettingsEditor(project)

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
  override def readExternal(element: Element): Unit = {
    super.readExternal(element)
    options.designName = JDOMExternalizerUtil.readField(element, kFieldDesignName)
    options.netlistFile = JDOMExternalizerUtil.readField(element, kFieldNetlistName)
  }
  override def writeExternal(element: Element): Unit = {
    super.writeExternal(element)
    JDOMExternalizerUtil.writeField(element, kFieldDesignName, options.designName)
    JDOMExternalizerUtil.writeField(element, kFieldNetlistName, options.netlistFile)
  }
}


class DesignTopSettingsEditor(project: Project) extends SettingsEditor[DesignTopRunConfiguration] {
  protected val panel = new JPanel()
  panel.setLayout(new GridLayout(0, 2))

  panel.add(new JLabel("Design top name"))
  val designName = new JTextField()
  panel.add(designName)

  panel.add(new JLabel("Netlist output file"))
  val netlistFile = new JTextField()  // no browse button b/c FileChooser can't create new filesJTextField
  panel.add(netlistFile)

  override def resetEditorFrom(s: DesignTopRunConfiguration): Unit = {
    designName.setText(s.options.designName)
    netlistFile.setText(s.options.netlistFile)
  }

  override def applyEditorTo(s: DesignTopRunConfiguration): Unit = {
    s.options.designName = designName.getText
    s.options.netlistFile = netlistFile.getText
  }

  override def createEditor(): JComponent = panel
}
