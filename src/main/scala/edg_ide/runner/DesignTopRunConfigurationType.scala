package edg_ide.runner

import com.intellij.execution.Executor
import com.intellij.execution.configurations.{CommandLineState, ConfigurationFactory, ConfigurationType, GeneralCommandLine, RunConfiguration, RunConfigurationBase, RunConfigurationOptions, RunProfileState}
import com.intellij.execution.process.{OSProcessHandler, ProcessHandler, ProcessHandlerFactory, ProcessTerminatedListener}
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.icons.AllIcons
import com.intellij.openapi.options.SettingsEditor
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.LabeledComponent

import javax.swing.{Icon, JComponent, JPanel, JTextField}


// Based on
// https://plugins.jetbrains.com/docs/intellij/run-configurations.html#implement-a-run-configuration


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
