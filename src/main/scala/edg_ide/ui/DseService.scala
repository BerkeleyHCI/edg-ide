package edg_ide.ui

import com.intellij.execution.RunManager
import com.intellij.openapi.Disposable
import com.intellij.openapi.components.PersistentStateComponent
import com.intellij.openapi.project.Project
import edg.EdgirUtils.SimpleLibraryPath
import edg_ide.dse.{DseConfigElement, DseObjective, DseResult}
import edg_ide.runner.{DseConfigurationFactory, DseRunConfiguration, DseRunConfigurationType}
import edgir.ref.ref

import java.awt.Component
import scala.jdk.CollectionConverters.ListHasAsScala

object DseService {
  def apply(project: Project): DseService = {
    project.getService(classOf[DseServiceWrapper]).asInstanceOf[DseService]
  }

  def option(project: Project): Option[DseService] = {
    Option(project.getServiceIfCreated(classOf[DseServiceWrapper])).map(_.asInstanceOf[DseService])
  }
}

class DseService(project: Project) extends PersistentStateComponent[DseServiceState] with Disposable {
  private def dsePanelOption: Option[DsePanel] =
    BlockVisualizerService(project).visualizerPanelOption.map(_.getDsePanel)
  private var initialState: Option[DseServiceState] = None

  // Called when the run configuration changes
  def onSearchConfigChanged(config: DseRunConfiguration, scrollToLast: Boolean): Unit = {
    dsePanelOption.foreach { panel =>
      panel.onConfigChange(config)
      panel.focusConfigSearch(scrollToLast)
    }
  }

  def onObjectiveConfigChanged(config: DseRunConfiguration, scrollToLast: Boolean): Unit = {
    dsePanelOption.foreach { panel =>
      panel.onConfigChange(config)
      panel.focusConfigObjective(scrollToLast)
    }
  }

  // Returns the currently selected run configuration, if it is a DSE configuration
  def getRunConfiguration: Option[DseRunConfiguration] = {
    Option(RunManager.getInstance(project).getSelectedConfiguration)
      .map(_.getConfiguration)
      .collect { case config: DseRunConfiguration => config }
  }

  def getOrCreateRunConfiguration(blockType: ref.LibraryPath, owner: Component): DseRunConfiguration = {
    val runManager = RunManager.getInstance(project)
    val currentConfig = Option(runManager.getSelectedConfiguration)
      .map(_.getConfiguration)
      .collect {
        case config: DseRunConfiguration if config.options.designName == blockType.toFullString => config
      }
    val existingConfig = currentConfig.orElse {
      runManager.getAllSettings.asScala
        .map { configSettings =>
          (configSettings, configSettings.getConfiguration)
        }
        .collectFirst {
          case (configSettings, config: DseRunConfiguration)
            if config.options.designName == blockType.toFullString =>
            (configSettings, config)
        }
        .map { case (configSettings, config) =>
          runManager.setSelectedConfiguration(configSettings)
          PopupUtils.createPopupAtMouse(s"switched to existing DSE config ${config.getName}", owner)
          config
        }
    }
    existingConfig.getOrElse { // if no existing config of the type, create a new one
      val newConfigSettings = runManager.createConfiguration(
        blockType.toFullString,
        new DseConfigurationFactory(new DseRunConfigurationType)
      )
      runManager.addConfiguration(newConfigSettings)
      runManager.setSelectedConfiguration(newConfigSettings)

      val newConfig = newConfigSettings.getConfiguration.asInstanceOf[DseRunConfiguration]
      newConfig.options.designName = blockType.toFullString
      PopupUtils.createPopupAtMouse(s"created new DSE config ${newConfig.getName}", owner)

      newConfig
    }
  }

  def addSearchConfig(blockType: ref.LibraryPath, newConfig: DseConfigElement, owner: Component): Unit = {
    val config = getOrCreateRunConfiguration(blockType, owner)
    config.options.searchConfigs = config.options.searchConfigs :+ newConfig
    onSearchConfigChanged(config, true)
  }

  def setResults(
      results: Seq[DseResult],
      search: Seq[DseConfigElement],
      objectives: Seq[DseObjective],
      inProgress: Boolean,
      initial: Boolean
  ): Unit = {
    dsePanelOption.foreach { panel =>
      panel.setResults(results, search, objectives, inProgress)
      if (initial) { // only set focus on the initial result - to not squash user intent
        panel.focusResults()
      }
    }
  }

  // State management
  //
  override def getState: DseServiceState = {
    val state = new DseServiceState
    dsePanelOption.foreach {
      _.saveState(state)
    }
    state
  }

  override def loadState(state: DseServiceState): Unit = {
    // TODO: nothing actually propagates this to the panel, and state doesn't get saved
    initialState = Some(state)
  }

  override def dispose(): Unit = {}
}
