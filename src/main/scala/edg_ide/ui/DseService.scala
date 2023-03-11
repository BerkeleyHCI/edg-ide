package edg_ide.ui

import com.intellij.execution.RunManager
import com.intellij.openapi.Disposable
import com.intellij.openapi.components.PersistentStateComponent
import com.intellij.openapi.project.Project
import edg.EdgirUtils.SimpleLibraryPath
import edg_ide.dse.{DseConfigElement, DseObjective, DseResult}
import edg_ide.runner.{DseConfigurationFactory, DseRunConfiguration, DseRunConfigurationType}
import edgir.ref.ref


object DseService {
  def apply(project: Project): DseService = {
    project.getService(classOf[DseServiceWrapper]).asInstanceOf[DseService]
  }
}

class DseService(project: Project) extends
    PersistentStateComponent[DseServiceState] with Disposable {
  private def dsePanelOption: Option[DsePanel] = BlockVisualizerService(project).visualizerPanelOption.map(_.getDsePanel)
  private var initialState: Option[DseServiceState] = None

  // Called when the run configuration changes
  def onSearchConfigChanged(config: DseRunConfiguration): Unit = {
    dsePanelOption.foreach { panel =>
      panel.onConfigChange(config)
      panel.focusConfigSearch()
    }
  }

  def onObjectiveConfigChanged(config: DseRunConfiguration): Unit = {
    dsePanelOption.foreach { panel =>
      panel.onConfigChange(config)
      panel.focusConfigObjective()
    }
  }

  // Returns the currently selected run configuration, if it is a DSE configuration
  def getRunConfiguration: Option[DseRunConfiguration] = {
    Option(RunManager.getInstance(project).getSelectedConfiguration)
        .map(_.getConfiguration)
        .collect { case config: DseRunConfiguration => config }
  }

  def getOrCreateRunConfiguration(blockType: ref.LibraryPath): DseRunConfiguration = {
    val existingConfig = Option(RunManager.getInstance(project).getSelectedConfiguration)
        .map(_.getConfiguration)
        .collect { case config: DseRunConfiguration => config } match {
      case Some(existingConfig) if existingConfig.options.designName == blockType.toFullString =>
        Some(existingConfig)
      case _ => None
    }
    existingConfig.getOrElse { // if no existing config of the type, create a new one
      val runManager = RunManager.getInstance(project)
      val newRunnerConfig = runManager.createConfiguration(
        blockType.toFullString, new DseConfigurationFactory(new DseRunConfigurationType))
      runManager.addConfiguration(newRunnerConfig)
      runManager.setSelectedConfiguration(newRunnerConfig)

      val newConfig = newRunnerConfig.getConfiguration.asInstanceOf[DseRunConfiguration]
      newConfig.options.designName = blockType.toFullString
      newConfig
    }
  }

  def addConfig(blockType: ref.LibraryPath, newConfig: DseConfigElement): Unit = {
    val config = getOrCreateRunConfiguration(blockType)
    config.options.searchConfigs = config.options.searchConfigs ++ Seq(newConfig)
    onSearchConfigChanged(config)
  }

  def setResults(results: Seq[DseResult], search: Seq[DseConfigElement], objectives: Seq[DseObjective],
                 inProgress: Boolean, initial: Boolean): Unit = {
    dsePanelOption.foreach { panel =>
      panel.setResults(results, search, objectives, inProgress)
      if (initial) {  // only set focus on the initial result - to not squash user intent
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
