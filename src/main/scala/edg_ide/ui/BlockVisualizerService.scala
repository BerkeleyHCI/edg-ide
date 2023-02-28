package edg_ide.ui

import com.intellij.execution.RunManager
import com.intellij.openapi.Disposable
import com.intellij.openapi.components.PersistentStateComponent
import com.intellij.openapi.project.Project
import com.intellij.openapi.wm.ToolWindow
import edg.EdgirUtils.SimpleLibraryPath
import edg.compiler.{Compiler, CompilerError, PythonInterfaceLibrary}
import edg.util.Errorable
import edgir.schema.schema
import edgir.elem.elem
import edgir.ref.ref
import edg.wir.DesignPath
import edg_ide.dse.{DseConfigElement, DseObjective, DseResult}
import edg_ide.proven.{ProvenDataReader, ProvenDatabase}
import edg_ide.runner.{DseConfigurationFactory, DseRunConfiguration, DseRunConfigurationType}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptNotify, ExceptOption}
import edg_ide.util.exceptable
import edgrpc.hdl.{hdl => edgrpc}

import java.io.File
import scala.collection.SeqMap


// Note: the implementation is here, but the actual service in plugin.xml is a Java class,
// because IntelliJ doesn't seem to like the Scala class.
object BlockVisualizerService {
  def apply(project: Project): BlockVisualizerService = {
    project.getService(classOf[BlockVisualizerServiceWrapper]).asInstanceOf[BlockVisualizerService]
  }
}

class BlockVisualizerService(project: Project) extends
    PersistentStateComponent[BlockVisualizerServiceState] with Disposable {
  private var visualizerPanel: Option[BlockVisualizerPanel] = None
  private var initialState: Option[BlockVisualizerServiceState] = None

  def visualizerPanelOption: Option[BlockVisualizerPanel] = visualizerPanel

  def dsePanelOption: Option[DsePanel] = visualizerPanelOption.map(_.getDsePanel)

  def createBlockVisualizerPanel(toolWindow: ToolWindow): BlockVisualizerPanel = {
    require(visualizerPanel.isEmpty)
    val created = new BlockVisualizerPanel(project, toolWindow)
    initialState.foreach { state => created.loadState(state) }
    visualizerPanel = Some(created)
    created
  }

  def selectPath(path: DesignPath): Unit = {
    visualizerPanelOption.foreach(_.selectPath(path))
  }

  def setContext(path: DesignPath): Unit = {
    visualizerPanelOption.foreach(_.setContext(path))
  }

  def getContextBlock: Option[(DesignPath, elem.HierarchyBlock)] = {
    visualizerPanelOption.flatMap(_.getContextBlock)
  }

  def setLibrary(library: PythonInterfaceLibrary): Unit = {
    visualizerPanelOption.foreach(_.updateLibrary(library))
  }

  def setDesignTop(design: schema.Design, compiler: Compiler, refinements: edgrpc.Refinements,
                   errors: Seq[CompilerError], namePrefix: Option[String] = None): Unit = {
    visualizerPanelOption.foreach(_.setDesignTop(design, compiler, refinements, errors, namePrefix))
  }

  def getDesign: Option[schema.Design] = {
    visualizerPanelOption.map(_.getDesign)
  }

  // DSE Feature
  //
  // TODO maybe separate DSE functionality into its own class / service?
  // but this is mixed into the block diagram visualizer panel and the two are quite linked

  // Called when the run configuration changes
  def onDseConfigChanged(config: DseRunConfiguration): Unit = {
    dsePanelOption.foreach(_.onConfigChange(config))
  }

  // Returns the currently selected run configuration, if it is a DSE configuration
  def getDseRunConfiguration: Option[DseRunConfiguration] = {
    Option(RunManager.getInstance(project).getSelectedConfiguration)
        .map(_.getConfiguration)
        .collect { case config: DseRunConfiguration => config }
  }

  def getOrCreateDseRunConfiguration(blockType: ref.LibraryPath): DseRunConfiguration = {
    val existingConfig = Option(RunManager.getInstance(project).getSelectedConfiguration)
        .map(_.getConfiguration)
        .collect { case config: DseRunConfiguration => config } match {
      case Some(existingConfig) if existingConfig.options.designName == blockType.toFullString =>
        Some(existingConfig)
      case _ => None
    }
    existingConfig.getOrElse {  // if no existing config of the type, create a new one
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

  def addDseConfig(blockType: ref.LibraryPath, newConfig: DseConfigElement): Unit = {
    val config = getOrCreateDseRunConfiguration(blockType)
    config.options.searchConfigs = config.options.searchConfigs ++ Seq(newConfig)
    onDseConfigChanged(config)
  }

  def setDseResults(results: Seq[DseResult], objectives: Seq[DseObjective],
                    inProgress: Boolean): Unit = {
    dsePanelOption.foreach(_.setResults(results, objectives, inProgress))
  }

  // Proven feature
  //
  lazy val getProvenDatabase: ProvenDatabase = {
    ProvenDataReader.read(new File("src/main/resources/proven-designs/data.csv"))
  }

  // State management
  //
  override def getState: BlockVisualizerServiceState = {
    val state = new BlockVisualizerServiceState
    visualizerPanel.foreach { _.saveState(state) }
    state
  }

  override def loadState(state: BlockVisualizerServiceState): Unit = {
    initialState = Some(state)
  }

  override def dispose(): Unit = { }
}
