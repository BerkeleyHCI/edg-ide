package edg_ide.ui

import com.intellij.openapi.Disposable
import com.intellij.openapi.components.PersistentStateComponent
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import edg.wir.DesignPath


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

  def createBlockVisualizerPanel(): BlockVisualizerPanel = {
    require(visualizerPanel.isEmpty)
    val created = new BlockVisualizerPanel(project)
    initialState.foreach { state => created.loadState(state) }
    visualizerPanel = Some(created)
    created
  }


  def setContext(path: DesignPath): Unit = {
    visualizerPanelOption.foreach(_.setContext(path))
  }

  def setModuleClass(module: String, block: String): Unit = {
    visualizerPanelOption.foreach(_.setFileBlock(module, block))
    update()
  }

  def update(): Unit = {
    visualizerPanelOption.foreach(_.update())
  }

  def getModule(): String = {
    visualizerPanelOption.get.getModule
  }

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
