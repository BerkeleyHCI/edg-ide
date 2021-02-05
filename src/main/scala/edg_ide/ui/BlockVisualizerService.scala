package edg_ide.ui

import com.intellij.openapi.Disposable
import com.intellij.openapi.components.PersistentStateComponent
import com.intellij.openapi.project.Project


// Note: the implementation is here, but the actual service in plugin.xml is a Java class,
// because IntelliJ doesn't seem to like the Scala class.
object BlockVisualizerService {
  def getInstance(project: Project): BlockVisualizerService = {
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
