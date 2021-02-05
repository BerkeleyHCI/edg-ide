package edg_ide.ui

import com.intellij.openapi.Disposable
import com.intellij.openapi.components.PersistentStateComponent
import com.intellij.openapi.project.Project


class BlockVisualizerServiceState {

}


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

  def visualizerPanelOption: Option[BlockVisualizerPanel] = visualizerPanel

  def createBlockVisualizerPanel(): BlockVisualizerPanel = {
    require(visualizerPanel.isEmpty)
    val created = new BlockVisualizerPanel(project)
    visualizerPanel = Some(created)
    created
  }


  override def getState: BlockVisualizerServiceState = new BlockVisualizerServiceState()

  override def loadState(state: BlockVisualizerServiceState): Unit = { }

  override def dispose(): Unit = { }
}
