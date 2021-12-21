package edg_ide.ui

import com.intellij.openapi.Disposable
import com.intellij.openapi.components.PersistentStateComponent
import com.intellij.openapi.project.Project
import com.intellij.openapi.wm.ToolWindow

import edgir.schema.schema
import edgir.elem.elem
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

  def setModuleClass(module: String, block: String): Unit = {
    visualizerPanelOption.foreach(_.setFileBlock(module, block))
    update()
  }

  def update(): Unit = {
    visualizerPanelOption.foreach(_.recompile())
  }

  def getModule(): String = {
    visualizerPanelOption.get.getModule
  }

  def getContextBlock: Option[(DesignPath, elem.HierarchyBlock)] = {
    visualizerPanelOption.flatMap(_.getContextBlock)
  }

  def getDesign: Option[schema.Design] = {
    visualizerPanelOption.map(_.getDesign)
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
