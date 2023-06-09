package edg_ide.ui

import com.intellij.openapi.Disposable
import com.intellij.openapi.components.PersistentStateComponent
import com.intellij.openapi.project.Project
import com.intellij.openapi.wm.ToolWindow
import edg.compiler.{Compiler, CompilerError, PythonInterfaceLibrary}
import edg.wir.DesignPath
import edg_ide.proven.{ProvenDataReader, ProvenDatabase}
import edgir.elem.elem
import edgir.schema.schema
import edgrpc.hdl.{hdl => edgrpc}

import java.io.File

// Note: the implementation is here, but the actual service in plugin.xml is a Java class,
// because IntelliJ doesn't seem to like the Scala class.
object BlockVisualizerService {
  def apply(project: Project): BlockVisualizerService = {
    project.getService(classOf[BlockVisualizerServiceWrapper]).asInstanceOf[BlockVisualizerService]
  }
}

class BlockVisualizerService(project: Project)
    extends PersistentStateComponent[BlockVisualizerServiceState]
    with Disposable {
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

  def setDesignTop(
      design: schema.Design,
      compiler: Compiler,
      refinements: edgrpc.Refinements,
      errors: Seq[CompilerError],
      namePrefix: Option[String] = None
  ): Unit = {
    visualizerPanelOption.foreach(_.setDesignTop(design, compiler, refinements, errors, namePrefix))
  }

  def setDesignStale(): Unit = {
    visualizerPanelOption.foreach(_.setDesignStale())
  }

  def getDesign: Option[schema.Design] = {
    visualizerPanelOption.map(_.getDesign)
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

  override def dispose(): Unit = {}
}
