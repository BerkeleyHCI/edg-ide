package edg_ide.ui

import com.intellij.openapi.project.Project
import edg.schema.schema
import edg.wir.DesignPath

import java.awt.event.MouseEvent


trait ToolInterface {
  // Returns the top-level visualization (focus / context) path
  def getFocus: DesignPath
  def getProject: Project
  def getDesign: schema.Design

  // Starts a new tool
  def startNewTool(tool: BaseTool): Unit
  // Ends this tool, and returns to the default tool.
  def endTool(): Unit

  // TODO should these have smarter error handling?
  // Sets the selected design tree element in the graph.
  def setDesignTreeSelection(path: Option[DesignPath]): Unit
  // Sets the selected elements in the graph
  def setGraphSelections(paths: Set[DesignPath]): Unit
  // Sets the highlighted items on the graph, or None to disable highlighting.
  def setGraphHighlights(paths: Option[Seq[DesignPath]]): Unit
  def setFocus(path: DesignPath): Unit
  def setDetailView(path: DesignPath): Unit
}


// Base class for things tools need to implement, as well as hooks to the parent
trait BaseTool {
  val interface: ToolInterface  // can be used to affect the visualizer

  //
  // These functions are called by the visualizer
  //
  // Initialization function that runs when the tool is made active. By default clears state.
  def init(): Unit = {
    interface.setDesignTreeSelection(None)
    interface.setGraphSelections(Set())
    interface.setGraphHighlights(None)
  }

  // Mouse event that is generated on any mouse event in either the design tree or graph layout
  def onPathMouse(e: MouseEvent, path: DesignPath): Unit = { }

  // Event that is generated when the tree selection changes.
  // IMPORTANT: this may be triggered from setting the design tree selection, and may infinite-loop.
  def onSelect(path: DesignPath): Unit = { }
}
