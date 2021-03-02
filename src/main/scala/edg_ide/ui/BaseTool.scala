package edg_ide.ui

import com.intellij.openapi.project.Project
import edg.wir.DesignPath

import java.awt.event.MouseEvent


trait ToolInterface {
  // Starts a new tool
  def startNewTool(tool: BaseTool): Unit
  // Ends this tool, and returns to the default tool.
  def endTool(): Unit

  // TODO should these have smarter error handling?
  // Sets the selected design tree element in the graph.
  def setDesignTreePath(path: DesignPath): Unit
  // Sets the selected elements in the graph
  def setGraphSelections(paths: DesignPath): Unit
  // Sets the highlighted items on the graph, or None to disable highlighting.
  def setGraphHighlights(paths: Option[Seq[DesignPath]]): Unit
}


// Base class for things tools need to implement, as well as hooks to the parent
trait BaseTool {
  val project: Project
  val interface: ToolInterface  // can be used to affect the visualizer

  //
  // These functions are called by the visualizer
  //

  // Mouse event that is generated on any mouse event in either the design tree or graph layout
  def onBlockMouse(e: MouseEvent, path: DesignPath): Unit = { }

  // Event that is generated when the tree selection changes.
  def onSelect(path: DesignPath): Unit = { }
}
