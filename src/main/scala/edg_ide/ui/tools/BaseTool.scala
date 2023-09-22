package edg_ide.ui.tools

import com.intellij.openapi.project.Project
import edg.wir.{DesignPath, Library}
import edgir.schema.schema

import java.awt.event.{KeyEvent, MouseEvent}

trait ToolInterface {
  // Returns the top-level visualization (focus / context) path
  def getFocus: DesignPath
  def getProject: Project
  def getLibrary: Library
  def getDesign: schema.Design

  // Starts a new tool
  def startNewTool(tool: BaseTool): Unit
  // Ends this tool, and returns to the default tool.
  def endTool(): Unit

  // TODO should these have smarter error handling?
  // Scrolls the graph so the selected elt is visible
  def scrollGraphToVisible(path: DesignPath): Unit
  // Sets the selected elements in the graph
  def setGraphSelections(paths: Set[DesignPath]): Unit
  // Sets the highlighted items on the graph, or None to disable highlighting.
  def setGraphHighlights(paths: Option[Set[DesignPath]]): Unit
  // Sets port insert items (draws the arrow)
  def setGraphPortInserts(paths: Set[DesignPath]): Unit
  def resetGraphTransientSelections(): Unit

  // Sets the selected design tree and detail view element
  def setSelection(path: DesignPath): Unit
  def setFocus(path: DesignPath): Unit
  def setStatus(status: String): Unit
}

// Base class for things tools need to implement, as well as hooks to the parent
trait BaseTool {
  val interface: ToolInterface // can be used to affect the visualizer

  //
  // These functions are called by the visualizer
  //
  // Initialization function that runs when the tool is made active. By default clears state.
  def init(): Unit = {
    interface.resetGraphTransientSelections()
  }

  // Mouse event that is generated on any mouse event in either the design tree or graph layout
  def onPathMouse(e: MouseEvent, path: DesignPath): Unit = {}

  // Keyboard event generated on any key even in the graph layout
  def onKeyPress(e: KeyEvent): Unit = {}
}
