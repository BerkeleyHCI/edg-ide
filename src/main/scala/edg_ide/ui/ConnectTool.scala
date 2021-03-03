package edg_ide.ui

import edg.ExprBuilder.ValueExpr
import edg.elem.elem
import edg.expr.expr
import edg.ref.ref
import edg.wir.DesignPath


object ConnectToolAnalysis {
  /** Returns the link-side top-level port names and types of connected elements.
    * Local analysis only, does not guarantee these ultimately resolve to a block port (may be a dangling export).
    * Works on both fully expanded as well as non-expanded (with ALLOCATEs) designs.
    *
    * If linkName is invalid, returns empty.
    */
  def connectsToLink(block: elem.HierarchyBlock, linkName: String): Seq[(String, ref.LibraryPath)] = {
    val linkConnectedConstraints = block.constraints
        .mapValues(_.expr)
        .collect {  // filter for connected constraints only
          case (constrName, expr.ValueExpr.Expr.Connected(connected)) =>
            (constrName, (connected, connected.getBlockPort, connected.getLinkPort))
        } .collect {  // filter for link
      case pair @ (constrName, (connected, blockExpr, ValueExpr.Ref(linkRef)))
        if linkRef.nonEmpty && linkRef.head == linkName => pair
    }
    ???
  }
}


/** Tool for making connections from a port
  *
  */
class ConnectTool(val interface: ToolInterface, initialPortPath: DesignPath) extends BaseTool {
  override def init(): Unit = {
    interface.setDesignTreeSelection(None)
    interface.setGraphSelections(Set(initialPortPath))
    interface.setGraphHighlights(Some(Set(initialPortPath)))  // TODO all connectable
  }


}
