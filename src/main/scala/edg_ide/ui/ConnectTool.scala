package edg_ide.ui

import com.intellij.openapi.ui.{ComponentValidator, ValidationInfo}
import edg.ExprBuilder.ValueExpr
import edg.elem.elem
import edg.expr.expr
import edg.ref.ref
import edg.wir.DesignPath
import edg_ide.EdgirUtils

import java.awt.Point
import java.awt.event.MouseEvent
import javax.swing.{JComponent, SwingUtilities}
import collection.mutable


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
  */
class ConnectTool(val interface: ToolInterface, initialPortPath: DesignPath) extends BaseTool {
  val selected = mutable.Set[DesignPath]()

  val containingBlockPath = EdgirUtils.resolveDeepestBlock(initialPortPath,
    interface.getDesign.contents.getOrElse(elem.HierarchyBlock())).get._1

  override def init(): Unit = {
    interface.setDesignTreeSelection(None)
    interface.setGraphSelections(Set())
    interface.setGraphHighlights(Some(Set(containingBlockPath, initialPortPath)))  // TODO all connectable
    interface.setStatus(s"Connect to $initialPortPath")
  }

  override def onPathMouse(e: MouseEvent, path: DesignPath): Unit = {
    val resolved = EdgirUtils.resolveExact(path, interface.getDesign.contents.getOrElse(elem.HierarchyBlock()))

    if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 1) {
      resolved match {
        case Some(_: elem.Port | _: elem.Bundle | _: elem.PortArray) if path != initialPortPath=>
          if (selected.contains(path)) {  // toggle selection
            selected -= path
          } else {
            selected += path
          }
          interface.setGraphSelections(selected.toSet)
        case _ =>
          val popupBuilder = ComponentValidator.createPopupBuilder(new ValidationInfo("ERROR", e.getComponent.asInstanceOf[JComponent]), null)
              .setCancelOnWindowDeactivation(false)
              .setCancelOnClickOutside(true)
              .addUserData("SIMPLE_WINDOW")

          val myErrorPopup = popupBuilder.createPopup
          myErrorPopup.showInScreenCoordinates(e.getComponent, new Point(e.getXOnScreen, e.getYOnScreen))
      }
    } else if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2) {
      if (selected.isEmpty) {
        interface.endTool()  // TODO do connect operation
      }
    }
  }
}
