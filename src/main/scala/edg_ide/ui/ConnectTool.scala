package edg_ide.ui

import com.intellij.openapi.ui.{ComponentValidator, ValidationInfo}
import com.intellij.ui.scale.JBUIScale
import edg.ExprBuilder.ValueExpr
import edg.elem.elem
import edg.expr.expr
import edg.ref.ref
import edg.ref.ref.LocalStep
import edg.util.Errorable
import edg.wir.DesignPath
import edg_ide.EdgirUtils
import edg_ide.util.{exceptable, requireExcept}

import java.awt.Point
import java.awt.event.MouseEvent
import javax.swing.{JComponent, JEditorPane, SwingUtilities}
import collection.mutable


object ConnectToolAnalysis {
  /** Returns the link-side top-level port names and types of connected elements.
    * Local analysis only, does not guarantee these ultimately resolve to a block port (may be a dangling export).
    * Works on both fully expanded as well as non-expanded (with ALLOCATEs) designs.
    *
    * If linkName is invalid, returns empty.
    */
  def connectsToLink(blockPath: DesignPath, block: elem.HierarchyBlock,
                     linkName: String): Seq[(DesignPath, ref.LibraryPath)] = {
    val link = block.links.getOrElse(linkName, return Seq())
        .`type`.link.getOrElse(return Seq())

    block.constraints
        .mapValues(_.expr)
        .collect {  // filter for connected constraints only, and unpack
          case (constrName, expr.ValueExpr.Expr.Connected(connected)) =>
            (connected.getBlockPort.getRef, connected.getLinkPort.getRef)
        } .collect {  // filter for link
          case (blockRef, linkRef) if linkRef.steps.head.getName == linkName =>
            val linkPortName = linkRef.steps(1).getName
            val portType = link.ports(linkPortName).is match {
              case elem.PortLike.Is.Port(port) =>
                require(port.superclasses.length == 1)
                port.superclasses.head
              case elem.PortLike.Is.Bundle(port) =>
                require(port.superclasses.length == 1)
                port.superclasses.head
              case elem.PortLike.Is.Array(array) =>
                require(array.superclasses.length == 1)
                array.superclasses.head
              case isOther => throw new IllegalArgumentException(s"unexpected $isOther")
            }
            (blockPath ++ blockRef, portType)
        }.toSeq
  }

  def linkNameOfPort(blockPath: DesignPath, block: elem.HierarchyBlock, port: DesignPath): Option[String] = {
    val allLinks = block.constraints
        .mapValues(_.expr)
        .collect {  // filter for connected constraints only, and unpack
          case (constrName, expr.ValueExpr.Expr.Connected(connected)) =>
            (connected.getBlockPort.getRef, connected.getLinkPort.getRef)
        } .collect { // filter for link
          case (blockRef, linkRef) if blockPath ++ blockRef == port =>
            linkRef.steps.head.getName
        }
    require(allLinks.size <= 1)
    allLinks.headOption
  }
}


object ConnectTool {
  def apply(interface: ToolInterface, initialPortPath: DesignPath): Errorable[ConnectTool] = exceptable {
    val containingBlockPath = EdgirUtils.resolveDeepestBlock(initialPortPath, interface.getDesign)._1
    val focusPath = interface.getFocus
    requireExcept(containingBlockPath == focusPath || containingBlockPath.split._1 == focusPath,
      "port not reachable from focus")
      // TODO refactor

    val containingBlock = EdgirUtils.resolveExactBlock(focusPath, interface.getDesign).get
    // TODO exterior connect analysis and bridge analysis
    val linkNameOpt = ConnectToolAnalysis.linkNameOfPort(focusPath, containingBlock, initialPortPath)
    val linkConnects = linkNameOpt match {
      case Some(linkName) =>
        val connectTypes = ConnectToolAnalysis.connectsToLink(focusPath, containingBlock, linkName)
        // TODO use port type data
        connectTypes.map(_._1).toSet
      case None => Set(initialPortPath)
    }

    new ConnectTool(interface, focusPath, initialPortPath, linkConnects)
  }
}


/** Tool for making connections from a port
  */
class ConnectTool(val interface: ToolInterface, focusPath: DesignPath, initialPortPath: DesignPath,
                 linkConnects: Set[DesignPath],
                 ) extends BaseTool {
  private val selected = mutable.Set[DesignPath]()



  override def init(): Unit = {
    interface.setDesignTreeSelection(None)
    interface.setGraphSelections(linkConnects)
    interface.setGraphHighlights(Some(Set(focusPath)))  // TODO all connectable
    interface.setStatus(s"Connect to $initialPortPath")
  }

  override def onPathMouse(e: MouseEvent, path: DesignPath): Unit = {
    val resolved = EdgirUtils.resolveExact(path, interface.getDesign)

    if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 1) {
      resolved match {
        case Some(_: elem.Port | _: elem.Bundle | _: elem.PortArray) if path != initialPortPath=>
          if (selected.contains(path)) {  // toggle selection
            selected -= path
          } else {
            selected += path
          }
          interface.setGraphSelections(selected.toSet + initialPortPath)
        case _ =>

      }
    } else if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2) {
      if (selected.isEmpty) {
        interface.endTool()  // TODO do connect operation
      }
    }
  }
}
