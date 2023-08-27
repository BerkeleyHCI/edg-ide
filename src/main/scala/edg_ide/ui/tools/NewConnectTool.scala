package edg_ide.ui.tools

import edg.util.Errorable
import edg.wir.{DesignPath, LibraryConnectivityAnalysis}
import edg_ide.EdgirUtils
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption, ExceptSeq}
import edg_ide.util.{BlockConnectedAnalysis, ConnectBuilder, ConnectTypes, exceptable, requireExcept}
import edgir.elem.elem

import java.awt.event.MouseEvent
import javax.swing.SwingUtilities
import scala.collection.mutable

object NewConnectTool {
  def apply(interface: ToolInterface, portPath: DesignPath): Errorable[NewConnectTool] = exceptable {
    val focusPath = interface.getFocus
    val focusBlock = EdgirUtils
      .resolveExact(focusPath, interface.getDesign)
      .exceptNone("can't reach focus block")
      .instanceOfExcept[elem.HierarchyBlock]("focus block not a block")

    val portLink = {
      val port = EdgirUtils.resolveExact(portPath, interface.getDesign).exceptNone("no port")
      val portType = port match {
        case port: elem.Port => port.getSelfClass
        case port: elem.Bundle => port.getSelfClass
        case array: elem.PortArray => array.getSelfClass
        case _ => exceptable.fail("invalid port type")
      }
      val libraryAnalysis = new LibraryConnectivityAnalysis(interface.getLibrary) // TODO save and reuse?
      val linkType = libraryAnalysis.linkOfPort(portType).exceptNone("no link type for port")
      interface.getLibrary.getLink(linkType).exceptError
    }

    val portRef = { // get selected port as Seq(...) reference
      val (containingBlockPath, containingBlock) = EdgirUtils.resolveDeepestBlock(portPath, interface.getDesign)
      val portRef = portPath.postfixFromOption(containingBlockPath).exceptNone("port not in focus block")
      val portName = portRef.steps.headOption.exceptNone("port path empty").getName

      if (containingBlockPath == focusPath) { // boundary port
        Seq(portName)
      } else { // block port
        val (blockParent, blockName) = containingBlockPath.split
        requireExcept(blockParent == focusPath, "port not in focus block")
        Seq(blockName, portName)
      }
    }

    val analysis = new BlockConnectedAnalysis(focusBlock)
    val (portConnectName, (_, portConstrs)) =
      analysis.connectedGroups.toSeq.find { case (name, (connecteds, constrs)) =>
        connecteds.exists(_.topPortRef == portRef)
      }.exceptNone("no connection")
    val connectBuilder = ConnectBuilder(focusBlock, portLink, portConstrs)
      .exceptNone("invalid connections to port")

    new NewConnectTool(interface, portConnectName, focusPath, connectBuilder, analysis)
  }
}

class NewConnectTool(
    val interface: ToolInterface,
    name: String,
    containingBlockPath: DesignPath,
    baseConnectBuilder: ConnectBuilder,
    analysis: BlockConnectedAnalysis
) extends BaseTool {
  var selectedPorts = mutable.Set[Seq[String]]()
  var currentConnectBuilder = baseConnectBuilder // corresponding to selectedPorts

  def updateSelected(): Unit = { // updates selected in graph and text
    interface.setStatus(name)
//    interface.setGraphSelections(priorConnect.getPorts.map(focusPath ++ _).toSet + portPath ++ selected.toSet)
//    val availablePaths = connectsAvailable()
//    val availableBlockPaths = availablePaths.map(_.split._1)
//    interface.setGraphHighlights(Some(Set(focusPath) ++ availablePaths ++ availableBlockPaths))

    val connectedPortRefs = currentConnectBuilder.connected.map(_._1.topPortRef)
    interface.setGraphSelections(connectedPortRefs.map(containingBlockPath ++ _).toSet)
  }

  override def init(): Unit = {
    interface.setGraphSelections(Set())
    updateSelected()
  }

  override def onPathMouse(e: MouseEvent, path: DesignPath): Unit = {
    val resolved = EdgirUtils.resolveExact(path, interface.getDesign)

    if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 1) { // toggle selected port
    } else if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2) { // double-click finish shortcut

    } else if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) {}
  }
}
