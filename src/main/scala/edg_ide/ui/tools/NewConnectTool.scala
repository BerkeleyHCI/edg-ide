package edg_ide.ui.tools

import edg.EdgirUtils.SimpleLibraryPath
import edg.util.Errorable
import edg.wir.{DesignPath, LibraryConnectivityAnalysis}
import edg_ide.EdgirUtils
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption, ExceptSeq}
import edg_ide.util.{BlockConnectedAnalysis, ConnectBuilder, PortConnects, exceptable, requireExcept}
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
        connecteds.exists(_.connect.topPortRef == portRef)
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
  var selectedPorts = mutable.Set[DesignPath]() // individual ports selected by the user
  var currentConnectBuilder = baseConnectBuilder // corresponding to selectedPorts, may have more ports from net joins

  def updateSelected(): Unit = { // updates selected in graph and text
    interface.setStatus(name)

    // mark all current selections
    val connectedPortRefs = currentConnectBuilder.connected.map(_._1.connect.topPortRef)
    interface.setGraphSelections(connectedPortRefs.map(containingBlockPath ++ _).toSet)

    // try all connections to determine additional possible connects
    val connectablePorts = mutable.ArrayBuffer[DesignPath]()
    val connectableBlocks = mutable.ArrayBuffer[DesignPath]()
    analysis.connectedGroups.foreach { case (name, (connecteds, constrs)) =>
      currentConnectBuilder.append(connecteds) match {
        case Some(_) => connecteds.foreach { connected =>
            val blockName = connected.connect.topPortRef match {
              case Seq(blockName, portName) => connectableBlocks.append(containingBlockPath + blockName)
              case _ => None
            }
            connectablePorts.append(containingBlockPath ++ connected.connect.topPortRef)
          }
        case None => Seq()
      }
    }

    interface.setGraphHighlights(Some((Seq(containingBlockPath) ++ connectablePorts ++ connectableBlocks).toSet))
  }

  override def init(): Unit = {
    interface.setGraphSelections(Set())
    updateSelected()
  }

  override def onPathMouse(e: MouseEvent, path: DesignPath): Unit = {
    val resolved = EdgirUtils.resolveExact(path, interface.getDesign)

    if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 1) { // toggle selected port
      val currentSelectedPorts = currentConnectBuilder.connected.map(containingBlockPath ++ _._1.connect.topPortRef)
      resolved match {
        case Some(_: elem.Port | _: elem.Bundle | _: elem.PortArray) => // toggle port
          if (selectedPorts.contains(path)) { // toggle deselect
            selectedPorts.remove(path)
            // TODO UPDATE CONNECT BUILDER
            updateSelected()
          } else if (!currentSelectedPorts.contains(path)) { // toggle select
            selectedPorts.add(path)
            // TODO UPDATE CONNECT BUILDER
            updateSelected()
          } // otherwise unselectable port / block
        case _ => // ignored
      }
    } else if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2) { // double-click finish shortcut
      // TODO implement me
      interface.endTool()
    } else if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) {}
  }
}
