package edg_ide.ui.tools

import com.intellij.openapi.diagnostic.Logger
import com.intellij.psi.PsiElement
import edg.util.Errorable
import edg.wir.{DesignPath, LibraryConnectivityAnalysis}
import edg_ide.EdgirUtils
import edg_ide.psi_edits.LiveTemplateConnect
import edg_ide.ui.{BlockVisualizerService, PopupUtils}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption, ExceptSeq}
import edg_ide.util._
import edgir.elem.elem

import java.awt.Component
import java.awt.event.{KeyEvent, MouseEvent}
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
    val (portConnectName, portConnecteds, portConstrs) = {
      // TODO findLast is a hack to prioritize the new vector slice (as opposed to prior connected ones)
      analysis.connectedGroups.findLast { case (linkNameOpt, connecteds, constrs) =>
        connecteds.exists(_.connect.topPortRef == portRef)
      }.exceptNone("no connection")
    }
    val portConnected = portConnecteds.filter(_.connect.topPortRef == portRef)
      .onlyExcept("multiple connections")
    var connectBuilder = ConnectBuilder(focusBlock, portLink, portConstrs)
      .exceptNone("invalid connections to port")

    if (portConstrs.isEmpty) { // if no constraints (no prior link) start with the port itself
      connectBuilder = connectBuilder.append(Seq(portConnected))
        .exceptNone("invalid connections to port")
    }

    new NewConnectTool(interface, focusPath, portConnected, connectBuilder, analysis)
  }
}

class NewConnectTool(
    val interface: ToolInterface,
    containingBlockPath: DesignPath,
    startingPort: PortConnectTyped[PortConnects.Base],
    baseConnectBuilder: ConnectBuilder, // including startingPort, even if it's the only item (new link)
    analysis: BlockConnectedAnalysis
) extends BaseTool {
  private val logger = Logger.getInstance(this.getClass)

  val startingPortPath = containingBlockPath ++ startingPort.connect.topPortRef

  // individual ports selected by the user
  var selectedConnects = mutable.ArrayBuffer[PortConnectTyped[PortConnects.Base]]()
  // corresponding to selectedPorts, may have more ports from net joins
  var currentConnectBuilder = baseConnectBuilder

  def getCurrentName(): String = {
    if (selectedConnects.nonEmpty) {
      val connectedPortNames = selectedConnects.map(_.connect.topPortRef.mkString("."))
      s"Connect ${connectedPortNames.mkString(", ")} to ${startingPort.connect.topPortRef.mkString(".")}"
    } else {
      s"Connect to ${startingPort.connect.topPortRef.mkString(".")}"
    }
  }

  def updateSelected(): Unit = { // updates selected in graph and text
    if (selectedConnects.isEmpty) {
      interface.setStatus("[Esc] cancel;" + getCurrentName())
    } else {
      interface.setStatus("[Esc] cancel; [Enter/DblClick] complete; " + getCurrentName())
    }

    // mark all current selections
    val connectedPorts = currentConnectBuilder.connected.map(containingBlockPath ++ _._1.connect.topPortRef)
    interface.setGraphSelections(connectedPorts.toSet)

    // try all connections to determine additional possible connects
    // note, vector slices may overlap and appear in multiple connect groups (and a new connection),
    // but in this case it doesn't matter since this takes the most available port for marking highlights
    val connectablePorts = mutable.ArrayBuffer[DesignPath]()
    val connectableBlocks = mutable.ArrayBuffer[DesignPath]()
    analysis.connectedGroups.foreach { case (linkNameOpt, connecteds, constrs) =>
      currentConnectBuilder.append(connecteds) match {
        case Some(_) => connecteds.foreach { connected =>
            connected.connect.topPortRef match { // add containing block, if a block port
              case Seq(blockName, portName) => connectableBlocks.append(containingBlockPath + blockName)
              case _ => // ignored
            }
            connectablePorts.append(containingBlockPath ++ connected.connect.topPortRef)
          }
        case None => Seq()
      }
    }

    // enable selection of existing ports in connection (toggle-able) and connect-able ports
    interface.setGraphHighlights(
      Some((Seq(containingBlockPath) ++ connectedPorts ++ connectablePorts ++ connectableBlocks).toSet)
    )
  }

  override def init(): Unit = {
    updateSelected()
  }

  def removeConnect(portPath: DesignPath): Unit = {
    // remove all connections to the port path (should really only be one)
    selectedConnects.filterInPlace(containingBlockPath ++ _.connect.topPortRef != portPath)
    val selectedConnectConnects = selectedConnects.map(_.connect)
    // recompute from scratch on removal, for simplicity
    val allConnected = analysis.connectedGroups.filter { case (linkNameOpt, connecteds, constrs) =>
      connecteds.exists(connected =>
        selectedConnectConnects.contains(connected.connect) &&
          !((connected.connect.isInstanceOf[PortConnects.BlockVectorSlicePort] ||
            connected.connect.isInstanceOf[PortConnects.BlockVectorSliceVector]) &&
            connecteds.length > 1) // only keep individual BlockVectorSlice, TODO should not be hardcoded
      )
    }.flatMap(_._2)
    // update state
    baseConnectBuilder.append(allConnected) match {
      case Some(newConnectBuilder) =>
        currentConnectBuilder = newConnectBuilder
      case None => // if the connect is invalid (shouldn't be possible), revert to the empty connect
        logger.error(s"invalid connect from removal of $portPath")
        currentConnectBuilder = baseConnectBuilder
        selectedConnects.clear()
    }
    updateSelected()
  }

  def addConnect(portPath: DesignPath): Unit = {
    val newConnectedNet = analysis.connectedGroups.filter { case (linkNameOpt, connecteds, constrs) =>
      connecteds.exists(connected =>
        containingBlockPath ++ connected.connect.topPortRef == portPath &&
          !((connected.connect.isInstanceOf[PortConnects.BlockVectorSlicePort] ||
            connected.connect.isInstanceOf[PortConnects.BlockVectorSliceVector]) &&
            connecteds.length > 1) // only keep individual BlockVectorSlice, TODO should not be hardcoded
      )
    }.flatMap(_._2)
    val newConnected = // get single connected of this port
      newConnectedNet.filter(containingBlockPath ++ _.connect.topPortRef == portPath)
    val newConnectBuilder = currentConnectBuilder.append(newConnectedNet)
    (newConnected, newConnectBuilder) match {
      case (Seq(newConnected), Some(newConnectBuilder)) => // valid connect, commit and update UI
        selectedConnects.append(newConnected)
        currentConnectBuilder = newConnectBuilder
        updateSelected()
      case _ =>
        logger.warn(s"invalid connect from added port $portPath") // invalid connect, ignore
    }
  }

  protected def completeConnect(component: Component): Unit = {
    if (selectedConnects.nonEmpty) {
      val newConnects = selectedConnects.toSeq
      val connectedBlockOpt =
        EdgirConnectExecutor(
          analysis.block,
          analysis,
          currentConnectBuilder,
          startingPort +: newConnects
        )
      val containerPyClassOpt = DesignAnalysisUtils.pyClassOf(analysis.block.getSelfClass, interface.getProject)

      (connectedBlockOpt, containerPyClassOpt.toOption) match {
        case (Some(connectedBlock), Some(containerPyClass)) =>
          val continuation = (name: Option[String], inserted: PsiElement) => {
            BlockVisualizerService(interface.getProject).visualizerPanelOption.foreach {
              _.currentDesignModifyBlock(containingBlockPath)(_ => connectedBlock)
            }
            interface.endTool()
          }

          exceptionPopup.atMouse(component) {
            val movableLiveTemplate = LiveTemplateConnect.createTemplateConnect(
              containerPyClass,
              startingPort.connect +: newConnects.map(_.connect),
              getCurrentName(),
              continuation
            )

            movableLiveTemplate.start(interface.getProject).exceptError
          }
        case _ =>
          if (connectedBlockOpt.isEmpty) {
            logger.error(s"failed to create connected IR block")
          }
          containerPyClassOpt match {
            case Errorable.Error(msg) => logger.error(s"failed to get container pyclass: $msg")
            case _ => // ignored
          }
          PopupUtils.createErrorPopupAtMouse(s"internal error", component)
          interface.endTool()
      }
    } else { // nothing to do, cancel
      interface.endTool()
    }
  }

  override def onPathMouse(e: MouseEvent, path: DesignPath): Unit = {
    val resolved = EdgirUtils.resolveExact(path, interface.getDesign)

    if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 1) { // toggle selected port
      val currentSelectedPorts = currentConnectBuilder.connected.map(containingBlockPath ++ _._1.connect.topPortRef)
      resolved match {
        case Some(_: elem.Port | _: elem.Bundle | _: elem.PortArray) => // toggle port
          if (selectedConnects.exists(containingBlockPath ++ _.connect.topPortRef == path)) { // toggle de-select
            removeConnect(path)
          } else if (!currentSelectedPorts.contains(path) && path != startingPortPath) { // toggle select
            addConnect(path)
          } // otherwise unselectable port / block
        case _ => // ignored
      }
    } else if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2) { // double-click finish shortcut
      completeConnect(e.getComponent)
    }
  }

  override def onKeyPress(e: KeyEvent): Unit = {
    if (e.getKeyCode == KeyEvent.VK_ESCAPE) {
      interface.endTool()
      e.consume()
    } else if (e.getKeyCode == KeyEvent.VK_ENTER) {
      completeConnect(e.getComponent)
      e.consume()
    }
  }
}
