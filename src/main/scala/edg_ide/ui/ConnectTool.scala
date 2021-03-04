package edg_ide.ui

import edg.elem.elem
import edg.expr.expr
import edg.ref.ref
import edg.util.Errorable
import edg.wir.{DesignPath, LibraryConnectivityAnalysis}
import edg_ide.EdgirUtils
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption, ExceptSeq}
import edg_ide.util.{ExceptionNotifyException, exceptable, requireExcept}

import java.awt.event.MouseEvent
import javax.swing.SwingUtilities
import collection.mutable


object ConnectToolAnalysis {
  // TODO this is awful, replace with union types when possible!
  // TODO goes in some shared analysis util?
  def typeOfPort(port: Any): Errorable[ref.LibraryPath] = exceptable { port match {
    case portLike: elem.PortLike => portLike.is match {
      case elem.PortLike.Is.Port(port) => typeOfPort(port).exceptError
      case elem.PortLike.Is.Bundle(port) => typeOfPort(port).exceptError
      case elem.PortLike.Is.Array(array) => typeOfPort(array).exceptError
    }
    case port: elem.Port =>
      port.superclasses.onlyExcept("invalid port class")
    case port: elem.Bundle =>
      port.superclasses.onlyExcept("invalid port class")
    case array: elem.PortArray =>
      array.superclasses.onlyExcept("invalid port class")
    case isOther => throw ExceptionNotifyException(s"unexpected $isOther")
  }}

  /** Returns all connected ports in a block, both ports of internal blocks and bounary exports
    */
  def connectsInBlock(blockPath: DesignPath, block: elem.HierarchyBlock): Seq[DesignPath] = {
    block.constraints
        .values
        .map(_.expr)
        .collect {  // filter for connected constraints only, and unpack
          case expr.ValueExpr.Expr.Connected(connected) =>
            Seq(blockPath ++ connected.getBlockPort.getRef)
          case expr.ValueExpr.Expr.Exported(exported) =>
            Seq(blockPath ++ exported.getExteriorPort.getRef,
              blockPath ++exported.getInternalBlockPort.getRef)
        }.flatten.toSeq
  }

  /** Returns the block-side top-level port names and types of connected elements.
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
        .values
        .map(_.expr)
        .collect {  // filter for connected constraints only, and unpack
          case expr.ValueExpr.Expr.Connected(connected) =>
            (connected.getBlockPort.getRef, connected.getLinkPort.getRef)
        } .collect {  // filter for link
          case (blockRef, linkRef) if linkRef.steps.head.getName == linkName =>
            val linkPortName = linkRef.steps(1).getName
            (blockPath ++ blockRef, typeOfPort(link.ports(linkPortName)).get)
        }.toSeq
  }

  /** For a set of block-side ports (of blocks in block), of those that are connected to a PortBridge,
    * returns all the connected exported ports with the associated link-side port type.
    */
  def getExported(blockPath: DesignPath, block: elem.HierarchyBlock,
                  portPathTypes: Seq[(DesignPath, ref.LibraryPath)]): Seq[(DesignPath, ref.LibraryPath)] = {
    val exportMap = block.constraints
        .values
        .map(_.expr)
        .collect {  // filter for exported only, into (inner block port path, exterior port path) pairs
          case expr.ValueExpr.Expr.Exported(exported) =>
            (blockPath ++ exported.getInternalBlockPort.getRef,
                blockPath ++ exported.getExteriorPort.getRef)
        }.groupBy(_._1)  // group by inner block
        .mapValues { pairs =>  // extract only exterior port path from value pairs seq
          require(pairs.size == 1)
          pairs.head._2
        }

    // TODO more graceful error handling?
    portPathTypes.flatMap { case (portPath, portType) =>
      val (portBlockPath, portName) = portPath.split
      val (portBlockContainerPath, blockName) = portBlockPath.split
      require(portBlockContainerPath == blockPath)
      // TODO a superclass check would be better!
      if (portName == LibraryConnectivityAnalysis.portBridgeLinkPort) {
        val exportedPortPath = exportMap(portBlockPath + LibraryConnectivityAnalysis.portBridgeOuterPort)
        Some((exportedPortPath, portType))
      } else {
        None
      }
    }
  }

  def linkNameOfPort(blockPath: DesignPath, block: elem.HierarchyBlock, port: DesignPath): Option[String] = {
    val allLinks = block.constraints
        .values
        .map(_.expr)
        .collect {  // filter for connected constraints only, and unpack
          case expr.ValueExpr.Expr.Connected(connected) =>
            (connected.getBlockPort.getRef, connected.getLinkPort.getRef)
        } .collect { // filter for link
          case (blockRef, linkRef) if blockPath ++ blockRef == port =>
            linkRef.steps.head.getName
        }
    require(allLinks.size <= 1)
    allLinks.headOption
  }

  // Returns port path -> port type
  def topPortsOfBlock(blockPath: DesignPath, block: elem.HierarchyBlock): Seq[(DesignPath, ref.LibraryPath)] = {
    block.ports.toSeq.map { case (portName, portLike) =>
      val portType = typeOfPort(portLike).get
      (blockPath + portName, portType)
    }
  }
}


object ConnectTool {
  def apply(interface: ToolInterface, initialPortPath: DesignPath): Errorable[ConnectTool] = exceptable {
    val libraryAnalysis = new LibraryConnectivityAnalysis(interface.getLibrary)  // TODO this should be saved & reused!
    val port = EdgirUtils.resolveExact(initialPortPath, interface.getDesign).exceptNone("no port")

    val containingBlockPath = EdgirUtils.resolveDeepestBlock(initialPortPath, interface.getDesign)._1
    val focusPath = interface.getFocus
    val focusBlock = EdgirUtils.resolveExact(focusPath, interface.getDesign)
        .exceptNone("can't reach focus block")
        .instanceOfExcept[elem.HierarchyBlock]("focus block not a block")
    val isExteriorPort = containingBlockPath == focusPath  // a boundary port that may need to be bridged
    requireExcept(isExteriorPort || containingBlockPath.split._1 == focusPath,
      "port not reachable from focus")
      // TODO refactor

    // TODO exterior connect analysis and bridge analysis
    val linkNameOpt = ConnectToolAnalysis.linkNameOfPort(focusPath, focusBlock, initialPortPath)
    val (linkType, priorConnects) = linkNameOpt match {
      case Some(linkName) =>
        val link = EdgirUtils.resolveExact(focusPath + linkName, interface.getDesign)
            .exceptNone("can't find connected link")
            .instanceOfExcept[elem.Link]("invalid connected link")
        val linkType = link.superclasses.onlyExcept("invalid connected link type")
        val priorDirectConnects = ConnectToolAnalysis.connectsToLink(focusPath, focusBlock, linkName)
        val priorConnects = priorDirectConnects ++
            ConnectToolAnalysis.getExported(focusPath, focusBlock, priorDirectConnects)
        // Use the link type from the link itself
        (linkType, priorConnects)
      case None =>
        val portType = ConnectToolAnalysis.typeOfPort(port).exceptError
        val linkType = libraryAnalysis.linkOfPort(portType).exceptNone("can't find link of port")
        (linkType, Seq())
    }
    val linkAvailable = libraryAnalysis.connectablePorts(linkType)
    val linkConnectableTypes = linkAvailable.keySet
    val alreadyConnected = ConnectToolAnalysis.connectsInBlock(focusPath, focusBlock)

    val boundaryPorts = ConnectToolAnalysis.topPortsOfBlock(focusPath, focusBlock)
        .filter { case (portPath, portType) =>
          libraryAnalysis.bridgedPort(portType) match {
            case Some(bridgedPortType) =>
              linkConnectableTypes.contains(bridgedPortType) && !alreadyConnected.contains(portPath)
            case None => false
          }
        }
    val internalPorts = focusBlock.blocks.toSeq.flatMap { case (intBlockName, intBlock) =>
      ConnectToolAnalysis.topPortsOfBlock(focusPath + intBlockName, intBlock.getHierarchy)
    }.filter { case (portPath, portType) =>
      linkConnectableTypes.contains(portType) && !alreadyConnected.contains(portPath)
    }

    new ConnectTool(interface, focusPath, initialPortPath,
      priorConnects, linkAvailable, boundaryPorts ++ internalPorts, Seq())  // TODO implement available exports
  }
}


/** Tool for making connections from a port
  */
class ConnectTool(val interface: ToolInterface, focusPath: DesignPath, initialPortPath: DesignPath,
                  priorConnects: Seq[(DesignPath, ref.LibraryPath)], linkAvailable: Map[ref.LibraryPath, Int],
                  availableTargets: Seq[(DesignPath, ref.LibraryPath)], availableExports: Seq[DesignPath]
                 ) extends BaseTool {
  private val selected = mutable.Set[DesignPath]()

  private val linkConnectPaths = priorConnects.map(_._1).toSet
  private val availableTargetBlocks = availableTargets.map(_._1.split._1)

  override def init(): Unit = {
    interface.setDesignTreeSelection(None)
    interface.setGraphSelections(linkConnectPaths + initialPortPath)
    // TODO filter by selected
    interface.setGraphHighlights(Some(Set(focusPath) ++ availableTargetBlocks ++ availableTargets.map(_._1)))  // TODO all connectable
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
          interface.setGraphSelections(selected.toSet ++ linkConnectPaths + initialPortPath)
        case _ =>

      }
    } else if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2) {
      if (selected.isEmpty) {
        interface.endTool()  // TODO do connect operation
      }
    }
  }
}
