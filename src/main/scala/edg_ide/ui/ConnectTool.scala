package edg_ide.ui

import com.intellij.psi.PsiElement
import edg.elem.elem
import edg.expr.expr
import edg.ref.ref
import edg.util.Errorable
import edg.wir.{DesignPath, LibraryConnectivityAnalysis}
import edg_ide.EdgirUtils
import edg_ide.actions.{InsertAction, InsertBlockAction, InsertConnectAction}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption, ExceptSeq}
import edg_ide.util.{DesignAnalysisUtils, ExceptionNotifyException, exceptable, exceptionPopup, requireExcept}

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
      case isOther => throw ExceptionNotifyException(s"unexpected port ${isOther.getClass}")
    }
    case port: elem.Port =>
      port.superclasses.onlyExcept("invalid port class")
    case port: elem.Bundle =>
      port.superclasses.onlyExcept("invalid port class")
    case array: elem.PortArray =>
      array.superclasses.onlyExcept("invalid port class")
    case isOther => throw ExceptionNotifyException(s"unexpected port ${isOther.getClass}")
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

  /** Returns the block-side top-level port paths of connected elements.
    * Local analysis only, does not guarantee these ultimately resolve to a block port (may be a dangling export).
    * Works on both fully expanded as well as non-expanded (with ALLOCATEs) designs.
    *
    * If linkName is invalid, returns empty.
    */
  def connectsToLink(blockPath: DesignPath, block: elem.HierarchyBlock,
                     linkName: String): Seq[DesignPath] = {
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
            blockPath ++ blockRef
        }.toSeq
  }

  /** Given a block, return a map of all exports as inner block port path -> exterior port path
    */
  def blockExportMap(blockPath: DesignPath, block: elem.HierarchyBlock): Map[DesignPath, DesignPath] = {
    block.constraints
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
        }.toMap
  }

  /** For a set of block-side ports (of blocks in block), of those that are connected to a PortBridge,
    * returns all the connected exported ports.
    */
  def getExported(blockPath: DesignPath, block: elem.HierarchyBlock,
                  portPaths: Seq[DesignPath]): Seq[DesignPath] = {
    val exportMap = blockExportMap(blockPath, block)

    // TODO more graceful error handling?
    portPaths.flatMap { portPath =>
      val (portBlockPath, portName) = portPath.split
      val (portBlockContainerPath, blockName) = portBlockPath.split
      require(portBlockContainerPath == blockPath)
      // TODO a superclass check would be better!
      if (portName == LibraryConnectivityAnalysis.portBridgeLinkPort) {
        val exportedPortPath = exportMap(portBlockPath + LibraryConnectivityAnalysis.portBridgeOuterPort)
        Some(exportedPortPath)
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
  def apply(interface: ToolInterface, portPath: DesignPath): Errorable[ConnectTool] = exceptable {
    val libraryAnalysis = new LibraryConnectivityAnalysis(interface.getLibrary)  // TODO this should be saved & reused!
    val port = EdgirUtils.resolveExact(portPath, interface.getDesign).exceptNone("no port")
    val portType = ConnectToolAnalysis.typeOfPort(port).exceptError

    val containingBlockPath = EdgirUtils.resolveDeepestBlock(portPath, interface.getDesign)._1
    val focusPath = interface.getFocus
    val focusBlock = EdgirUtils.resolveExact(focusPath, interface.getDesign)
        .exceptNone("can't reach focus block")
        .instanceOfExcept[elem.HierarchyBlock]("focus block not a block")
    val isExteriorPort = containingBlockPath == focusPath  // a boundary port that may need to be bridged
    requireExcept(isExteriorPort || containingBlockPath.split._1 == focusPath,
      "port not reachable from focus")
      // TODO refactor

    val bridgedPortTypeOption = libraryAnalysis.bridgedPortByOuter(portType)

    if (isExteriorPort && bridgedPortTypeOption.isEmpty) {  // exterior port with no bridge, no link available
      val exportablePorts = focusBlock.blocks.toSeq.flatMap { case (intBlockName, intBlock) =>  // get all ports
        ConnectToolAnalysis.topPortsOfBlock(focusPath + intBlockName, intBlock.getHierarchy)
      }.filter { case (innerPortPath, innerPortType) =>  // filter by same type
        innerPortType == portType
      }.map(_._1)  // don't need type data

      // TODO dedup w/ link-exists case below?
      val exportMap = ConnectToolAnalysis.blockExportMap(focusPath, focusBlock)
      val reverseExportMap = exportMap.map { case (k, v) => v -> k }
      val priorConnects = if (exportMap.contains(portPath)) {  // TODO can this be cleaner with a match op?
        Seq(portPath, exportMap(portPath))  // exterior port
      } else if (reverseExportMap.contains(portPath)) {
        Seq(portPath, reverseExportMap(portPath))  // interior port
      } else {
        Seq(portPath)
      }

      new ConnectTool(interface, focusPath, portPath,
        priorConnects, Map(), Seq(), exportablePorts,
        s"Export to $portPath"
      )
      // Note: case of interior port with no bridge and no link doesn't make sense
    } else { // either interior port, or bridged exterior port
      val (linkPortType, linkNameOpt) = if (isExteriorPort) {  // port type as seen by the link, correcting for bridges
        // also need to resolve the link side port across an export and bridge
        val exportedByExterior = ConnectToolAnalysis.blockExportMap(focusPath, focusBlock)
            .map { case (k, v) => v -> k }
        val linkNameOpt = exportedByExterior.get(portPath).flatMap { outerPortPath =>
          val (bridgeBlockPath, _) = outerPortPath.split
          val linkPortPath = bridgeBlockPath + LibraryConnectivityAnalysis.portBridgeLinkPort
          ConnectToolAnalysis.linkNameOfPort(focusPath, focusBlock, linkPortPath)
        }
        (bridgedPortTypeOption.get, linkNameOpt)
      } else {
        (portType, ConnectToolAnalysis.linkNameOfPort(focusPath, focusBlock, portPath))
      }

      val (linkType, priorConnectPaths, connectName) = linkNameOpt match {
        case Some(linkName) =>  // prior existing link
          val link = EdgirUtils.resolveExact(focusPath + linkName, interface.getDesign)
              .exceptNone("can't find connected link")
              .instanceOfExcept[elem.Link]("invalid connected link")
          val linkType = link.superclasses.onlyExcept("invalid connected link type")
          val priorDirectConnects = ConnectToolAnalysis.connectsToLink(focusPath, focusBlock, linkName)
          val priorConnects = priorDirectConnects ++
              ConnectToolAnalysis.getExported(focusPath, focusBlock, priorDirectConnects)
          // Use the link type from the link itself
          (linkType, priorConnects,
              s"Connect to $portPath by appending to ${EdgirUtils.SimpleLibraryPath(linkType)} at $linkName")
        case None =>  // no existing link: new port or direct export
          val exportMap = ConnectToolAnalysis.blockExportMap(focusPath, focusBlock)
          val reverseExportMap = exportMap.map { case (k, v) => v -> k }
          val priorConnects = if (exportMap.contains(portPath)) {  // TODO can this be cleaner with a match op?
            Seq(portPath, exportMap(portPath))  // exterior port
          } else if (reverseExportMap.contains(portPath)) {
            Seq(portPath, reverseExportMap(portPath))  // interior port
          } else {
             Seq(portPath)
          }

          val linkType = libraryAnalysis.linkOfPort(portType).exceptNone("can't find link of port")
          (linkType, priorConnects, s"Connect to $portPath with new ${EdgirUtils.SimpleLibraryPath(linkType)}")
      }
      val linkAvailable = libraryAnalysis.connectablePorts(linkType)
      val linkConnectableTypes = linkAvailable.keySet
      val alreadyConnectedPaths = ConnectToolAnalysis.connectsInBlock(focusPath, focusBlock)

      val boundaryPorts = ConnectToolAnalysis.topPortsOfBlock(focusPath, focusBlock)
          .filter { case (portPath, portType) =>
            libraryAnalysis.bridgedPortByOuter(portType) match {
              case Some(bridgedPortType) =>
                linkConnectableTypes.contains(bridgedPortType) &&
                    (priorConnectPaths.contains(portPath) || !alreadyConnectedPaths.contains(portPath))
              case None => false
            }
          }
      val internalPorts = focusBlock.blocks.toSeq.flatMap { case (intBlockName, intBlock) =>
        ConnectToolAnalysis.topPortsOfBlock(focusPath + intBlockName, intBlock.getHierarchy)
      }.filter { case (portPath, portType) =>  // filter by connectable type and not connected (except to this link)
        linkConnectableTypes.contains(portType) &&
            (priorConnectPaths.contains(portPath) || !alreadyConnectedPaths.contains(portPath))
      }

      val exportablePorts = if (isExteriorPort) {  // get all internal ports of the same type
        focusBlock.blocks.toSeq.flatMap { case (intBlockName, intBlock) => // get all ports
          ConnectToolAnalysis.topPortsOfBlock(focusPath + intBlockName, intBlock.getHierarchy)
        }.filter { case (innerPortPath, innerPortType) => // filter by same type and not connected (except to this link)
          innerPortType == portType &&
              (priorConnectPaths.contains(portPath) || !alreadyConnectedPaths.contains(portPath))
        }.map(_._1) // don't need type data
      } else {  // get all external ports of the same type
        focusBlock.ports.filter { case (extPortName, extPort) =>
          ConnectToolAnalysis.typeOfPort(extPort).get == portType
        }.map(focusPath + _._1).toSeq  // only keep path data
      }

      new ConnectTool(interface, focusPath, portPath,
        priorConnectPaths, linkAvailable, boundaryPorts ++ internalPorts, exportablePorts,
        connectName
      )
    }
  }
}


/** Tool for making connections from a port
  */
class ConnectTool(val interface: ToolInterface, focusPath: DesignPath, initialPortPath: DesignPath,
                  priorConnectPaths: Seq[DesignPath], linkAvailable: Map[ref.LibraryPath, Int],
                  linkTargets: Seq[(DesignPath, ref.LibraryPath)], availableExports: Seq[DesignPath],
                  name: String
                 ) extends BaseTool {
  private val selected = mutable.ListBuffer[DesignPath]()  // order preserving

  private val linkTargetsMap = linkTargets.toMap

  def connectsAvailable(): Set[DesignPath] = {  // returns all available ports to connect
    val selectedTypes = selected.toSeq.flatMap { linkTargetsMap.get(_) }
    if (linkTargets.isEmpty) {  // export only TODO less heuristic?
      if (selected.nonEmpty || priorConnectPaths.size > 1) {  // already connected
        Set()
      } else {
        availableExports.toSet
      }
    } else {
      val allTypes = selectedTypes :+ linkTargetsMap(initialPortPath)  // TODO if nonexistent, eg export only ext port
      val allTypeCounts = allTypes.groupBy(identity).mapValues(_.size)
      val linkRemainingTypes = linkAvailable.map { case (linkType, linkTypeCount) =>  // subtract connected count
        linkType -> (linkTypeCount - allTypeCounts.getOrElse(linkType, 0))
      } .collect { case (linkType, linkTypeCount) if linkTypeCount > 0 =>  // filter > 0, convert to counts
        linkType
      } .toSet
      val linkConnectablePaths = linkTargets.collect {  // filter by type, convert to paths
        case (linkTargetPath, linkTargetType) if linkRemainingTypes.contains(linkTargetType) => linkTargetPath
      }

      if (selected.isEmpty && priorConnectPaths == Seq(initialPortPath)) {  // no existing connects, can also export
        linkConnectablePaths.toSet ++ availableExports
      } else {  // existing connects, can't export
        linkConnectablePaths.toSet
      }
    }
  }

  def updateSelected(): Unit = {  // updates selected in graph and text
    interface.setStatus(name + ": " + selected.mkString(", "))
    interface.setGraphSelections(priorConnectPaths.toSet + initialPortPath ++ selected.toSet)
    val availablePaths = connectsAvailable()
    val availableBlockPaths = availablePaths.map(_.split._1)
    interface.setGraphHighlights(Some(Set(focusPath) ++ availablePaths ++ availableBlockPaths))
  }

  override def init(): Unit = {
    interface.setDesignTreeSelection(None)
    updateSelected()
  }

  override def onPathMouse(e: MouseEvent, path: DesignPath): Unit = {
    val resolved = EdgirUtils.resolveExact(path, interface.getDesign)

    if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 1) {
      resolved match {
        case Some(_: elem.Port | _: elem.Bundle | _: elem.PortArray) if path != initialPortPath =>
          if (selected.contains(path)) {  // toggle selection
            selected -= path
            updateSelected()
          } else {
            if (connectsAvailable().contains(path)) {
              selected += path
              updateSelected()
            } else {
              PopupUtils.createErrorPopup(s"not connectable", e)  // TODO more detailed errors?
            }
          }
        case _ =>  // ignored
      }
    } else if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2) {
      if (selected.isEmpty) {  // cancel
        interface.endTool()
        return
      }
      exceptionPopup(e) {  // quick insert at caret
        // TODO this should use the captured focus instead, but here's a sanity check
        requireExcept(BlockVisualizerService(interface.getProject).getContextBlock.get._1 == focusPath,
          "focus consistency sanity check failed")

        val contextPyClass = InsertAction.getPyClassOfContext(interface.getProject).exceptError
        val contextPsiFile = contextPyClass.getContainingFile.exceptNull("no file")
        val caretPsiElement = InsertAction.getCaretAtFile(contextPsiFile, contextPyClass, interface.getProject).exceptError

        def pathToPairs(path: DesignPath): (String, String) = {
          requireExcept(path.startsWith(focusPath), s"unable to create expr for $path")
          if (path.steps.size == focusPath.steps.size + 2) {  // inner block + port
            (path.steps(path.steps.size - 2), path.steps.last)
          } else if (path.steps.size == focusPath.steps.size + 1) {
            ("", path.steps.last)
          } else {
            throw ExceptionNotifyException(s"unable to create expr for $path")
          }
        }
        val connectPairs = (Seq(initialPortPath) ++ selected.toSeq).map(pathToPairs)

        def continuation(elem: PsiElement): Unit = {  // TODO can we use compose or something?
          interface.endTool()
          InsertAction.navigateElementFn(elem)
        }
        val action = InsertConnectAction.createInsertConnectFlow(caretPsiElement, connectPairs,
          s"Connect ${selected.mkString(", ")} at ${contextPyClass.getName} caret",
          interface.getProject, continuation).exceptError
        action()
      }
    }
  }
}
