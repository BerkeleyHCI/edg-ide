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
            (blockPath ++ blockRef, typeOfPort(link.ports(linkPortName)).get)
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
    val libraryAnalysis = new LibraryConnectivityAnalysis(interface.getLibrary)  // TODO this should be saved & reused!
    val port = EdgirUtils.resolveExact(initialPortPath, interface.getDesign).exceptNone("no port")

    val containingBlockPath = EdgirUtils.resolveDeepestBlock(initialPortPath, interface.getDesign)._1
    val focusPath = interface.getFocus
    requireExcept(containingBlockPath == focusPath || containingBlockPath.split._1 == focusPath,
      "port not reachable from focus")
      // TODO refactor

    val containingBlock = EdgirUtils.resolveExactBlock(focusPath, interface.getDesign).get
    // TODO exterior connect analysis and bridge analysis
    val linkNameOpt = ConnectToolAnalysis.linkNameOfPort(focusPath, containingBlock, initialPortPath)
    val (linkType, linkConnects) = linkNameOpt match {
      case Some(linkName) =>
        val link = EdgirUtils.resolveExact(focusPath + linkName, interface.getDesign)
            .exceptNone("can't find connected link")
            .instanceOfExcept[elem.Link]("invalid connected link")
        val linkType = link.superclasses.onlyExcept("invalid connected link type")
        val connectTypes = ConnectToolAnalysis.connectsToLink(focusPath, containingBlock, linkName)
        // Use the link type from the link itself
        (linkType, connectTypes)
      case None =>
        val portType = ConnectToolAnalysis.typeOfPort(port).exceptError
        val linkType = libraryAnalysis.linkOfPort(portType).exceptNone("can't find link of port")
        (linkType, Seq())
    }

    new ConnectTool(interface, focusPath, initialPortPath, linkType, linkConnects)
  }
}


/** Tool for making connections from a port
  */
class ConnectTool(val interface: ToolInterface, focusPath: DesignPath, initialPortPath: DesignPath,
                  linkType: ref.LibraryPath, linkConnects: Seq[(DesignPath, ref.LibraryPath)]
                 ) extends BaseTool {
  private val selected = mutable.Set[DesignPath]()

  private val linkConnectPaths = linkConnects.map(_._1).toSet

  override def init(): Unit = {
    interface.setDesignTreeSelection(None)
    interface.setGraphSelections(linkConnectPaths + initialPortPath)
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
