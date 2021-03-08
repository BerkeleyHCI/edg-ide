package edg_ide.ui

import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.PyFunction
import edg.elem.elem
import edg.ref.ref
import edg.util.Errorable
import edg.wir.{BlockConnectivityAnalysis, Connection, DesignPath, LibraryConnectivityAnalysis}
import edg_ide.{EdgirUtils, PsiUtils}
import edg_ide.actions.{InsertAction, InsertConnectAction}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption, ExceptSeq}
import edg_ide.util.{ExceptionNotifyException, exceptable, exceptionPopup, requireExcept}

import java.awt.event.MouseEvent
import javax.swing.{JLabel, JPopupMenu, SwingUtilities}
import collection.mutable


object ConnectToolAnalysis {
  // TODO this is awful, replace with union types when possible!
  // TODO goes in some shared analysis util?
  def typeOfPort(port: Any): Errorable[ref.LibraryPath] = exceptable { port match {
    case port: elem.Port =>
      port.superclasses.onlyExcept("invalid port class")
    case port: elem.Bundle =>
      port.superclasses.onlyExcept("invalid port class")
    case array: elem.PortArray =>
      array.superclasses.onlyExcept("invalid port class")
    case isOther => throw ExceptionNotifyException(s"unexpected port ${isOther.getClass}")
  }}
}


object ConnectTool {
  /** External interface for creating a ConnectTool, which does the needed analysis work and can return an error
    * (as an exceptable) if the ConnectTool cannot be created.
    */
  def apply(interface: ToolInterface, portPath: DesignPath): Errorable[ConnectTool] = exceptable {
    val libraryAnalysis = new LibraryConnectivityAnalysis(interface.getLibrary)  // TODO this should be saved & reused!
    val port = EdgirUtils.resolveExact(portPath, interface.getDesign).exceptNone("no port")
    val portType = ConnectToolAnalysis.typeOfPort(port).exceptError

    val containingBlockPath = EdgirUtils.resolveDeepestBlock(portPath, interface.getDesign)._1
    val focusPath = interface.getFocus
    val focusBlock = EdgirUtils.resolveExact(focusPath, interface.getDesign)
        .exceptNone("can't reach focus block")
        .instanceOfExcept[elem.HierarchyBlock]("focus block not a block")
    val blockAnalysis = new BlockConnectivityAnalysis(focusBlock)

    val portRef = portPath.postfixFromOption(focusPath).exceptNone("port not in focus block")
    val portConnected = blockAnalysis.getConnected(portRef)
    val focusBlockConnectedRefs =
      blockAnalysis.getAllConnectedInternalPorts ++ blockAnalysis.getAllConnectedExternalPorts
    val focusBlockConnectable = blockAnalysis.allConnectablePortTypes

    val isExteriorPort = containingBlockPath == focusPath  // a boundary port that may need to be bridged
    val exportableRefs = if (isExteriorPort) {  // find inner ports of the same type
      focusBlockConnectable.innerPortTypes.collect {
        case (intPortRef, intPortType) if portType == intPortType => intPortRef
      }
    } else {  // find exterior ports of the same type
      focusBlockConnectable.exteriorPortTypes.collect {
        case (extPortRef, extPortType) if portType == extPortType =>
          extPortRef
      }
    }

    val interiorPortTypeOption = if (isExteriorPort) {
      libraryAnalysis.bridgedPortByOuter(portType)
    } else {
      Some(portType)
    }
    val (linkAvailable, connectableRefTypes) = interiorPortTypeOption.map { interiorPortType =>
      val linkType = libraryAnalysis.linkOfPort(interiorPortType).exceptNone(s"no link for bridged port")
      val linkAvailable = libraryAnalysis.connectablePorts(linkType)
      val linkAvailableTypes = linkAvailable.keySet
      val connectableRefs = focusBlockConnectable.innerPortTypes.collect {  // filter by has-port
        case (intPortRef, intPortType) if linkAvailableTypes.contains(intPortType) =>
          (intPortRef, intPortType)
      } ++ focusBlockConnectable.exteriorPortTypes.map {  // map to bridged-port-option
        case (extPortRef, extPortType) => (extPortRef, libraryAnalysis.bridgedPortByOuter(extPortType))
      }.collect {  // filter by has-bridged-port and connectable-bridged-port
        case (extPortRef, Some(bridgedPortType)) if linkAvailableTypes.contains(bridgedPortType) =>
          (extPortRef, bridgedPortType)
      }
      (linkAvailable, connectableRefs)
    } .getOrElse((Map[ref.LibraryPath, Int](), Seq()))  // if no link, then no connects

    val otherConnectedRefs = focusBlockConnectedRefs.toSet -- portConnected.getPorts - portRef
    val connectablePathTypes = connectableRefTypes.collect{
      case (connectableRef, connectableType) if !otherConnectedRefs.contains(connectableRef) =>
        (focusPath ++ connectableRef, connectableType)
    }
    val exportablePaths = exportableRefs.collect {
      case exportableRef if !otherConnectedRefs.contains(exportableRef) =>
        focusPath ++ exportableRef
    }
    new ConnectTool(interface, focusPath, portPath,
      portConnected, linkAvailable, connectablePathTypes.toSeq, exportablePaths.toSeq
    )
  }
}


class ConnectPopup(interface: ToolInterface, focusPath: DesignPath, initialPortPath: DesignPath,
                   selected: Seq[DesignPath],
                   name: String) extends JPopupMenu {
  add(new JLabel(s"$name"))
  addSeparator()

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

  def continuation(name: String, elem: PsiElement): Unit = {  // TODO can we use compose or something?
    interface.endTool()
    InsertAction.navigateElementFn(name, elem)
  }

  private val contextPyClass = InsertAction.getPyClassOfContext(interface.getProject)
  private val contextPyName = contextPyClass.mapToString(_.getName)
  private val connectPairs = (initialPortPath +: selected).map(pathToPairs)

  private val caretPsiElement = exceptable {
    val contextPsiFile = contextPyClass.exceptError.getContainingFile.exceptNull("no file")
    InsertAction.getCaretAtFile(contextPsiFile, contextPyClass.exceptError, interface.getProject).exceptError
  }

  val appendConnectAction: Errorable[() => Unit] = exceptable {
    requireExcept(selected.nonEmpty, "no selection to connect")
    // TODO this should use the captured focus instead, but here's a sanity check
    requireExcept(BlockVisualizerService(interface.getProject).getContextBlock.get._1 == focusPath,
      "focus consistency sanity check failed")

    InsertConnectAction.createAppendConnectFlow(caretPsiElement.exceptError, connectPairs,
      s"Append connect to ${connectPairs.mkString(", ")} at $contextPyName caret",
      interface.getProject, continuation).exceptError
  }
  private val appendConnectCaretFileLine = exceptable {
    appendConnectAction.exceptError
    PsiUtils.fileLineOf(caretPsiElement.exceptError, interface.getProject).exceptError
  }.mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
  private val appendConnectItem = PopupMenuUtils.MenuItemFromErrorable(
    appendConnectAction, s"Append connect at $contextPyName caret$appendConnectCaretFileLine")
  add(appendConnectItem)

  private val appendPairs = exceptable {
    requireExcept(selected.nonEmpty, "no selection to connect")
    // TODO this should use the captured focus instead, but here's a sanity check
    requireExcept(BlockVisualizerService(interface.getProject).getContextBlock.get._1 == focusPath,
      "focus consistency sanity check failed")

    InsertConnectAction.findConnectsTo(contextPyClass.exceptError, connectPairs.head, interface.getProject).exceptError
        .map { call =>
          val fn = PsiTreeUtil.getParentOfType(call, classOf[PyFunction])
          val fileLine = PsiUtils.fileLineOf(call, interface.getProject)
              .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
          val label = s"Append connect at ${contextPyName}.${fn.getName}$fileLine"
          val action = InsertConnectAction.createAppendConnectFlow(call, connectPairs,
            s"Append connect to ${connectPairs.mkString(", ")} at $contextPyName.${fn.getName}",
            interface.getProject, continuation)
          (label, action)
        } .collect {
      case (fn, Errorable.Success(action)) => (fn, action)
    }.exceptEmpty("no insertion points")
  }
  PopupMenuUtils.MenuItemsFromErrorableSeq(appendPairs, s"Append connect into $contextPyName")
      .foreach(add)

  val insertConnectAction: Errorable[() => Unit] = exceptable {
    requireExcept(selected.nonEmpty, "no selection to connect")
    // TODO this should use the captured focus instead, but here's a sanity check
    requireExcept(BlockVisualizerService(interface.getProject).getContextBlock.get._1 == focusPath,
      "focus consistency sanity check failed")

    InsertConnectAction.createInsertConnectFlow(caretPsiElement.exceptError, connectPairs,
      s"Insert connect to ${connectPairs.mkString(", ")} at $contextPyName caret",
      interface.getProject, continuation).exceptError
  }
  private val insertConnectCaretFileLine = exceptable {
    insertConnectAction.exceptError
    PsiUtils.fileNextLineOf(caretPsiElement.exceptError, interface.getProject).exceptError
  }.mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
  private val insertConnectItem = PopupMenuUtils.MenuItemFromErrorable(
    insertConnectAction, s"Insert connect at $contextPyName caret$insertConnectCaretFileLine")
  add(insertConnectItem)

  private val insertionPairs = exceptable {
    requireExcept(selected.nonEmpty, "no selection to connect")
    // TODO this should use the captured focus instead, but here's a sanity check
    requireExcept(BlockVisualizerService(interface.getProject).getContextBlock.get._1 == focusPath,
      "focus consistency sanity check failed")

    InsertAction.findInsertionPoints(contextPyClass.exceptError, interface.getProject).exceptError
        .map { fn =>
          val fileLine = PsiUtils.fileNextLineOf(fn.getStatementList.getLastChild, interface.getProject)
              .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
          val label = s"Insert connect at ${contextPyName}.${fn.getName}$fileLine"
          val action = InsertConnectAction.createInsertConnectFlow(fn.getStatementList.getStatements.last,
            connectPairs,
            s"Insert connect to ${connectPairs.mkString(", ")} at $contextPyName.${fn.getName}",
            interface.getProject, continuation)
          (label, action)
        } .collect {
      case (fn, Errorable.Success(action)) => (fn, action)
    }.exceptEmpty("no insertion points")
  }
  PopupMenuUtils.MenuItemsFromErrorableSeq(insertionPairs, s"Insert connect into $contextPyName")
      .foreach(add)
  addSeparator()

  val cancelAction: Errorable[() => Unit] = exceptable {
    () => interface.endTool()
  }
  private val cancelItem = PopupMenuUtils.MenuItemFromErrorable(cancelAction, "Cancel")
  add(cancelItem)

  val defaultAction: Errorable[() => Unit] = if (selected.nonEmpty) {
    appendConnectAction match {
      case Errorable.Success(_) => appendConnectAction  // prefer append connect if available
      case _ => insertConnectAction
    }
  } else {
    cancelAction
  }
}


sealed trait ConnectToolAction
object ConnectToolAction {
  case class None() extends ConnectToolAction
  case class AppendToLink(focusPath: DesignPath, linkName: String,
                          linkPorts: Seq[DesignPath], bridgePorts: Seq[DesignPath]) extends ConnectToolAction
  case class NewLink(focusPath: DesignPath,
                     linkPorts: Seq[DesignPath], bridgePorts: Seq[DesignPath]) extends ConnectToolAction
  case class NewExport(focusPath: DesignPath, exteriorPort: DesignPath, innerPort: DesignPath) extends ConnectToolAction
  case class RefactorExportToLink(focusPath: DesignPath, priorExteriorPort: DesignPath, priorInnerPort: DesignPath,
                                  linkPorts: Seq[DesignPath], bridgePorts: Seq[DesignPath]) extends ConnectToolAction
}


/** Tool for making connections from a port
  */
class ConnectTool(val interface: ToolInterface, focusPath: DesignPath, portPath: DesignPath,
                  priorConnect: Connection, linkAvailable: Map[ref.LibraryPath, Int],
                  linkTargets: Seq[(DesignPath, ref.LibraryPath)], availableExports: Seq[DesignPath]
                 ) extends BaseTool {
  private val linkTargetsMap = linkTargets.toMap

  private def isExportOnly(path: DesignPath) = !linkTargetsMap.contains(path)
  private def isExteriorPort(path: DesignPath) = path.split._1 == focusPath

  // User State
  private val selected = mutable.ListBuffer[DesignPath]()  // order preserving; excluding priorConnect and portPath

  private def getConnectAction(): ConnectToolAction = priorConnect match {
    case Connection.Disconnected() =>
      if (selected.isEmpty) { // no connects, can link and export
        ConnectToolAction.None()
      } else if (selected.size == 1 &&
          (isExportOnly(selected.head) || isExportOnly(portPath))) {  // selected export
        if (isExteriorPort(portPath)) {
          ConnectToolAction.NewExport(focusPath, portPath, selected.head)
        } else {
          ConnectToolAction.NewExport(focusPath, selected.head, portPath)
        }
      } else {  // link connection
        val (linkPorts, bridgePorts) = selected.toSeq.partition(!isExteriorPort(_))
        if (isExteriorPort(portPath)) {
          ConnectToolAction.NewLink(focusPath, linkPorts, portPath +: bridgePorts)
        } else {
          ConnectToolAction.NewLink(focusPath, portPath +: linkPorts, bridgePorts)
        }
      }
    case Connection.Export(constraintName, exteriorPort, innerBlockPort) =>
      if (linkTargetsMap.isDefinedAt(focusPath ++ exteriorPort)) {  // can refactor current export to link
        val (linkPorts, bridgePorts) = selected.toSeq.partition(!isExteriorPort(_))
        ConnectToolAction.RefactorExportToLink(focusPath, focusPath ++ exteriorPort, focusPath ++ innerBlockPort,
          linkPorts, bridgePorts)
      } else {  // cannot refactor existing export to link, can only have single export
        ConnectToolAction.None()
      }
    case Connection.Link(linkName, linkConnects, bridgedExports) =>  // prior link, add new links but no exports
      val (linkPorts, bridgePorts) = selected.toSeq.partition(!isExteriorPort(_))
      ConnectToolAction.AppendToLink(focusPath, linkName, linkPorts, bridgePorts)
  }

  private def connectToolActionToDesc(action: ConnectToolAction): String = action match {
    case ConnectToolAction.None() => s"Create new connection with $portPath"
    case ConnectToolAction.AppendToLink(focusPath, linkName, linkPorts, bridgePorts) =>
      s"Append ports to $linkName: ${(linkPorts ++ bridgePorts).mkString(", ")}"
    case ConnectToolAction.NewLink(focusPath, linkPorts, bridgePorts) =>
      s"Create new connection: ${(linkPorts ++ bridgePorts).mkString(", ")}"
    case ConnectToolAction.NewExport(focusPath, exteriorPort, innerPort) =>
      s"Create new export: $exteriorPort from $innerPort"
    case ConnectToolAction.RefactorExportToLink(focusPath, priorExteriorPort, priorInnerPort, linkPorts, bridgePorts) =>
      s"Create connection from export of $priorInnerPort from $priorExteriorPort: ${(linkPorts ++ bridgePorts).mkString(", ")}"

  }

  // returns all available ports to connect
  private def connectsAvailable(): Seq[DesignPath] = {
    // returns all available ports, assuming a current link
    def linkRemainingConnects(priorConnectPaths: Seq[DesignPath]): Seq[DesignPath] = {
      val allTypes = priorConnectPaths.flatMap { linkTargetsMap.get(_) }
      val allTypeCounts = allTypes.groupBy(identity).mapValues(_.size)

      val linkRemainingTypes = linkAvailable.map { case (linkType, linkTypeCount) =>  // subtract connected count
        linkType -> (linkTypeCount - allTypeCounts.getOrElse(linkType, 0))
      } .collect { case (linkType, linkTypeCount) if linkTypeCount > 0 =>  // filter > 0, convert to types
        linkType
      } .toSet
      linkTargets.collect {  // filter by type, convert to paths
        case (linkTargetPath, linkTargetType) if linkRemainingTypes.contains(linkTargetType) => linkTargetPath
      }
    }

    priorConnect match {
      case Connection.Disconnected() =>
        if (selected.isEmpty) { // no connects, can link and export
          linkRemainingConnects(Seq(portPath)) ++ availableExports
        } else if (selected.size == 1 &&
            (isExportOnly(selected.head) || isExportOnly(portPath))) {  // selected export
          Seq()
        } else {  // link connection
          linkRemainingConnects(portPath +: selected.toSeq)
        }
      case Connection.Export(constraintName, exteriorPort, innerBlockPort) =>
        if (linkTargetsMap.isDefinedAt(focusPath ++ exteriorPort)) {  // can refactor current export to link
          linkRemainingConnects(Seq(focusPath ++ exteriorPort, focusPath ++ innerBlockPort))
        } else {  // cannot refactor existing export to link, can only have single export
          Seq()
        }
      case Connection.Link(linkName, linkConnects, bridgedExports) =>  // prior link, add new links but no exports
        linkRemainingConnects(linkConnects.map(focusPath ++ _._1) ++
            bridgedExports.map(focusPath ++ _._1) ++ selected.toSeq)
    }
  }

  def updateSelected(): Unit = {  // updates selected in graph and text
    interface.setStatus(connectToolActionToDesc(getConnectAction()))
    interface.setGraphSelections(priorConnect.getPorts.map(focusPath ++ _).toSet + portPath ++ selected.toSet)
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
        case Some(_: elem.Port | _: elem.Bundle | _: elem.PortArray) if path != portPath =>
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
      exceptionPopup(e) { // quick insert at caret
        val connectToolAction = getConnectAction()
        (new ConnectPopup(interface, focusPath, portPath, selected.toSeq, connectToolActionToDesc(connectToolAction))
            .defaultAction.exceptError)()
      }
    } else if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) {
      val connectToolAction = getConnectAction()
      new ConnectPopup(interface, focusPath, portPath, selected.toSeq, connectToolActionToDesc(connectToolAction))
          .show(e.getComponent, e.getX, e.getY)
    }
  }
}
