package edg_ide.ui

import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.PyFunction
import edg.elem.elem
import edg.expr.expr
import edg.ref.ref
import edg.util.Errorable
import edg.wir.{BlockConnectivityAnalysis, DesignPath, LibraryConnectivityAnalysis}
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
    val blockAnalysis = new BlockConnectivityAnalysis(focusBlock)

    val portConnected = blockAnalysis.getConnected(
      portPath.postfixFromOption(focusPath).exceptNone("port not in focus block"))
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

    val priorConnectPaths = portConnected.getPorts.map(focusPath ++ _).toSet + portPath
    val connectablePathTypes = connectableRefTypes.collect{
      case (connectableRef, connectableType) if !focusBlockConnectedRefs.contains(connectableRef) =>
        (focusPath ++ connectableRef, connectableType)
    }
    val exportablePaths = exportableRefs.collect {
      case exportableRef if !focusBlockConnectedRefs.contains(exportableRef) =>
        focusPath ++ exportableRef
    }
    new ConnectTool(interface, focusPath, portPath,
      priorConnectPaths.toSeq, linkAvailable, connectablePathTypes.toSeq, exportablePaths.toSeq,
      portConnected.toString
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
      // TODO need to account for prior connects
      val allTypes = selectedTypes ++ linkTargetsMap.get(initialPortPath).toSeq
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
      exceptionPopup(e) { // quick insert at caret
        (new ConnectPopup(interface, focusPath, initialPortPath, selected.toSeq, name)
            .defaultAction.exceptError)()
      }
    } else if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) {
      new ConnectPopup(interface, focusPath, initialPortPath, selected.toSeq, name)
          .show(e.getComponent, e.getX, e.getY)
    }
  }
}
