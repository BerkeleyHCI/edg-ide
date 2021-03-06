package edg_ide.ui

import com.intellij.openapi.project.Project
import edg.elem.elem
import edg.schema.schema
import edg.ref.ref
import edg.util.Errorable
import edg.wir.DesignPath
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptOption}
import edg_ide.{EdgirUtils, PsiUtils}
import edg_ide.util.{DesignAnalysisUtils, ExceptionNotifyException, exceptable, exceptionPopup, requireExcept}

import java.awt.event.MouseEvent
import javax.swing.{JLabel, JPopupMenu, SwingUtilities}


trait NavigationPopupMenu extends JPopupMenu {
  def addGotoInstantiationItems(path: DesignPath,
                                design: schema.Design, project: Project): Unit = {
    val actionPairs = exceptable {
      val assigns = DesignAnalysisUtils.allAssignsTo(path, design, project).exceptError

      assigns.map { assign =>
        val fileLine = PsiUtils.fileLineOf(assign, project)
            .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
        (s"Goto Instantiation$fileLine", () => assign.navigate(true))
      }
    }

    PopupMenuUtils.MenuItemsFromErrorableSeq(actionPairs, s"Goto Instantiation")
        .foreach(add)
  }

  def addGotoDefinitionItem(superclass: Errorable[ref.LibraryPath],
                            project: Project): Unit = {
    val pyClass = exceptable {
      val pyClass = DesignAnalysisUtils.pyClassOf(superclass.exceptError, project).exceptError
      requireExcept(pyClass.canNavigateToSource, "class not navigatable")
      pyClass
    }
    val fileLine = pyClass.flatMap(PsiUtils.fileLineOf(_, project))
        .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
    val action = exceptable {
      () => pyClass.exceptError.navigate(true)
    }
    val actionName = s"Goto Definition$fileLine"

    val gotoDefinitionItem = PopupMenuUtils.MenuItemFromErrorable(action, actionName)
    add(gotoDefinitionItem)
  }
}


class DesignBlockPopupMenu(path: DesignPath, interface: ToolInterface)
    extends JPopupMenu with NavigationPopupMenu {
  private val block = Errorable(EdgirUtils.resolveExactBlock(path, interface.getDesign), "no block at path")
  private val blockClass = block.map(_.superclasses).require("invalid class")(_.length == 1)
      .map(_.head)

  add(new JLabel(s"Design Block: ${blockClass.mapToString(EdgirUtils.SimpleLibraryPath)} at $path"))
  addSeparator()

  val setFocusAction: Errorable[() => Unit] = exceptable {
    if (path == interface.getFocus) {  // double click focus block to zoom out
      requireExcept(path != DesignPath(), "can't zoom out of top")
      () => interface.setFocus(path.split._1)
    } else {
      () => interface.setFocus(path)
    }
  }
  val setFocusName: String = {  // TODO unify w/ above?
    if (path == interface.getFocus) {  // double click focus block to zoom out
      if (path != DesignPath()) {
        s"Focus Out to ${path.split._1}"
      } else {
        s"Focus Out"
      }
    } else {
      s"Focus In to $path"
    }
  }
  private val setFocusItem = PopupMenuUtils.MenuItemFromErrorable(setFocusAction, setFocusName)
  add(setFocusItem)
  addSeparator()

  addGotoInstantiationItems(path, interface.getDesign, interface.getProject)
  addGotoDefinitionItem(blockClass, interface.getProject)
}


class DesignPortPopupMenu(path: DesignPath, interface: ToolInterface)
    extends JPopupMenu with NavigationPopupMenu {
  private val portClass = exceptable {
    val port = EdgirUtils.resolveExact(path, interface.getDesign).exceptNone("no port at path")
    port match {
      case port: elem.Port =>
        requireExcept(port.superclasses.length == 1, "invalid class")
        port.superclasses.head
      case bundle: elem.Bundle =>
        requireExcept(bundle.superclasses.length == 1, "invalid class")
        bundle.superclasses.head
      case array: elem.PortArray =>
        requireExcept(array.superclasses.length == 1, "invalid class")
        array.superclasses.head
      case other => throw ExceptionNotifyException(s"unknown ${other.getClass} at path")
    }
  }
  add(new JLabel(s"Design Port: ${portClass.mapToString(EdgirUtils.SimpleLibraryPath)} at $path"))
  addSeparator()

  val startConnectAction = exceptable {
    val connectTool = ConnectTool(interface, path).exceptError
    () => interface.startNewTool(connectTool)
  }
  private val startConnectItem = PopupMenuUtils.MenuItemFromErrorable(startConnectAction, "Start Connect")
  add(startConnectItem)
  addSeparator()

  addGotoInstantiationItems(path, interface.getDesign, interface.getProject)
  addGotoDefinitionItem(portClass, interface.getProject)

  val gotoConnectPairs = exceptable {
    val assigns = DesignAnalysisUtils.allConnectsTo(path, interface.getDesign, interface.getProject).exceptError

    assigns.map { assign =>
      val fileLine = PsiUtils.fileLineOf(assign, interface.getProject)
          .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
      (s"Goto Connect$fileLine", () => assign.navigate(true))
    }
  }

  PopupMenuUtils.MenuItemsFromErrorableSeq(gotoConnectPairs, s"Goto Connect")
      .foreach(add)
}


class DefaultTool(val interface: ToolInterface) extends BaseTool {
  private var ignoreSelect: Boolean = false

  // Mouse event that is generated on any mouse event in either the design tree or graph layout
  override def onPathMouse(e: MouseEvent, path: DesignPath): Unit = {
    val resolved = EdgirUtils.resolveExact(path, interface.getDesign)

    if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 1) {
      onSelect(path)
    } else if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2) {
      // double click
      resolved match {
        case Some(_: elem.HierarchyBlock) => // blocks: quick set focus
          exceptionPopup(e) {
            (new DesignBlockPopupMenu(path, interface).setFocusAction.exceptError)()
          }
        case Some(_: elem.Port | _: elem.Bundle | _: elem.PortArray) =>  // ports: start connect
          exceptionPopup(e) {
            (new DesignPortPopupMenu(path, interface).startConnectAction.exceptError)()
          }
        case _ =>
      }
    } else if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) {
      // right click context menu
      resolved match {
        case Some(_: elem.HierarchyBlock) =>
          new DesignBlockPopupMenu(path, interface).show(e.getComponent, e.getX, e.getY)
        case Some(_: elem.Port | _: elem.Bundle | _: elem.PortArray) =>
          new DesignPortPopupMenu(path, interface).show(e.getComponent, e.getX, e.getY)
        case _ =>  // TODO support other element types
      }
    }
  }

  // Event that is generated when the tree selection changes.
  override def onSelect(path: DesignPath): Unit = {
    if (ignoreSelect) {
      return
    }
    ignoreSelect = true
    val (containingPath, containingBlock) = EdgirUtils.resolveDeepestBlock(path, interface.getDesign)
    interface.setDesignTreeSelection(Some(containingPath))
    interface.setGraphSelections(Set(path))
    interface.setDetailView(containingPath)
    ignoreSelect = false
  }
}
