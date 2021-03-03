package edg_ide.ui

import com.intellij.notification.NotificationGroup
import com.intellij.openapi.project.Project
import com.jetbrains.python.psi.PyAssignmentStatement
import edg.elem.elem
import edg.schema.schema
import edg.ref.ref
import edg.util.Errorable
import edg.wir.DesignPath
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptOption}
import edg_ide.{EdgirUtils, PsiUtils}
import edg_ide.util.{DesignAnalysisUtils, ExceptionNotifyException, exceptable, exceptionNotify, requireExcept}

import java.awt.event.{ActionEvent, MouseEvent}
import javax.swing.{JLabel, JMenuItem, JPopupMenu, SwingUtilities}


trait NavigationPopupMenu extends JPopupMenu {
  def addNavigationItems(path: DesignPath, superclass: Errorable[ref.LibraryPath],
                         design: schema.Design, project: Project): Unit = {
    val assigns = DesignAnalysisUtils.allAssignsTo(path, design, project)
    PopupMenuUtils.MenuItemsFromErrorableSeq(assigns,
      errMsg => s"Goto Instantiation ($errMsg)",
      {assign: PyAssignmentStatement =>
        s"Goto Instantiation (${PsiUtils.fileLineOf(assign, project).mapToString(identity)})"}) { assign =>
      assign.navigate(true)
    }.foreach(add)

    val pyClass = superclass.flatMap(DesignAnalysisUtils.pyClassOf(_, project))
    val pyNavigatable = pyClass.require("class not navigatable")(_.canNavigateToSource)

    val fileLine = pyNavigatable.flatMap(PsiUtils.fileLineOf(_, project)).mapToString(identity)
    val gotoDefinitionItem = PopupMenuUtils.MenuItemFromErrorable(pyNavigatable,
      s"Goto Definition (${fileLine})") { pyNavigatable =>
      pyNavigatable.navigate(true)
    }
    add(gotoDefinitionItem)
  }
}


class DesignBlockPopupMenu(path: DesignPath, design: schema.Design, project: Project)
    extends JPopupMenu with NavigationPopupMenu {
  private val block = Errorable(EdgirUtils.resolveExactBlock(path, design.getContents), "no block at path")
  private val blockClass = block.map(_.superclasses).require("invalid class")(_.length == 1)
      .map(_.head)

  add(new JLabel(s"Design Block: ${blockClass.mapToString(EdgirUtils.SimpleLibraryPath)} at $path"))
  addSeparator()


  addNavigationItems(path, blockClass, design, project)
  addSeparator()

  // TODO add goto parent / goto root if selected current focus?

  val setFocusItem = new JMenuItem(s"Focus View on $path")
  setFocusItem.addActionListener((e: ActionEvent) => {
    BlockVisualizerService(project).setContext(path)
  })
  add(setFocusItem)
}


class DesignPortPopupMenu(path: DesignPath, design: schema.Design, project: Project)
    extends JPopupMenu with NavigationPopupMenu {
  private val portClass = exceptable {
    val port = EdgirUtils.resolveExact(path, design.getContents).exceptNone("no port at path")
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


  addNavigationItems(path, portClass, design, project)
}


class DefaultTool(val interface: ToolInterface) extends BaseTool {
  private val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.ui.DefaultTool")
  private var ignoreSelect: Boolean = false

  // Mouse event that is generated on any mouse event in either the design tree or graph layout
  override def onPathMouse(e: MouseEvent, path: DesignPath): Unit = {
    val resolved = EdgirUtils.resolveExact(path, interface.getDesign.contents.getOrElse(elem.HierarchyBlock()))

    if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 1) {
      onSelect(path)
    } else if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2) {
      // double click
      resolved match {
        case Some(_: elem.HierarchyBlock) => // blocks: quick set focus
          if (path == interface.getFocus) {  // double click focus block to zoom out
            if (path != DesignPath()) {
              interface.setFocus(path.split._1)
            }
          } else {
            interface.setFocus(path)
          }
        case Some(_: elem.Port | _: elem.Bundle | _: elem.PortArray) =>  // ports: start connect
          interface.startNewTool(new ConnectTool(interface, path))
        case _ =>
      }
    } else if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) {
      // right click context menu
      resolved match {
        case Some(_: elem.HierarchyBlock) =>
          val menu = new DesignBlockPopupMenu(path, interface.getDesign, interface.getProject)
          menu.show(e.getComponent, e.getX, e.getY)
        case Some(_: elem.Port | _: elem.Bundle | _: elem.PortArray) =>
          val menu = new DesignPortPopupMenu(path, interface.getDesign, interface.getProject)
          menu.show(e.getComponent, e.getX, e.getY)
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
    val designContents = interface.getDesign.contents.getOrElse(elem.HierarchyBlock())
    val (containingPath, containingBlock) = EdgirUtils.resolveDeepestBlock(path, designContents) match {
      case Some((path, block)) => (path, block)
      case None => (DesignPath(), designContents)
    }
    interface.setDesignTreeSelection(Some(containingPath))
    interface.setGraphSelections(Set(path))
    interface.setDetailView(containingPath)
    ignoreSelect = false
  }
}
