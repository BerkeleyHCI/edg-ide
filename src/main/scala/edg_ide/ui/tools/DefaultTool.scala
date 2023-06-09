package edg_ide.ui.tools

import com.intellij.openapi.application.{ModalityState, ReadAction}
import com.intellij.openapi.project.Project
import com.intellij.util.concurrency.AppExecutorUtil
import com.jetbrains.python.psi.search.PyClassInheritorsSearch
import edg.EdgirUtils.SimpleLibraryPath
import edg.util.Errorable
import edg.wir.DesignPath
import edg.wir.ProtoUtil.ParamProtoToSeqMap
import edg_ide.dse._
import edg_ide.ui.{BlockVisualizerService, ContextMenuUtils, DseService, PopupUtils}
import edg_ide.util.ExceptionNotifyImplicits.ExceptErrorable
import edg_ide.util._
import edg_ide.{EdgirUtils, PsiUtils}
import edgir.elem.elem
import edgir.ref.ref
import edgir.schema.schema

import java.awt.event.MouseEvent
import java.util.concurrent.Callable
import javax.swing.{JLabel, JPopupMenu, SwingUtilities}
import scala.jdk.CollectionConverters.CollectionHasAsScala

trait NavigationPopupMenu extends JPopupMenu {
  def addGotoInstantiationItems(path: DesignPath, design: schema.Design, project: Project): Unit = {
    val placeholder = ContextMenuUtils.MenuItem(() => {}, "Goto Instantiation (searching...)")
    placeholder.setEnabled(false)
    add(placeholder)

    ReadAction
      .nonBlocking(
        (
            () =>
              exceptable {
                val assigns = DesignAnalysisUtils.allAssignsTo(path, design, project).exceptError
                assigns.map { assign =>
                  val fileLine = PsiUtils
                    .fileLineOf(assign, project)
                    .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
                  (s"Goto Instantiation$fileLine", () => assign.navigate(true))
                }
              }
        ): Callable[Errorable[Seq[(String, () => Unit)]]]
      )
      .finishOnUiThread(
        ModalityState.defaultModalityState(),
        result => {
          val insertionIndex = this.getComponentIndex(placeholder)
          ContextMenuUtils
            .MenuItemsFromErrorableSeq(result, s"Goto Instantiation")
            .reverse
            .foreach(insert(_, insertionIndex))
          this.remove(placeholder)
          this.revalidate()
          this.repaint()
        }
      )
      .inSmartMode(project)
      .submit(AppExecutorUtil.getAppExecutorService)
  }

  def addGotoDefinitionItem(superclass: ref.LibraryPath, project: Project): Unit = {
    val pyClass = exceptable {
      val pyClass = DesignAnalysisUtils.pyClassOf(superclass, project).exceptError
      requireExcept(pyClass.canNavigateToSource, "class not navigatable")
      pyClass
    }
    val fileLine = pyClass
      .flatMap(PsiUtils.fileLineOf(_, project))
      .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
    val action = exceptable { () =>
      pyClass.exceptError.navigate(true)
    }
    val actionName = s"Goto Definition$fileLine"

    val gotoDefinitionItem = ContextMenuUtils.MenuItemFromErrorable(action, actionName)
    add(gotoDefinitionItem)
  }
}

class DesignBlockPopupMenu(path: DesignPath, interface: ToolInterface)
    extends JPopupMenu
    with NavigationPopupMenu {
  private val project = interface.getProject
  private val block = EdgirUtils.resolveExactBlock(path, interface.getDesign).getOrElse {
    PopupUtils.createErrorPopupAtMouse(f"internal error: no block at $path", this)
    throw new Exception()
  }
  private val blockClass = block.getSelfClass

  add(new JLabel(s"Design Block: ${blockClass.toSimpleString} at $path"))
  addSeparator()

  val setFocusAction: Errorable[() => Unit] = exceptable {
    if (path == interface.getFocus) { // double click focus block to zoom out
      requireExcept(path != DesignPath(), "can't zoom out of top")
      () => interface.setFocus(path.split._1)
    } else { () =>
      interface.setFocus(path)
    }
  }
  val setFocusName: String = { // TODO unify w/ above?
    if (path == interface.getFocus) { // double click focus block to zoom out
      if (path != DesignPath()) {
        s"Focus Out to ${path.split._1}"
      } else {
        s"Focus Out"
      }
    } else {
      s"Focus In to $path"
    }
  }
  private val setFocusItem = ContextMenuUtils.MenuItemFromErrorable(setFocusAction, setFocusName)
  add(setFocusItem)
  addSeparator()

  addGotoInstantiationItems(path, interface.getDesign, project)
  addGotoDefinitionItem(blockClass, project)

  if (DseFeature.kEnabled) {
    addSeparator()

    val rootClass = interface.getDesign.getContents.getSelfClass
    val refinementClass = block.prerefineClass.getOrElse(block.getSelfClass)
    add(
      ContextMenuUtils.MenuItemFromErrorable(
        exceptable {
          val blockPyClass = DesignAnalysisUtils.pyClassOf(refinementClass, project).get
          () => {
            ReadAction
              .nonBlocking((() => {
                DesignAnalysisUtils
                  .findOrderedSubclassesOf(blockPyClass)
                  .filter { subclass => // filter out abstract blocks
                    !DesignAnalysisUtils.isPyClassAbstract(subclass)
                  }
                  .map { subclass =>
                    DesignAnalysisUtils.typeOf(subclass)
                  }
              }): Callable[Iterable[ref.LibraryPath]])
              .finishOnUiThread(
                ModalityState.defaultModalityState(),
                subclasses => {
                  if (subclasses.isEmpty) {
                    PopupUtils.createErrorPopupAtMouse(
                      s"${blockPyClass.getName} has no non-abstract subclasses",
                      this
                    )
                  } else {
                    val config = DseService(project).getOrCreateRunConfiguration(rootClass, this)
                    config.options.searchConfigs =
                      config.options.searchConfigs :+ DseSubclassSearch(path, subclasses.toSeq)
                    DseService(project).onSearchConfigChanged(config, true)
                  }
                }
              )
              .inSmartMode(project)
              .submit(AppExecutorUtil.getAppExecutorService)
          }
        },
        f"Search subclasses of ${refinementClass.toSimpleString}"
      )
    )
    add(
      ContextMenuUtils.MenuItemFromErrorable(
        exceptable {
          requireExcept(block.params.toSeqMap.contains("matching_parts"), "block must have matching_parts")
          () => {
            val config = DseService(project).getOrCreateRunConfiguration(rootClass, this)
            config.options.searchConfigs = config.options.searchConfigs :+ DseDerivedPartSearch(path)
            DseService(project).onSearchConfigChanged(config, true)
          }
        },
        s"Search matching parts"
      )
    )

    add(
      ContextMenuUtils.MenuItem(
        () => {
          val config = DseService(project).getOrCreateRunConfiguration(rootClass, this)
          config.options.objectives = config.options.objectives :+ DseObjectiveFootprintArea(path)
          DseService(project).onObjectiveConfigChanged(config, true)
        },
        s"Add objective area"
      )
    )
    add(
      ContextMenuUtils.MenuItem(
        () => {
          val config = DseService(project).getOrCreateRunConfiguration(rootClass, this)
          config.options.objectives = config.options.objectives ++ Seq(DseObjectiveFootprintCount(path))
          DseService(project).onObjectiveConfigChanged(config, true)
        },
        "Add objective component count"
      )
    )
    if (path == DesignPath()) { // price only supported at top level for now
      add(
        ContextMenuUtils.MenuItem(
          () => {
            val config = DseService(project).getOrCreateRunConfiguration(rootClass, this)
            config.options.objectives = config.options.objectives ++ Seq(DseObjectivePrice())
            DseService(project).onObjectiveConfigChanged(config, true)
          },
          "Add objective price"
        )
      )
    }
    add(
      ContextMenuUtils.MenuItem(
        () => {
          val config = DseService(project).getOrCreateRunConfiguration(rootClass, this)
          config.options.objectives = config.options.objectives ++ Seq(DseObjectiveUnprovenCount(path))
          DseService(project).onObjectiveConfigChanged(config, true)
        },
        "Add unproven count"
      )
    )
  }
}

class DesignPortPopupMenu(path: DesignPath, interface: ToolInterface)
    extends JPopupMenu
    with NavigationPopupMenu {
  def addGotoConnectItems(path: DesignPath, design: schema.Design, project: Project): Unit = {
    val placeholder = ContextMenuUtils.MenuItem(() => {}, "Goto Connect (searching...)")
    placeholder.setEnabled(false)
    add(placeholder)

    ReadAction
      .nonBlocking(
        (
            () =>
              exceptable {
                val assigns = DesignAnalysisUtils.allConnectsTo(path, design, project).exceptError
                assigns.map { assign =>
                  val fileLine = PsiUtils
                    .fileLineOf(assign, project)
                    .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
                  (s"Goto Connect$fileLine", () => assign.navigate(true))
                }
              }
        ): Callable[Errorable[Seq[(String, () => Unit)]]]
      )
      .finishOnUiThread(
        ModalityState.defaultModalityState(),
        result => {
          val insertionIndex = this.getComponentIndex(placeholder)
          ContextMenuUtils
            .MenuItemsFromErrorableSeq(result, s"Goto Connect")
            .reverse
            .foreach(insert(_, insertionIndex))
          this.remove(placeholder)
          this.revalidate()
          this.repaint()
        }
      )
      .inSmartMode(project)
      .submit(AppExecutorUtil.getAppExecutorService)
  }

  private val project = interface.getProject
  private val port = EdgirUtils.resolveExact(path, interface.getDesign).getOrElse {
    PopupUtils.createErrorPopupAtMouse(f"internal error: no port at $path", this)
    throw new Exception()
  }
  private val portClass = port match {
    case port: elem.Port       => port.getSelfClass
    case bundle: elem.Bundle   => bundle.getSelfClass
    case array: elem.PortArray => array.getSelfClass
    case other =>
      PopupUtils.createErrorPopupAtMouse(f"internal error: unknown ${other.getClass} at $path", this)
      throw new Exception()
  }
  add(new JLabel(s"Design Port: ${portClass.toSimpleString} at $path"))
  addSeparator()

  val startConnectAction = exceptable {
    val connectTool = ConnectTool(interface, path).exceptError
    () => interface.startNewTool(connectTool)
  }
  private val startConnectItem = ContextMenuUtils.MenuItemFromErrorable(startConnectAction, "Start Connect")
  add(startConnectItem)
  addSeparator()

  addGotoInstantiationItems(path, interface.getDesign, project)
  addGotoDefinitionItem(portClass, project)
  addGotoConnectItems(path, interface.getDesign, project)

}

class DefaultTool(val interface: ToolInterface) extends BaseTool {
  override def init(): Unit = {
    super.init()
  }

  // Mouse event that is generated on any mouse event in either the design tree or graph layout
  override def onPathMouse(e: MouseEvent, path: DesignPath): Unit = {
    val resolved = EdgirUtils.resolveExact(path, interface.getDesign)

    if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 1) {
      interface.setSelection(path)
    } else if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2) {
      // double click
      resolved match {
        case Some(_: elem.HierarchyBlock) => // blocks: quick set focus
          exceptionPopup(e) {
            new DesignBlockPopupMenu(path, interface).setFocusAction.exceptError()
          }
        case Some(_: elem.Port | _: elem.Bundle | _: elem.PortArray) => // ports: start connect
          exceptionPopup(e) {
            new DesignPortPopupMenu(path, interface).startConnectAction.exceptError()
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
        case _ => // TODO support other element types
      }
    }
  }
}
