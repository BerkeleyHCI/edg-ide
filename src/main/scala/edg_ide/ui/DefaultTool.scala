package edg_ide.ui

import com.intellij.openapi.application.{ModalityState, ReadAction}
import com.intellij.openapi.project.Project
import com.intellij.util.concurrency.AppExecutorUtil
import com.jetbrains.python.psi.search.PyClassInheritorsSearch
import edg.EdgirUtils.SimpleLibraryPath
import edg.util.Errorable
import edg.wir.DesignPath
import edg.wir.ProtoUtil.ParamProtoToSeqMap
import edg_ide.dse._
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptOption}
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

    ContextMenuUtils.MenuItemsFromErrorableSeq(actionPairs, s"Goto Instantiation")
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

    val gotoDefinitionItem = ContextMenuUtils.MenuItemFromErrorable(action, actionName)
    add(gotoDefinitionItem)
  }
}


class DesignBlockPopupMenu(path: DesignPath, interface: ToolInterface)
    extends JPopupMenu with NavigationPopupMenu {
  private val project = interface.getProject
  private val block = Errorable(EdgirUtils.resolveExactBlock(path, interface.getDesign), "no block at path")
  private val blockClass = block.map(_.getSelfClass)

  add(new JLabel(s"Design Block: ${blockClass.mapToString(_.toSimpleString)} at $path"))
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
  private val setFocusItem = ContextMenuUtils.MenuItemFromErrorable(setFocusAction, setFocusName)
  add(setFocusItem)
  addSeparator()

  addGotoInstantiationItems(path, interface.getDesign, project)
  addGotoDefinitionItem(blockClass, project)

  if (DseFeature.kEnabled) {
    val rootClass = interface.getDesign.getContents.getSelfClass

    addSeparator()

    val (refinementClass, refinementLabel) = block.get.prerefineClass match {
      case Some(prerefineClass) if prerefineClass != block.get.getSelfClass =>
        (prerefineClass, f"Search refinements of base ${prerefineClass.toSimpleString}")
      case _ => (block.get.getSelfClass, f"Search refinements of ${block.get.getSelfClass.toSimpleString}")
    }
    add(ContextMenuUtils.MenuItemFromErrorable(exceptable {
      val blockPyClass = DesignAnalysisUtils.pyClassOf(refinementClass, project).get
      () => {
        ReadAction.nonBlocking((() => {
          val subClasses = PyClassInheritorsSearch.search(blockPyClass, true).findAll().asScala
          subClasses.filter { subclass =>  // filter out abstract blocks
            !subclass.getDecoratorList.getDecorators.exists(_.getName == "abstract_block")
          } .map { subclass =>
            DesignAnalysisUtils.typeOf(subclass)
          }
        }): Callable[Iterable[ref.LibraryPath]]).finishOnUiThread(ModalityState.defaultModalityState(), subclasses => {
          if (subclasses.isEmpty) {
            PopupUtils.createErrorPopupAtMouse(s"${blockPyClass.getName} has no non-abstract subclasses", this)
          } else {
            val config = BlockVisualizerService(project).getOrCreateDseRunConfiguration(rootClass)
            config.options.searchConfigs = config.options.searchConfigs ++ Seq(
              DseSubclassSearch(path, subclasses.toSeq)
            )
            BlockVisualizerService(project).onDseConfigChanged(config)
          }
        }).inSmartMode(project).submit(AppExecutorUtil.getAppExecutorService)
      }
    }, refinementLabel))
    add(ContextMenuUtils.MenuItemFromErrorable(exceptable {
      requireExcept(block.get.params.toSeqMap.contains("matching_parts"), "block must have matching_parts")
      () => {
        val config = BlockVisualizerService(project).getOrCreateDseRunConfiguration(rootClass)
        config.options.searchConfigs = config.options.searchConfigs ++ Seq(DseDerivedPartSearch(path))
        BlockVisualizerService(project).onDseConfigChanged(config)
    }}, "Search matching parts"))

    add(ContextMenuUtils.MenuItemFromErrorable(exceptable {
      () => {
        PopupUtils.createStringEntryPopup("Name", project) { text => exceptable {
          val config = BlockVisualizerService(project).getOrCreateDseRunConfiguration(rootClass)
          config.options.objectives = config.options.objectives ++ Seq((text, DseObjectiveFootprintArea(path)))
          BlockVisualizerService(project).onDseConfigChanged(config)
        } }
      }
    }, "Add objective contained footprint area"))
    add(ContextMenuUtils.MenuItemFromErrorable(exceptable {
      () => {
        PopupUtils.createStringEntryPopup("Name", project) { text => exceptable {
          val config = BlockVisualizerService(project).getOrCreateDseRunConfiguration(rootClass)
          config.options.objectives = config.options.objectives ++ Seq((text, DseObjectiveFootprintCount(path)))
          BlockVisualizerService(project).onDseConfigChanged(config)
        } }
      }
    }, "Add objective contained footprint count"))
  }
  // TODO eg
  // parameters search
  //DseParameterSearch(DesignPath() + "reg_5v" + "ripple_current_factor",
  //      Seq(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5).map(value => RangeValue(value - 0.05, value + 0.05))
  //    ),
  // objective parametrs
  // "inductance" -> DseObjectiveParameter(DesignPath() + "reg_5v" + "power_path" + "inductor" + "actual_inductance"),
}


class DesignPortPopupMenu(path: DesignPath, interface: ToolInterface)
    extends JPopupMenu with NavigationPopupMenu {
  private val portClass = exceptable {
    val port = EdgirUtils.resolveExact(path, interface.getDesign).exceptNone("no port at path")
    // TODO replace w/ EdgirUtils.typeOfPort, but this needs to take a PortLike instead of Any Port
    port match {
      case port: elem.Port => port.getSelfClass
      case bundle: elem.Bundle => bundle.getSelfClass
      case array: elem.PortArray => array.getSelfClass
      case other => throw ExceptionNotifyException(s"unknown ${other.getClass} at path")
    }
  }
  add(new JLabel(s"Design Port: ${portClass.mapToString(_.toSimpleString)} at $path"))
  addSeparator()

  val startConnectAction = exceptable {
    val connectTool = ConnectTool(interface, path).exceptError
    () => interface.startNewTool(connectTool)
  }
  private val startConnectItem = ContextMenuUtils.MenuItemFromErrorable(startConnectAction, "Start Connect")
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

  ContextMenuUtils.MenuItemsFromErrorableSeq(gotoConnectPairs, s"Goto Connect")
      .foreach(add)
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
}
