package edg_ide.ui

import com.intellij.openapi.project.Project
import com.intellij.ui.components.JBScrollPane
import com.intellij.ui.treeStructure.treetable.TreeTable
import edg.EdgirUtils.SimpleLibraryPath
import edg.compiler.{Compiler, ExprToString}
import edg.wir.ProtoUtil.ParamProtoToSeqMap
import edg.wir.{DesignPath, IndirectDesignPath, Refinements}
import edg_ide.dse.{DseClassParameterSearch, DseFeature, DseObjectiveParameter, DsePathParameterSearch}
import edg_ide.psi_edits.{InsertAction, InsertRefinementAction}
import edg_ide.swing.{ElementDetailTreeModel, TreeTableUtils}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptOption, ExceptSeq}
import edg_ide.util.{DesignAnalysisUtils, exceptable, requireExcept}
import edg_ide.{EdgirUtils, swing}
import edgir.schema.schema
import edgrpc.hdl.{hdl => edgrpc}

import java.awt.BorderLayout
import java.awt.event.{MouseAdapter, MouseEvent}
import javax.swing.{JPanel, JPopupMenu, SwingUtilities}


class DetailParamPopupMenu(path: IndirectDesignPath, design: schema.Design, compiler: Compiler, project: Project) extends JPopupMenu {
  private val rootClass = design.getContents.getSelfClass
  private val rootPyClass = DesignAnalysisUtils.pyClassOf(rootClass, project)

  add(ContextMenuUtils.MenuItemFromErrorable(exceptable {
    val value = compiler.getParamValue(path).exceptNone("no value")
    val insertAction = new InsertRefinementAction(project, rootPyClass.exceptError).createInsertRefinements(new Refinements(
      instanceValues = Map(DesignPath.fromIndirectOption(path).exceptNone("not a direct parameter") -> value)
    )).exceptError
    () => {
      val inserted = insertAction().head
      InsertAction.navigateToEnd(inserted)
    }
  }, s"Insert refinement for instance $path"))

  // Determine the user-defined (pre-refinement) class for class-based refinements
  private val blockClassPostfix = exceptable {
    val directPath = DesignPath.fromIndirectOption(path).exceptNone("not a direct parameter")
    val (blockPath, block) = EdgirUtils.resolveDeepestBlock(directPath, design)
    val blockClass = block.getPrerefineClass
    val postfix = directPath.postfixFromOption(blockPath).get
    val paramName = postfix.steps.onlyExcept("not a direct parameter of a block").getName
    requireExcept(block.params.get(paramName).isDefined, f"${blockClass.toSimpleString} does not have $paramName")
    (blockClass, postfix)
  }

  add(ContextMenuUtils.MenuItemNamedFromErrorable(exceptable {
    val (blockClass, postfix) = blockClassPostfix.exceptError
    val value = compiler.getParamValue(path).exceptNone("no value")

    val insertAction = new InsertRefinementAction(project, rootPyClass.exceptError).createInsertRefinements(new Refinements(
      classValues = Map((blockClass, postfix) -> value)
    )).exceptError
    (() => {
      val inserted = insertAction().head
      InsertAction.navigateToEnd(inserted)
    }, s"Insert refinement for class ${blockClass.toSimpleString}:${ExprToString(postfix)}")
  }, s"Insert refinement for class"))

  // Determine the param-defining class for class-based refinements
  private val paramDefiningClassPostfix = exceptable {
    val directPath = DesignPath.fromIndirectOption(path).exceptNone("not a direct parameter")
    val (blockPath, block) = EdgirUtils.resolveDeepestBlock(directPath, design)
    val postfix = directPath.postfixFromOption(blockPath).get
    val paramName = postfix.steps.onlyExcept("not a direct parameter of a block").getName
    val paramDefiningClass = compiler.library.blockParamGetDefiningSuperclass(block.getSelfClass, paramName)
      .exceptNone("no param-defining class")
    (paramDefiningClass, postfix)
  }

  add(ContextMenuUtils.MenuItemNamedFromErrorable(exceptable {
    val (paramDefiningClass, postfix) = paramDefiningClassPostfix.exceptError
    val value = compiler.getParamValue(path).exceptNone("no value")

    val insertAction = new InsertRefinementAction(project, rootPyClass.exceptError).createInsertRefinements(new Refinements(
      classValues = Map((paramDefiningClass, postfix) -> value)
    )).exceptError
    (() => {
      val inserted = insertAction().head
      InsertAction.navigateToEnd(inserted)
    }, s"Insert refinement for param-defining class ${paramDefiningClass.toSimpleString}:${ExprToString(postfix)}")
  }, s"Insert refinement for param-defining class"))

  if (DseFeature.kEnabled) {
    addSeparator()
    add(ContextMenuUtils.MenuItemFromErrorable(exceptable {
      val directPath = DesignPath.fromIndirectOption(path).exceptNone("not a direct parameter")
      val value = compiler.getParamValue(path).exceptNone("no value")
      val baseConfig = DsePathParameterSearch(directPath, Seq(value))
      DseSearchConfigPopupMenu.createParamSearchEditPopup(baseConfig, project, { newConfig =>
        DseService(project).addConfig(rootClass, newConfig)
      }).exceptError
    }, s"Search values for instance $path"))

    add(ContextMenuUtils.MenuItemNamedFromErrorable(exceptable {
      val (blockClass, postfix) = blockClassPostfix.exceptError
      val value = compiler.getParamValue(path).exceptNone("no value")
      val baseConfig = DseClassParameterSearch(blockClass, postfix, Seq(value))
      (DseSearchConfigPopupMenu.createParamSearchEditPopup(baseConfig, project, { newConfig =>
        DseService(project).addConfig(rootClass, newConfig)
      }).exceptError, s"Search values of class ${blockClass.toSimpleString}:${ExprToString(postfix)}")
    }, s"Search values of class"))

    add(ContextMenuUtils.MenuItemNamedFromErrorable(exceptable {
      val (paramDefiningClass, postfix) = paramDefiningClassPostfix.exceptError
      val value = compiler.getParamValue(path).exceptNone("no value")
      val baseConfig = DseClassParameterSearch(paramDefiningClass, postfix, Seq(value))
      (DseSearchConfigPopupMenu.createParamSearchEditPopup(baseConfig, project, { newConfig =>
        DseService(project).addConfig(rootClass, newConfig)
      }).exceptError, s"Search values of param-defining class ${paramDefiningClass.toSimpleString}:${ExprToString(postfix)}")
    }, s"Search values of param-defining class"))

    addSeparator()
    add(ContextMenuUtils.MenuItemFromErrorable(exceptable {
      val objective = compiler.getParamType(path) match {
        case Some(paramType) => DseObjectiveParameter(path, paramType)
        case _ => exceptable.fail(f"no parameter type at $path")
      }

      () => {
        val config = DseService(project).getOrCreateRunConfiguration(rootClass)
        config.options.objectives = config.options.objectives :+ objective
        DseService(project).onObjectiveConfigChanged(config)
      }
    }, "Add objective"))
  }
}


// TODO: remove initCompiler, it's counterintuitive
class DetailPanel(initPath: DesignPath, initCompiler: Compiler, project: Project) extends JPanel {
  private val tree = new TreeTable(new ElementDetailTreeModel(initPath, schema.Design(), edgrpc.Refinements(), initCompiler))
  tree.setShowColumns(true)
  private val treeScrollPane = new JBScrollPane(tree)
  private val treeTreeRenderer = tree.getTree.getCellRenderer
  private val treeTableRenderer = tree.getDefaultRenderer(classOf[Object])

  setLayout(new BorderLayout())
  add(treeScrollPane)

  private val treeMouseListener = new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = {
      val selectedTreePath = TreeTableUtils.getPathForRowLocation(tree, e.getX, e.getY).getOrElse(return)
      selectedTreePath.getLastPathComponent match {
        case selected: swing.ElementDetailNodes#ParamNode => // insert actions / menu for blocks
          if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) {  // right click context menu
            new DetailParamPopupMenu(selected.path, selected.outer.root, selected.outer.compiler, project)
                .show(e.getComponent, e.getX, e.getY)
          }

        case _ => // any other type ignored
      }
    }
  }
  tree.addMouseListener(treeMouseListener)

  // Actions
  //
  def setLoaded(path: DesignPath, root: schema.Design, refinements: edgrpc.Refinements, compiler: Compiler): Unit = {
    TreeTableUtils.updateModel(tree, new ElementDetailTreeModel(path, root, refinements, compiler))
  }

  def setStale(stale: Boolean): Unit = {
    if (stale) {
      tree.setTreeCellRenderer(new StaleTreeRenderer)
      tree.setDefaultRenderer(classOf[Object], new StaleTableRenderer)
    } else {
      tree.setTreeCellRenderer(treeTreeRenderer)
      tree.setDefaultRenderer(classOf[Object], treeTableRenderer)
    }
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
  }
}
