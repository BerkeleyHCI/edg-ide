package edg_ide.ui

import com.intellij.openapi.project.Project
import com.intellij.ui.components.JBScrollPane
import com.intellij.ui.treeStructure.treetable.TreeTable
import edg.EdgirUtils.SimpleLibraryPath
import edg.compiler.{Compiler, ExprToString}
import edg.wir.{DesignPath, IndirectDesignPath}
import edg_ide.dse.{DseClassParameterSearch, DseFeature, DseObjectiveParameter, DsePathParameterSearch}
import edg_ide.{EdgirUtils, swing}
import edg_ide.swing.{ElementDetailTreeModel, TreeTableUtils}
import edg_ide.util.ExceptionNotifyImplicits.ExceptOption
import edg_ide.util.{DesignAnalysisUtils, exceptable, requireExcept}
import edgir.schema.schema
import edgrpc.hdl.{hdl => edgrpc}

import java.awt.BorderLayout
import java.awt.event.{MouseAdapter, MouseEvent}
import javax.swing.{JPanel, JPopupMenu, SwingUtilities}


class DetailParamPopupMenu(path: IndirectDesignPath, design: schema.Design, compiler: Compiler, project: Project) extends JPopupMenu {
  val rootClass = design.getContents.getSelfClass

  add(ContextMenuUtils.MenuItemFromErrorable(exceptable {
    val directPath = DesignPath.fromIndirectOption(path).exceptNone("not a direct parameter")
    val value = compiler.getParamValue(path).exceptNone("no value")
    () => {
      val config = BlockVisualizerService(project).getOrCreateDseRunConfiguration(rootClass)
      config.options.searchConfigs = config.options.searchConfigs ++
          Seq(DsePathParameterSearch(directPath, Seq(value)))
      BlockVisualizerService(project).onDseConfigChanged(config)
    }
  }, s"Search values for instance $path"))

  add(ContextMenuUtils.MenuItemNamedFromErrorable(exceptable {
    val directPath = DesignPath.fromIndirectOption(path).exceptNone("not a direct parameter")
    val (blockPath, block) = EdgirUtils.resolveDeepestBlock(directPath, design)
    val blockClass = block.getSelfClass
    val postfix = directPath.postfixFromOption(blockPath).get
    requireExcept(postfix.steps.size == 1, "not a direct parameter of a block")
    val value = compiler.getParamValue(path).exceptNone("no value")
    (() => {
      val config = BlockVisualizerService(project).getOrCreateDseRunConfiguration(rootClass)
      config.options.searchConfigs = config.options.searchConfigs ++
          Seq(DseClassParameterSearch(blockClass, postfix, Seq(value)))
      BlockVisualizerService(project).onDseConfigChanged(config)
    }, s"Search values of class ${blockClass.toSimpleString}:${ExprToString(postfix)}")
  }, s"Search values of class"))

  add(ContextMenuUtils.MenuItemFromErrorable(exceptable {
    val objective = compiler.getParamType(path) match {
      case Some(paramType) => DseObjectiveParameter(path, paramType)
      case _ => exceptable.fail(f"no parameter type at $path")
    }

    () => PopupUtils.createStringEntryPopup("Name", project) { text => exceptable {
      val config = BlockVisualizerService(project).getOrCreateDseRunConfiguration(rootClass)
      config.options.objectives = config.options.objectives ++ Seq((text, objective))
      BlockVisualizerService(project).onDseConfigChanged(config)
    } }
  }, "Add objective"))
}


// TODO: remove initCompiler, it's counterintuitive
class DetailPanel(initPath: DesignPath, initCompiler: Compiler, project: Project) extends JPanel {
  private val tree = new TreeTable(new ElementDetailTreeModel(initPath, schema.Design(), edgrpc.Refinements(), initCompiler))
  tree.setShowColumns(true)
  private val treeScrollPane = new JBScrollPane(tree)

  setLayout(new BorderLayout())
  add(treeScrollPane)

  private val treeMouseListener = new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = {
      val selectedTreePath = TreeTableUtils.getPathForRowLocation(tree, e.getX, e.getY).getOrElse(return)
      selectedTreePath.getLastPathComponent match {
        case selected: swing.ElementDetailNodes#ParamNode => // insert actions / menu for blocks
          if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) {  // right click context menu
            if (DseFeature.kEnabled) {
              new DetailParamPopupMenu(selected.path, selected.outer.root, selected.outer.compiler, project)
                  .show(e.getComponent, e.getX, e.getY)
            }
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

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
  }
}
