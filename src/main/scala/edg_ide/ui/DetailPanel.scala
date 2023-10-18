package edg_ide.ui

import com.intellij.openapi.application.ApplicationManager
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
import edg_ide.util.{DesignAnalysisUtils, LibraryUtils, exceptable, requireExcept}
import edg_ide.{EdgirUtils, swing}
import edgir.schema.schema
import edgrpc.hdl.{hdl => edgrpc}

import java.awt.datatransfer.StringSelection
import java.awt.{BorderLayout, Toolkit}
import java.awt.event.{KeyAdapter, KeyEvent, MouseAdapter, MouseEvent}
import javax.swing.{JPanel, JPopupMenu, SwingUtilities}

class DetailParamPopupMenu(
    path: IndirectDesignPath,
    design: schema.Design,
    compiler: Compiler,
    project: Project
) extends JPopupMenu {
  private val rootClass = design.getContents.getSelfClass
  private val rootPyClass = DesignAnalysisUtils.pyClassOf(rootClass, project)

  add(
    ContextMenuUtils.MenuItemFromErrorable(
      exceptable {
        val value = compiler.getParamValue(path).exceptNone("no value")
        val insertAction =
          new InsertRefinementAction(project, rootPyClass.exceptError)
            .createInsertRefinements(
              new Refinements(
                instanceValues =
                  Map(DesignPath.fromIndirectOption(path).exceptNone("not a direct param") -> value)
              )
            )
            .exceptError
        () => {
          val inserted = insertAction().head
          InsertAction.navigateToEnd(inserted)
        }
      },
      s"Insert refinement"
    )
  )

  // Determine the param-defining class for class-based refinements
  private val paramDefiningClassPostfix = exceptable {
    val directPath = DesignPath.fromIndirectOption(path).exceptNone("not a direct param")
    val (blockPath, block) = EdgirUtils.resolveDeepestBlock(directPath, design)
    val postfix = directPath.postfixFromOption(blockPath).get
    val paramName = postfix.steps.onlyExcept("not a block param").getName
    val paramDefiningClass =
      LibraryUtils.blockParamGetDefiningSuperclass(compiler.library, block.getSelfClass, paramName)
        .exceptNone("no param-defining class")
    (paramDefiningClass, postfix)
  }

  add(
    ContextMenuUtils.MenuItemNamedFromErrorable(
      exceptable {
        val (paramDefiningClass, postfix) = paramDefiningClassPostfix.exceptError
        val value = compiler.getParamValue(path).exceptNone("no value")

        val insertAction =
          new InsertRefinementAction(project, rootPyClass.exceptError)
            .createInsertRefinements(
              new Refinements(
                classValues = Map((paramDefiningClass, postfix) -> value)
              )
            )
            .exceptError
        (
          () => {
            val inserted = insertAction().head
            InsertAction.navigateToEnd(inserted)
          },
          s"Insert refinement for all ${paramDefiningClass.toSimpleString}:${ExprToString(postfix)}"
        )
      },
      s"Insert refinement for param-defining class"
    )
  )

  if (DseFeature.kEnabled) {
    addSeparator()
    add(
      ContextMenuUtils.MenuItemFromErrorable(
        exceptable {
          val directPath = DesignPath.fromIndirectOption(path).exceptNone("not a direct param")
          val value = compiler.getParamValue(path).exceptNone("no value")
          val baseConfig = DsePathParameterSearch(directPath, Seq(value))
          DseSearchConfigPopupMenu
            .createParamSearchEditPopup(
              baseConfig,
              project,
              { newConfig =>
                DseService(project).addSearchConfig(rootClass, newConfig, this)
              }
            )
            .exceptError
        },
        s"Search values"
      )
    )

    add(
      ContextMenuUtils.MenuItemNamedFromErrorable(
        exceptable {
          val (paramDefiningClass, postfix) = paramDefiningClassPostfix.exceptError
          val value = compiler.getParamValue(path).exceptNone("no value")
          val baseConfig = DseClassParameterSearch(paramDefiningClass, postfix, Seq(value))
          (
            DseSearchConfigPopupMenu
              .createParamSearchEditPopup(
                baseConfig,
                project,
                { newConfig =>
                  DseService(project).addSearchConfig(rootClass, newConfig, this)
                }
              )
              .exceptError,
            s"Search values for all ${paramDefiningClass.toSimpleString}:${ExprToString(postfix)}"
          )
        },
        s"Search values of param-defining class"
      )
    )

    addSeparator()
    add(
      ContextMenuUtils.MenuItemFromErrorable(
        exceptable {
          val objective = compiler.getParamType(path) match {
            case Some(paramType) => DseObjectiveParameter(path, paramType)
            case _ => exceptable.fail(f"no parameter type at $path")
          }

          () => {
            val config = DseService(project).getOrCreateRunConfiguration(rootClass, this)
            config.options.objectives = config.options.objectives :+ objective
            DseService(project).onObjectiveConfigChanged(config, true)
          }
        },
        "Add objective"
      )
    )
  }
}

// TODO: remove initCompiler, it's counterintuitive
class DetailPanel(initPath: DesignPath, initCompiler: Compiler, project: Project) extends JPanel {
  private val tree =
    new TreeTable(new ElementDetailTreeModel(initPath, schema.Design(), edgrpc.Refinements(), initCompiler))
  tree.setShowColumns(true)
  private val treeScrollPane = new JBScrollPane(tree)
  private val treeTreeRenderer = tree.getTree.getCellRenderer
  private val treeTableRenderer = tree.getDefaultRenderer(classOf[Object])
  private var compiler = initCompiler

  setLayout(new BorderLayout())
  add(treeScrollPane)

  tree.addMouseListener(new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = {
      val selectedTreePath = TreeTableUtils
        .getPathForRowLocation(tree, e.getX, e.getY)
        .getOrElse(
          return
        )
      selectedTreePath.getLastPathComponent match {
        case selected: swing.ElementDetailNodes#ParamNode => // insert actions / menu for blocks
          if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) { // right click context menu
            new DetailParamPopupMenu(selected.path, selected.outer.root, selected.outer.compiler, project)
              .show(e.getComponent, e.getX, e.getY)
          }

        case _ => // any other type ignored
      }
    }
  })

  tree.addKeyListener(new KeyAdapter {
    override def keyPressed(e: KeyEvent): Unit = {
      if (e.getKeyCode == KeyEvent.VK_C) {
        e.consume()

        val copyString = tree.getTree.getLastSelectedPathComponent match {
          case selected: swing.ElementDetailNodes#ParamNode => // insert actions / menu for blocks
            compiler.getParamValue(selected.path) match {
              case Some(value) => Some(value.toStringValue)
              case _ => None
            }
          case selected: swing.ElementDetailNodes#ParamEltNode =>
            Some(selected.value.toStringValue)
          case _ => None
        }
        copyString match {
          case Some(copyString) =>
            val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
            clipboard.setContents(new StringSelection(copyString), null)
            PopupUtils.createPopupAtMouse(f"copied '$copyString'", tree)
          case None =>
            PopupUtils.createErrorPopupAtMouse("no value to copy", tree)
        }
      }
    }
  })

  // Actions
  //
  def setLoaded(
      path: DesignPath,
      root: schema.Design,
      refinements: edgrpc.Refinements,
      newCompiler: Compiler
  ): Unit = {
    ApplicationManager.getApplication.invokeLater(() => {
      TreeTableUtils.updateModel(tree, new ElementDetailTreeModel(path, root, refinements, newCompiler))
      compiler = newCompiler
    })
  }

  def setStale(stale: Boolean): Unit = {
    ApplicationManager.getApplication.invokeLater(() => {
      if (stale) {
        tree.setTreeCellRenderer(new StaleTreeRenderer)
        tree.setDefaultRenderer(classOf[Object], new StaleTableRenderer)
      } else {
        tree.setTreeCellRenderer(treeTreeRenderer)
        tree.setDefaultRenderer(classOf[Object], treeTableRenderer)
      }
    })
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {}

  def loadState(state: BlockVisualizerServiceState): Unit = {}
}
