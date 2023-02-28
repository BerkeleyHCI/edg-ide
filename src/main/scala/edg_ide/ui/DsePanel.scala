package edg_ide.ui

import com.intellij.openapi.project.Project
import com.intellij.ui.JBSplitter
import com.intellij.ui.components.{JBScrollPane, JBTabbedPane}
import com.intellij.ui.dsl.builder.impl.CollapsibleTitledSeparator
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.intellij.util.concurrency.AppExecutorUtil
import edg.util.Errorable
import edg_ide.dse._
import edg_ide.psi_edits.{InsertAction, InsertRefinementAction}
import edg_ide.runner.DseRunConfiguration
import edg_ide.swing._
import edg_ide.swing.dse.{DseConfigTreeNode, DseConfigTreeTableModel, DseResultNodeBase, DseResultTreeNode, DseResultTreeTableModel}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption}
import edg_ide.util.{DesignAnalysisUtils, exceptable, exceptionPopup}

import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.{GridBagConstraints, GridBagLayout}
import java.util.concurrent.TimeUnit
import javax.swing.tree.TreePath
import javax.swing.{JPanel, JPopupMenu, SwingUtilities}


object DseSearchConfigPopupMenu {
  def createParamSearchEditPopup(searchConfig: DseConfigElement, project: Project,
                                 newConfigFn: DseConfigElement => Unit): Errorable[() => Unit] = exceptable {
    val parseableSearchConfig = searchConfig.instanceOfExcept[DseParameterSearch]("not an editable config type")
    val initialValue = parseableSearchConfig.valuesToString()

    () => PopupUtils.createStringEntryPopup("Search Values", project, initialValue) { text => exceptable {
          val parsed = parseableSearchConfig.valuesStringToConfig(text).exceptError
          newConfigFn(parsed)
    } }
  }
}


class DseSearchConfigPopupMenu(searchConfig: DseConfigElement, project: Project) extends JPopupMenu {
  add(ContextMenuUtils.MenuItemFromErrorable(exceptable {
    val dseConfig = BlockVisualizerService(project).getDseRunConfiguration.exceptNone("no run config")
    val originalSearchConfigs = dseConfig.options.searchConfigs
    val found = originalSearchConfigs.find(searchConfig == _).exceptNone("search config not in config")
    () => {
      dseConfig.options.searchConfigs = originalSearchConfigs.filter(_ != found)
      BlockVisualizerService(project).onDseConfigChanged(dseConfig)
    }
  }, s"Delete"))

  add(ContextMenuUtils.MenuItemFromErrorable(
    DseSearchConfigPopupMenu.createParamSearchEditPopup(searchConfig, project, { newConfig =>
        exceptionPopup.atMouse(this) {
          val dseConfig = BlockVisualizerService(project).getDseRunConfiguration.exceptNone("no run config")
          val originalSearchConfigs = dseConfig.options.searchConfigs
          val index = originalSearchConfigs.indexOf(searchConfig).exceptEquals(-1, "config not found")
          val newSearchConfigs = originalSearchConfigs.patch(index, Seq(newConfig), 1)
          dseConfig.options.searchConfigs = newSearchConfigs
          BlockVisualizerService(project).onDseConfigChanged(dseConfig)
        }
    }), s"Edit"))
}


class DseObjectivePopupMenu(objective: DseObjective, project: Project) extends JPopupMenu {
  add(ContextMenuUtils.MenuItemFromErrorable(exceptable {
    val dseConfig = BlockVisualizerService(project).getDseRunConfiguration.exceptNone("no run config")
    val originalObjectives = dseConfig.options.objectives
    () => {
      dseConfig.options.objectives = originalObjectives.filter(_ != objective)
      BlockVisualizerService(project).onDseConfigChanged(dseConfig)
    }
  }, s"Delete"))
}


class DseResultPopupMenu(result: DseResult, project: Project) extends JPopupMenu {
  add(ContextMenuUtils.MenuItemFromErrorable(exceptable {
    val topType = result.compiled.getContents.getSelfClass
    val topClass = DesignAnalysisUtils.pyClassOf(topType, project).exceptError

    val insertAction = new InsertRefinementAction(project, topClass)
      .createInsertRefinements(result.searchRefinements).exceptError
    () => { // TODO standardized continuation?
      val inserted = insertAction().head
      InsertAction.navigateToEnd(inserted)
    }
  }, s"Insert refinements"))
}


class DsePanel(project: Project) extends JPanel {
  // currently displayed config
  private var displayedConfig: Option[DseRunConfiguration] = None

  // Regularly check the selected run config so the panel contents are kept in sync
  AppExecutorUtil.getAppScheduledExecutorService.scheduleWithFixedDelay(() => {
    val newConfig = BlockVisualizerService(project).getDseRunConfiguration
    if (newConfig != displayedConfig) {
      displayedConfig = newConfig
      onConfigUpdate()
    }
  }, 333, 333, TimeUnit.MILLISECONDS)  // seems flakey without initial delay

  protected def onConfigUpdate(): Unit = {
    displayedConfig match {
      case Some(config) =>
        separator.setText(f"Design Space Exploration: ${config.getName}")
        TreeTableUtils.updateModel(configTree,
          new DseConfigTreeTableModel(config.options.searchConfigs, config.options.objectives))
        TreeTableUtils.updateModel(resultsTree,
          new DseResultTreeTableModel(new CombinedDseResultSet(Seq()), Seq(), false))  // clear existing data
      case _ =>
        separator.setText(f"Design Space Exploration: no run config selected")
        TreeTableUtils.updateModel(configTree,
          new DseConfigTreeTableModel(Seq(), Seq()))
        TreeTableUtils.updateModel(resultsTree,
          new DseResultTreeTableModel(new CombinedDseResultSet(Seq()), Seq(), false))  // clear existing data
    }
  }

  setLayout(new GridBagLayout())

  // TODO make the collapse function actually work
  private val separator = new CollapsibleTitledSeparator("Design Space Exploration")
  add(separator, Gbc(0, 0, GridBagConstraints.HORIZONTAL))

  private val mainSplitter = new JBSplitter(true, 0.5f, 0.1f, 0.9f)
  add(mainSplitter, Gbc(0, 1, GridBagConstraints.BOTH))

  // GUI: Top plot
  private val plot = new DsePlotPanel() {
    override def onClick(data: Seq[DseResult]): Unit = {
      resultsTree.clearSelection()

      val treeRoot = resultsTree.getTableModel.asInstanceOf[DseResultTreeTableModel].rootNode
      def getTreePathsForResults(path: TreePath, node: DseResultNodeBase): Seq[TreePath] = node match {
        case node: treeRoot.ResultSetNode => node.children.flatMap(getTreePathsForResults(path.pathByAddingChild(node), _))
        case node: treeRoot.ResultNode if data.contains(node.result) => Seq(path)
        case _ => Seq()
      }
      val treeRootPath = new TreePath(treeRoot)
      val dataTreePaths = treeRoot.children.flatMap(getTreePathsForResults(treeRootPath, _))

      dataTreePaths.foreach { treePath =>
        resultsTree.addSelectedPath(treePath)
        resultsTree.getTree.expandPath(treePath)

      }
      dataTreePaths.headOption.foreach { treePath =>  // scroll to the first result
        resultsTree.scrollRectToVisible(resultsTree.getTree.getPathBounds(treePath))
      }

      setSelection(data)
    }

    override def onHoverChange(data: Seq[DseResult]): Unit = {
      // TODO something w/ results tree selection?
    }
  }
  mainSplitter.setFirstComponent(plot)

  // GUI: Bottom tabs

  private val tabbedPane = new JBTabbedPane()
  mainSplitter.setSecondComponent(tabbedPane)

  // GUI: Bottom Tabs: Config
  //
  private val configTree = new TreeTable(new DseConfigTreeTableModel(Seq(), Seq()))
  configTree.setShowColumns(true)
  configTree.setRootVisible(false)
  configTree.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      val selectedTreePath = TreeTableUtils.getPathForRowLocation(configTree, e.getX, e.getY).getOrElse(return)
      selectedTreePath.getLastPathComponent match {
        case node: DseConfigTreeNode.DseSearchConfigNode =>
          if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) {
            new DseSearchConfigPopupMenu(node.config, project).show(e.getComponent, e.getX, e.getY)
          }
        case node: DseConfigTreeNode.DseObjectiveNode =>
          if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) {
            new DseObjectivePopupMenu(node.config, project).show(e.getComponent, e.getX, e.getY)
          }
        case _ =>  // any other type ignored
      }
    }
  })
  tabbedPane.addTab("Config", new JBScrollPane(configTree))

  // GUI: Bottom Tabs: Results
  //
  private val resultsTree = new TreeTable(new DseResultTreeTableModel(new CombinedDseResultSet(Seq()), Seq(), false))
  resultsTree.setShowColumns(true)
  resultsTree.setRootVisible(false)
  resultsTree.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      val selectedTreePath = TreeTableUtils.getPathForRowLocation(resultsTree, e.getX, e.getY).getOrElse(return)
      selectedTreePath.getLastPathComponent match {
        case node: DseResultTreeNode#ResultSetNode =>
          if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 1) { // single click, highlight all in chart
            plot.setSelection(node.setMembers)
          }
        case node: DseResultTreeNode#ResultNode =>
          if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 1) { // single click, highlight in chart
            plot.setSelection(Seq(node.result))
          } else if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) {  // right click popup menu
            new DseResultPopupMenu(node.result, project).show(e.getComponent, e.getX, e.getY)
          } else if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2) { // double click
            val result = node.result
            BlockVisualizerService(project).setDesignTop(result.compiled, result.compiler,
              result.compiler.refinements.toPb, result.errors, Some(f"DSE ${result.index}: "))
          }
        case _ => // any other type ignored
      }
    }
  })
  tabbedPane.addTab("Results", new JBScrollPane(resultsTree))

  onConfigUpdate()  // set initial state

  // Data state update
  def onConfigChange(config: DseRunConfiguration): Unit = {
    if (displayedConfig.contains(config)) {
      onConfigUpdate()
    }
  }

  def setResults(results: Seq[DseResult], objectives: Seq[DseObjective], inProgress: Boolean): Unit = {
    val combinedResults = new CombinedDseResultSet(results)
    TreeTableUtils.updateModel(resultsTree,
      new DseResultTreeTableModel(combinedResults, objectives, inProgress))
    plot.setResults(combinedResults, objectives)
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
    state.dseTabIndex = tabbedPane.getSelectedIndex
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
    tabbedPane.setSelectedIndex(state.dseTabIndex)
  }
}
