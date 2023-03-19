package edg_ide.ui

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.project.Project
import com.intellij.ui.{JBSplitter, TitledSeparator}
import com.intellij.ui.components.{JBScrollPane, JBTabbedPane}
import com.intellij.ui.dsl.builder.impl.CollapsibleTitledSeparator
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.intellij.util.concurrency.AppExecutorUtil
import edg.util.Errorable
import edg_ide.dse._
import edg_ide.psi_edits.{InsertAction, InsertRefinementAction}
import edg_ide.runner.DseRunConfiguration
import edg_ide.swing._
import edg_ide.swing.dse.{DseConfigTreeNode, DseConfigTreeTableModel, DseResultNodeBase, DseResultTreeNode, DseResultTreeRenderer, DseResultTreeTableModel}
import edg_ide.ui.dse.{DseBasePlot, DseParallelPlotPanel, DseScatterPlotPanel}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption}
import edg_ide.util.{DesignAnalysisUtils, exceptable, exceptionPopup}

import java.awt.event.{KeyEvent, KeyListener, MouseAdapter, MouseEvent}
import java.awt.{GridBagConstraints, GridBagLayout}
import java.util.concurrent.TimeUnit
import javax.swing.tree.TreePath
import javax.swing.{JLabel, JPanel, JPopupMenu, SwingUtilities}


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
    val dseConfig = DseService(project).getRunConfiguration.exceptNone("no run config")
    val originalSearchConfigs = dseConfig.options.searchConfigs
    () => {
      dseConfig.options.searchConfigs = originalSearchConfigs.filter(_ != searchConfig)
      DseService(project).onSearchConfigChanged(dseConfig, false)
    }
  }, s"Delete"))

  add(ContextMenuUtils.MenuItemFromErrorable(
    DseSearchConfigPopupMenu.createParamSearchEditPopup(searchConfig, project, { newConfig =>
        exceptionPopup.atMouse(this) {
          val dseConfig = DseService(project).getRunConfiguration.exceptNone("no run config")
          val originalSearchConfigs = dseConfig.options.searchConfigs
          val index = originalSearchConfigs.indexOf(searchConfig).exceptEquals(-1, "config not found")
          val newSearchConfigs = originalSearchConfigs.patch(index, Seq(newConfig), 1)
          dseConfig.options.searchConfigs = newSearchConfigs
          DseService(project).onSearchConfigChanged(dseConfig, false)
        }
    }), s"Edit"))
}


class DseObjectivePopupMenu(objective: DseObjective, project: Project) extends JPopupMenu {
  add(ContextMenuUtils.MenuItemFromErrorable(exceptable {
    val dseConfig = DseService(project).getRunConfiguration.exceptNone("no run config")
    val originalObjectives = dseConfig.options.objectives
    () => {
      dseConfig.options.objectives = originalObjectives.filter(_ != objective)
      DseService(project).onObjectiveConfigChanged(dseConfig, false)
    }
  }, s"Delete"))
}


class DseResultPopupMenu(result: DseResult, project: Project) extends JPopupMenu {
  add(new JLabel(s"Point ${result.index}"))
  result.config.foreach { case (config, configValue) =>
    add(new JLabel(s"${config.configToString} = ${config.valueToString(configValue)}"))
  }
  result.objectives.foreach { case (objective, objectiveValue) =>
    add(new JLabel(s"${objective.objectiveToString} = ${DseConfigElement.valueToString(objectiveValue)}"))
  }
  addSeparator()

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
    val newConfig = DseService(project).getRunConfiguration
    if (newConfig != displayedConfig) {
      displayedConfig = newConfig
      onConfigUpdate()
    }
  }, 333, 333, TimeUnit.MILLISECONDS)  // seems flakey without initial delay

  protected def onConfigUpdate(): Unit = {
    ApplicationManager.getApplication.invokeLater(() => {
      displayedConfig match {
        case Some(config) =>
          separator.setText(f"Design Space Exploration: ${config.getName}")
          TreeTableUtils.updateModel(configTree,
            new DseConfigTreeTableModel(config.options.searchConfigs, config.options.objectives))
          TreeTableUtils.updateModel(resultsTree,
            new DseResultTreeTableModel(new CombinedDseResultSet(Seq()), Seq(), false)) // clear existing data
        case _ =>
          separator.setText(f"Design Space Exploration: no run config selected")
          TreeTableUtils.updateModel(configTree,
            new DseConfigTreeTableModel(Seq(), Seq()))
          TreeTableUtils.updateModel(resultsTree,
            new DseResultTreeTableModel(new CombinedDseResultSet(Seq()), Seq(), false)) // clear existing data
      }
    })
  }

  setLayout(new GridBagLayout())

  private val separator = new TitledSeparator("Design Space Exploration")
  add(separator, Gbc(0, 0, GridBagConstraints.HORIZONTAL))

  private val mainSplitter = new JBSplitter(true, 0.5f, 0.1f, 0.9f)
  add(mainSplitter, Gbc(0, 1, GridBagConstraints.BOTH))

  // GUI: Top plot
  def plotOnClick(e: MouseEvent, data: Seq[DseResult]): Unit = {
    // note: some platforms register drag-release as 0 clicks
    if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount <= 1) { // single click, highlight in tree
      resultsTree.clearSelection()

      val treeRoot = resultsTree.getTableModel.asInstanceOf[DseResultTreeTableModel].rootNode

      def getTreePathsForResults(path: TreePath, node: DseResultNodeBase): Seq[TreePath] = node match {
        case node: treeRoot.ResultSetNode => node.children.flatMap(getTreePathsForResults(path.pathByAddingChild(node), _))
        case node: treeRoot.ResultNode if data.contains(node.result) => Seq(path.pathByAddingChild(node))
        case _ => Seq()
      }

      val treeRootPath = new TreePath(treeRoot)
      val dataTreePaths = treeRoot.children.flatMap(getTreePathsForResults(treeRootPath, _))

      dataTreePaths.foreach { treePath =>
        resultsTree.addSelectedPath(treePath)
        resultsTree.getTree.expandPath(treePath)

      }
      dataTreePaths.headOption.foreach { treePath => // scroll to the first result
        resultsTree.scrollRectToVisible(resultsTree.getTree.getPathBounds(treePath))
      }

      scatterPlot.setSelection(data)
      parallelPlot.setSelection(data)
    } else if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) { // right click popup menu
      if (data.size != 1) {
        PopupUtils.createErrorPopupAtMouse("must select exactly one point", this)
        return
      }
      new DseResultPopupMenu(data.head, project).show(e.getComponent, e.getX, e.getY)
    } else if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2) { // double click view result
      if (data.size != 1) {
        PopupUtils.createErrorPopupAtMouse("must select exactly one point", this)
        return
      }
      val result = data.head
      BlockVisualizerService(project).setDesignTop(result.compiled, result.compiler,
        result.compiler.refinements.toPb, result.errors, Some(f"DSE ${result.index}: "))
    }
  }

  private val scatterPlot: DseScatterPlotPanel = new DseScatterPlotPanel() {
    override def onClick(e: MouseEvent, data: Seq[DseResult]): Unit = plotOnClick(e, data)
    override def onSwitchClick(): Unit = {
      mainSplitter.setFirstComponent(parallelPlot)
    }
  }
  private val parallelPlot: DseParallelPlotPanel = new DseParallelPlotPanel() {
    override def onClick(e: MouseEvent, data: Seq[DseResult]): Unit = plotOnClick(e, data)
    override def onSwitchClick(): Unit = {
      mainSplitter.setFirstComponent(scatterPlot)
    }
  }
  // TODO this updates all the plots even when one is displayed, this could be optimized
  private val allPlots = Seq(scatterPlot, parallelPlot)
  mainSplitter.setFirstComponent(scatterPlot)

  // GUI: Bottom tabs
  //
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
  configTree.addKeyListener(new KeyListener {
    override def keyTyped(keyEvent: KeyEvent): Unit = {}
    override def keyReleased(keyEvent: KeyEvent): Unit = {}
    override def keyPressed(keyEvent: KeyEvent): Unit = {
      if (keyEvent.getKeyCode == KeyEvent.VK_DELETE) {
        Option(configTree.getTree.getSelectionPath).getOrElse(return).getLastPathComponent match {
          case node: DseConfigTreeNode.DseSearchConfigNode =>
            DseService(project).getRunConfiguration.foreach { dseConfig =>
              val originalSearchConfigs = dseConfig.options.searchConfigs
              dseConfig.options.searchConfigs = originalSearchConfigs.filter(_ != node.config)
              DseService(project).onSearchConfigChanged(dseConfig, false)
            }
          case node: DseConfigTreeNode.DseObjectiveNode =>
            DseService(project).getRunConfiguration.foreach { dseConfig =>
              val originalObjectives = dseConfig.options.objectives
              dseConfig.options.objectives = originalObjectives.filter(_ != node.config)
              DseService(project).onSearchConfigChanged(dseConfig, false)
            }
          case _ => // ignored
        }
      }
    }
  })
  private val kTabConfig = tabbedPane.getTabCount
  tabbedPane.addTab("Config", new JBScrollPane(configTree))

  // GUI: Bottom Tabs: Results
  //
  private val resultsTree = new TreeTable(new DseResultTreeTableModel(new CombinedDseResultSet(Seq()), Seq(), false))
  resultsTree.setTreeCellRenderer(new DseResultTreeRenderer)
  resultsTree.setShowColumns(true)
  resultsTree.setRootVisible(false)
  resultsTree.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      val selectedTreePath = TreeTableUtils.getPathForRowLocation(resultsTree, e.getX, e.getY).getOrElse(return)
      selectedTreePath.getLastPathComponent match {
        case node: DseResultTreeNode#ResultSetNode =>
          if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 1) { // single click, highlight all in chart
            allPlots.foreach{_.setSelection(node.setMembers)}
          }
        case node: DseResultTreeNode#ResultNode =>
          if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 1) { // single click, highlight in chart
            allPlots.foreach{_.setSelection(Seq(node.result))}
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
  private val kTabResults = tabbedPane.getTabCount
  tabbedPane.addTab("Results", new JBScrollPane(resultsTree))

  onConfigUpdate()  // set initial state

  // Data state update
  def onConfigChange(config: DseRunConfiguration): Unit = {
    if (displayedConfig.contains(config)) {
      onConfigUpdate()
    }
  }

  def setResults(results: Seq[DseResult], search: Seq[DseConfigElement], objectives: Seq[DseObjective],
                 inProgress: Boolean): Unit = {
    val combinedResults = new CombinedDseResultSet(results)
    ApplicationManager.getApplication.invokeLater(() => {
      TreeTableUtils.updateModel(resultsTree,
        new DseResultTreeTableModel(combinedResults, objectives, inProgress))
    })
    allPlots.foreach{_.setResults(combinedResults, search, objectives)}
  }

  def focusConfigSearch(scrollToLast: Boolean): Unit = {
    // this makes the expansion fire AFTER the model is set (on the UI thread too), otherwise it
    // tries (and fails) to expand a node with no children (yet)
    // TODO this is kind of ugly
    ApplicationManager.getApplication.invokeLater(() => {
      val treeRoot = configTree.getTableModel.asInstanceOf[DseConfigTreeTableModel].rootNode
      val nodePath = new TreePath(treeRoot).pathByAddingChild(treeRoot.searchConfigNode)
      configTree.getTree.expandPath(nodePath)
      if (scrollToLast) {
        val lastNodePath = nodePath.pathByAddingChild(treeRoot.searchConfigNode.children.last)
        configTree.scrollRectToVisible(configTree.getTree.getPathBounds(lastNodePath))
      }
      tabbedPane.setSelectedIndex(kTabConfig)
    })
  }

  def focusConfigObjective(scrollToLast: Boolean): Unit = {
    // this makes the expansion fire AFTER the model is set (on the UI thread too), otherwise it
    // tries (and fails) to expand a node with no children (yet)
    // TODO this is kind of ugly
    ApplicationManager.getApplication.invokeLater(() => {
      val treeRoot = configTree.getTableModel.asInstanceOf[DseConfigTreeTableModel].rootNode
      val nodePath = new TreePath(treeRoot).pathByAddingChild(treeRoot.objectivesNode)
      configTree.getTree.expandPath(nodePath)
      if (scrollToLast) {
        val lastNodePath = nodePath.pathByAddingChild(treeRoot.objectivesNode.children.last)
        configTree.scrollRectToVisible(configTree.getTree.getPathBounds(lastNodePath))
      }
      tabbedPane.setSelectedIndex(kTabConfig)
    })
  }

  def focusResults(): Unit = {
    tabbedPane.setSelectedIndex(kTabResults)
  }

  // Configuration State
  //
  def saveState(state: DseServiceState): Unit = {
    state.dseTabIndex = tabbedPane.getSelectedIndex
  }

  def loadState(state: DseServiceState): Unit = {
    tabbedPane.setSelectedIndex(state.dseTabIndex)
  }
}
