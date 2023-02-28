package edg_ide.ui

import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.ComboBox
import com.intellij.ui.JBSplitter
import com.intellij.ui.components.{JBScrollPane, JBTabbedPane}
import com.intellij.ui.dsl.builder.impl.CollapsibleTitledSeparator
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.intellij.util.concurrency.AppExecutorUtil
import edg.compiler.{ExprValue, FloatValue, IntValue, RangeType, RangeValue}
import edg.util.Errorable
import edg_ide.dse.{CombinedDseResultSet, DseConfigElement, DseObjective, DseObjectiveFootprintArea, DseObjectiveFootprintCount, DseObjectiveParameter, DseParameterSearch, DseResult}
import edg_ide.psi_edits.{InsertAction, InsertRefinementAction}
import edg_ide.runner.DseRunConfiguration
import edg_ide.swing._
import edg_ide.swing.dse.{DseConfigTreeNode, DseConfigTreeTableModel, DseResultTreeNode, DseResultTreeTableModel, JScatterPlot}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption}
import edg_ide.util.{DesignAnalysisUtils, exceptable, exceptionPopup, requireExcept}

import java.awt.event.{ItemEvent, ItemListener, MouseAdapter, MouseEvent}
import java.awt.{GridBagConstraints, GridBagLayout}
import java.util.concurrent.TimeUnit
import javax.swing.tree.TreePath
import javax.swing.{JPanel, JPopupMenu, SwingUtilities}
import scala.collection.SeqMap


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


class DsePlotPanel() extends JPanel {
  // Data State
  private var combinedResults = new CombinedDseResultSet(Seq())  // reflects the data points
  private var displayAxisSelectorObjectives: Seq[DseObjective] = Seq()  // reflects the widget display

  setLayout(new GridBagLayout)

  private val plot = new JScatterPlot[Seq[DseResult]]() {
    override def onClick(data: Seq[Data]): Unit = {
      DsePlotPanel.this.onClick(data.map(_.value))
    }

    override def onHoverChange(data: Seq[Data]): Unit = {
      DsePlotPanel.this.onHoverChange(data.map(_.value))
    }
  }
  add(plot, Gbc(0, 0, GridBagConstraints.BOTH, 2))

  sealed trait AxisItem {
    def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], JScatterPlot.AxisType)
  }

  class DummyAxisItem(val name: String) extends AxisItem {
    override def toString = name

    override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], JScatterPlot.AxisType) = {
      (results.map(_ => Some(0)), Some(Seq()))
    }
  }

  class DseObjectiveItem(objective: DseObjective) extends AxisItem {
    override def toString = objective.objectiveToString

    override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], JScatterPlot.AxisType) = {
      val values = results.map { result =>
        objective.calculate(result.compiled, result.compiler) match {
          case x: Float => Some(x)
          case x: Int => Some(x.toFloat)
          case _ => None
        }
      }
      (values, None)
    }
  }

  class DseObjectiveParameterItem(objective: DseObjectiveParameter, postfix: String,
                                  map: ExprValue => Option[Float]) extends AxisItem {
    override def toString = objective.objectiveToString + postfix

    override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], JScatterPlot.AxisType) = {
      val values = results.map { result =>
        objective.calculate(result.compiled, result.compiler).flatMap(map)
      }
      (values, None)
    }
  }

  class DseObjectiveParameterStringItem(objective: DseObjectiveParameter) extends AxisItem {
    override def toString = objective.objectiveToString

    override def resultsToValuesAxis(results: Seq[DseResult]): (Seq[Option[Float]], JScatterPlot.AxisType) = {
      val values = results.map { result =>
        objective.calculate(result.compiled, result.compiler).map(_.toStringValue)
      }
      val stringToPos = values.flatten.distinct.sorted.zipWithIndex.map { case (str, index) => (str, index.toFloat) }
      val axis = stringToPos.map { case (str, index) => (index, str) }

      val stringToPosMap = stringToPos.toMap
      val positionalValues = values.map { value => value.flatMap { value =>
        stringToPosMap.get(value)
      } }
      (positionalValues, Some(axis))
    }
  }


  private val xSelector = new ComboBox[AxisItem]()
  private val xAxisHeader = new DummyAxisItem("X Axis")
  xSelector.addItem(xAxisHeader)
  add(xSelector, Gbc(0, 1, GridBagConstraints.HORIZONTAL))
  private val ySelector = new ComboBox[AxisItem]()
  private val yAxisHeader = new DummyAxisItem("Y Axis")
  ySelector.addItem(yAxisHeader)
  add(ySelector, Gbc(1, 1, GridBagConstraints.HORIZONTAL))

  private def updatePlot(): Unit = {
    val examples = combinedResults.groupedResults.map(_.head)
    val (xPoints, xAxis) = xSelector.getItem.resultsToValuesAxis(examples)
    val (yPoints, yAxis) = ySelector.getItem.resultsToValuesAxis(examples)

    val points = combinedResults.groupedResults.zip(xPoints.zip(yPoints)).toIndexedSeq.flatMap {
      case (resultSet, (Some(xVal), Some(yVal))) =>
        val example = resultSet.head
        val color = if (example.errors.nonEmpty) {
          Some(com.intellij.ui.JBColor.RED)
        } else {
          None
        }

        val objectivesStr = example.objectives.map { case (objective, value) =>
          f"$objective=${DseConfigElement.valueToString(value)}"
        }.mkString("\n")
        val resultsStr = resultSet.map { result =>
          DseConfigElement.configMapToString(result.config)
        }.mkString("\n")
        val tooltipText = objectivesStr + f"\n\n${resultSet.size} results:\n" + resultsStr

        Some(new plot.Data(resultSet, xVal, yVal, color,
          Some(SwingHtmlUtil.wrapInHtml(tooltipText, this.getFont))))
      case _ => None
    }

    plot.setData(points, xAxis, yAxis)
  }

  private def updateAxisSelectors(objectives: Seq[DseObjective]): Unit = {
    if (objectives == displayAxisSelectorObjectives) {
      return  // nothing needs to be done
    }

    val selectedX = xSelector.getItem
    val selectedY = ySelector.getItem

    val items = objectives flatMap {
      case objective: DseObjectiveFootprintArea => Seq(new DseObjectiveItem(objective))
      case objective: DseObjectiveFootprintCount => Seq(new DseObjectiveItem(objective))
      case objective: DseObjectiveParameter if objective.exprType == classOf[FloatValue] =>
        Seq(new DseObjectiveParameterItem(objective, "", param => Some(param.asInstanceOf[FloatValue].value)))
      case objective: DseObjectiveParameter if objective.exprType == classOf[IntValue] =>
        Seq(new DseObjectiveParameterItem(objective, "", param => Some(param.asInstanceOf[IntValue].toFloat)))
      case objective: DseObjectiveParameter if objective.exprType == classOf[RangeType] => Seq(
        new DseObjectiveParameterItem(objective, " (min)", {
          case RangeValue(lower, upper) => Some(lower)
          case _ => None
        }),
        new DseObjectiveParameterItem(objective, " (max)", {
          case RangeValue(lower, upper) => Some(upper)
          case _ => None
        })
      )
      case objective: DseObjectiveParameter =>
        Seq(new DseObjectiveParameterStringItem(objective))
      case objective => Seq(new DummyAxisItem(f"unknown ${objective.objectiveToString}"))
    }

    xSelector.removeItemListener(axisSelectorListener)
    ySelector.removeItemListener(axisSelectorListener)

    xSelector.removeAllItems()
    ySelector.removeAllItems()

    xSelector.addItem(xAxisHeader)
    ySelector.addItem(yAxisHeader)
    items.foreach { item =>
      xSelector.addItem(item)
      ySelector.addItem(item)
    }

    xSelector.addItemListener(axisSelectorListener)
    ySelector.addItemListener(axisSelectorListener)
    displayAxisSelectorObjectives = objectives

    // restore prior selection by name matching
    items.find { item => item.toString == selectedX.toString }.foreach { item => xSelector.setItem(item) }
    items.find { item => item.toString == selectedY.toString }.foreach { item => ySelector.setItem(item) }
  }

  private val axisSelectorListener = new ItemListener() {
    override def itemStateChanged(e: ItemEvent): Unit = {
      if (e.getStateChange == ItemEvent.SELECTED) {
        updatePlot()
      }
    }
  }

  def setResults(combinedResults: CombinedDseResultSet, objectives: Seq[DseObjective]): Unit = {
    updateAxisSelectors(objectives)

    this.combinedResults = combinedResults
    updatePlot()
  }

  def setSelection(resultSets: Seq[Seq[DseResult]]): Unit = {
    plot.setSelected(resultSets)
  }

  // User hooks - can be overridden
  //
  // called when this widget clicked, for all points within some hover radius of the cursor
  // sorted by distance from cursor (earlier = closer), and may be empty
  def onClick(data: Seq[Seq[DseResult]]): Unit = {}

  // called when the hovered-over data changes, for all points within some hover radius of the cursor
  // may be empty (when hovering over nothing)
  def onHoverChange(data: Seq[Seq[DseResult]]): Unit = {}

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
    override def onClick(data: Seq[Seq[DseResult]]): Unit = {
      resultsTree.clearSelection()
      val treeRoot = resultsTree.getTableModel.asInstanceOf[DseResultTreeTableModel].rootNode
      val treeRootPath = new TreePath(treeRoot)
      treeRoot.children foreach {
        case node: treeRoot.ResultSetNode if data.contains(node.setMembers) =>
          val nodeTreePath = treeRootPath.pathByAddingChild(node)
          resultsTree.addSelectedPath(nodeTreePath)
          resultsTree.getTree.expandPath(nodeTreePath)
          resultsTree.scrollRectToVisible(resultsTree.getTree.getPathBounds(nodeTreePath))
        case node =>  // ignored
      }
      setSelection(data)
    }

    override def onHoverChange(data: Seq[Seq[DseResult]]): Unit = {
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
          if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 1) { // single click, highlight in chart
            plot.setSelection(Seq(node.setMembers))
          }
        case node: DseResultTreeNode#ResultNode =>
          if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) {  // right click popup menu
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
