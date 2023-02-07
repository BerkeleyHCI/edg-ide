package edg_ide.ui

import com.intellij.openapi.project.Project
import com.intellij.ui.components.{JBScrollPane, JBTabbedPane}
import com.intellij.ui.dsl.builder.impl.CollapsibleTitledSeparator
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.intellij.util.concurrency.AppExecutorUtil
import edg_ide.dse.{DseConfigElement, DseObjective, DseParameterSearch, DseResult}
import edg_ide.runner.DseRunConfiguration
import edg_ide.swing._
import edg_ide.swing.dse.{DseConfigTreeNode, DseConfigTreeTableModel, DseResultTreeNode, DseResultTreeTableModel}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption}
import edg_ide.util.{exceptable, requireExcept}

import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.{GridBagConstraints, GridBagLayout}
import java.util.concurrent.TimeUnit
import javax.swing.{JPanel, JPopupMenu, SwingUtilities}
import scala.collection.SeqMap


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

  add(ContextMenuUtils.MenuItemFromErrorable(exceptable {
    val parseableSearchConfig = searchConfig.instanceOfExcept[DseParameterSearch]("not an editable config type")
    val initialValue = parseableSearchConfig.valuesToString()

    () => PopupUtils.createStringEntryPopup("Search Values", project, initialValue) { text => exceptable {
      val parsed = parseableSearchConfig.valuesStringToConfig(text).exceptError

      val dseConfig = BlockVisualizerService(project).getDseRunConfiguration.exceptNone("no run config")
      val originalSearchConfigs = dseConfig.options.searchConfigs
      val index = originalSearchConfigs.indexOf(searchConfig)
      requireExcept(index >= 0, "config not found")
      val newSearchConfigs = originalSearchConfigs.patch(index, Seq(parsed), 1)
      dseConfig.options.searchConfigs = newSearchConfigs
      BlockVisualizerService(project).onDseConfigChanged(dseConfig)
    } }
  }, s"Edit"))
}


class DseObjectivePopupMenu(objective: DseObjective[Any], project: Project) extends JPopupMenu {
  add(ContextMenuUtils.MenuItemFromErrorable(exceptable {
    val dseConfig = BlockVisualizerService(project).getDseRunConfiguration.exceptNone("no run config")
    val originalObjectives = dseConfig.options.objectives
    val key = originalObjectives.find(objective == _._2).exceptNone("objective not in config")._1
    () => {
      dseConfig.options.objectives = originalObjectives.filter(_._1 != key)
      BlockVisualizerService(project).onDseConfigChanged(dseConfig)
    }
  }, s"Delete"))
}


class DseConfigPanel(project: Project) extends JPanel {
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
        TreeTableUtils.updateModel(configTree, new DseConfigTreeTableModel(config.options.searchConfigs, config.options.objectives))
        TreeTableUtils.updateModel(resultsTree, new DseResultTreeTableModel(Seq(), false))  // clear existing data
      case _ =>
        separator.setText(f"Design Space Exploration: no run config selected")
        TreeTableUtils.updateModel(configTree, new DseConfigTreeTableModel(Seq(), SeqMap()))
        TreeTableUtils.updateModel(resultsTree, new DseResultTreeTableModel(Seq(), false))  // clear existing data
    }
  }

  def onConfigChange(config: DseRunConfiguration): Unit = {
    if (displayedConfig.contains(config)) {
      onConfigUpdate()
    }
  }

  def setResults(results: Seq[DseResult], inProgress: Boolean): Unit = {
    TreeTableUtils.updateModel(resultsTree, new DseResultTreeTableModel(results, inProgress))
  }

  setLayout(new GridBagLayout())

  // TODO make the collapse function actually work
  private val separator = new CollapsibleTitledSeparator("Design Space Exploration")
  add(separator, Gbc(0, 0, GridBagConstraints.HORIZONTAL))

  private val tabbedPane = new JBTabbedPane()
  add(tabbedPane, Gbc(0, 1, GridBagConstraints.BOTH))

  // GUI: Config Tab
  //
  private val configTree = new TreeTable(new DseConfigTreeTableModel(Seq(), SeqMap()))
  configTree.setShowColumns(true)
  configTree.setRootVisible(false)
  configTree.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      val selectedTreePath = configTree.getTree.getPathForLocation(e.getX, e.getY)
      if (selectedTreePath == null) {
        return
      }

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

  // GUI: Results Tab
  //
  private val resultsTree = new TreeTable(new DseResultTreeTableModel(Seq(), false))
  resultsTree.setShowColumns(true)
  resultsTree.setRootVisible(false)
  resultsTree.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      val selectedTreePath = resultsTree.getTree.getPathForLocation(e.getX, e.getY)
      if (selectedTreePath == null) {
        return
      }

      selectedTreePath.getLastPathComponent match {
        case node: DseResultTreeNode#ResultNode =>
          if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2) { // double click
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

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
    state.dseTabIndex = tabbedPane.getSelectedIndex
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
    tabbedPane.setSelectedIndex(state.dseTabIndex)
  }
}
