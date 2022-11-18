package edg_ide.ui

import com.intellij.openapi.project.Project
import com.intellij.ui.components.{JBLabel, JBScrollPane, JBTabbedPane}
import com.intellij.ui.dsl.builder.impl.CollapsibleTitledSeparator
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.intellij.util.concurrency.AppExecutorUtil
import edg_ide.dse.DseResult
import edg_ide.runner.DseRunConfiguration
import edg_ide.swing.{DseConfigTreeTableModel, DseResultTreeTableModel}

import java.awt.{GridBagConstraints, GridBagLayout}
import java.util.concurrent.TimeUnit
import javax.swing.{JPanel, JProgressBar}
import scala.collection.SeqMap


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
        configTree.setModel(new DseConfigTreeTableModel(config.options.searchConfigs, config.options.objectives))
        configTree.setRootVisible(false)
        resultsTree.setModel(new DseResultTreeTableModel(Seq()))  // clear existing data
        resultsTree.setRootVisible(false)
      case _ =>
        separator.setText(f"Design Space Exploration: no run config selected")
        configTree.setModel(new DseConfigTreeTableModel(Seq(), SeqMap()))
        configTree.setRootVisible(false)
        resultsTree.setModel(new DseResultTreeTableModel(Seq())) // clear existing data
        resultsTree.setRootVisible(false)
    }
  }

  def setResults(results: Seq[DseResult]): Unit = {
    resultsTree.setModel(new DseResultTreeTableModel(results))
    resultsTree.setRootVisible(false)
  }

  setLayout(new GridBagLayout())

  // TODO make the collapse function actually work
  private val separator = new CollapsibleTitledSeparator("Design Space Exploration")
  add(separator, Gbc(0, 0, GridBagConstraints.HORIZONTAL, 2))

  private val tabbedPane = new JBTabbedPane()
  add(tabbedPane, Gbc(0, 2, GridBagConstraints.BOTH, 2))

  // GUI: Config Tab
  //
  private val configTree = new TreeTable(new DseConfigTreeTableModel(Seq(), SeqMap()))
  configTree.setShowColumns(true)
  configTree.setRootVisible(false)
  tabbedPane.addTab("Config", new JBScrollPane(configTree))

  // GUI: Results Tab
  //
  private val resultsTree = new TreeTable(new DseResultTreeTableModel(Seq()))
  resultsTree.setShowColumns(true)
  resultsTree.setRootVisible(false)
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
