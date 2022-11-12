package edg_ide.ui

import com.intellij.execution.RunManager
import com.intellij.openapi.project.Project
import com.intellij.ui.components.{JBScrollPane, JBTabbedPane}
import com.intellij.ui.dsl.builder.impl.CollapsibleTitledSeparator
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.intellij.util.concurrency.AppExecutorUtil
import edg_ide.runner.DseRunConfiguration
import edg_ide.swing.DseConfigTreeTableModel

import java.awt.{GridBagConstraints, GridBagLayout}
import java.util.concurrent.TimeUnit
import javax.swing.JPanel
import scala.collection.SeqMap


class DseConfigPanel(project: Project) extends JPanel {
  private var config: Option[DseRunConfiguration] = None

  // Regularly check the selected run config so the panel contents are kept in sync
  AppExecutorUtil.getAppScheduledExecutorService.scheduleWithFixedDelay(() => {
    val newConfig = Option(RunManager.getInstance(project).getSelectedConfiguration).map(_.getConfiguration)
    val newTypedConfig = newConfig match {
      case Some(newConfig: DseRunConfiguration) => Some(newConfig)
      case _ => None
    }

    if (newTypedConfig != config) {
      config = newTypedConfig
      onConfigUpdate()
    }
  }, 333, 333, TimeUnit.MILLISECONDS)  // seems flakey without initial delay

  protected def onConfigUpdate(): Unit = {
    config match {
      case Some(config) =>
        separator.setText(f"Design Space Exploration: ${config.getName}")
        configTree.setModel(new DseConfigTreeTableModel(config.options.searchConfigs, config.options.objectives))
        configTree.setRootVisible(false)
      case _ =>
        separator.setText(f"Design Space Exploration: no run config selected")
        configTree.setModel(new DseConfigTreeTableModel(Seq(), SeqMap()))
        configTree.setRootVisible(false)
    }
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
  tabbedPane.addTab("Config", new JBScrollPane(configTree))

  onConfigUpdate()  // set initial state

  // GUI: Results Tab
  //
  private val resultsTree = new TreeTable(new DseConfigTreeTableModel(Seq(), SeqMap()))
  resultsTree.setShowColumns(true)
  resultsTree.setRootVisible(false)
  tabbedPane.addTab("Results", new JBScrollPane(resultsTree))

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
    state.dseTabIndex = tabbedPane.getSelectedIndex
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
    tabbedPane.setSelectedIndex(state.dseTabIndex)
  }
}
