package edg_ide.ui

import com.intellij.execution.RunManager
import com.intellij.openapi.project.Project
import com.intellij.ui.components.JBScrollPane
import com.intellij.ui.dsl.builder.impl.CollapsibleTitledSeparator
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.intellij.util.concurrency.AppExecutorUtil
import edg_ide.runner.DseRunConfiguration
import edg_ide.swing.DseConfigTreeTableModel

import java.awt.{GridBagConstraints, GridBagLayout}
import java.util.concurrent.TimeUnit
import javax.swing.JPanel


class DseConfigPanel(project: Project) extends JPanel {
  private var config: Option[DseRunConfiguration] = None

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
        tree.setModel(new DseConfigTreeTableModel(config.options.searchConfigs))
        tree.setRootVisible(false)
      case _ =>
        separator.setText(f"Design Space Exploration: no run config selected")
        tree.setModel(new DseConfigTreeTableModel(Seq()))
        tree.setRootVisible(false)
    }
  }

  setLayout(new GridBagLayout())

  private val separator = new CollapsibleTitledSeparator("Design Space Exploration")
  add(separator, Gbc(0, 0, GridBagConstraints.HORIZONTAL))

  private val tree = new TreeTable(new DseConfigTreeTableModel(Seq()))
  tree.setShowColumns(true)
  tree.setRootVisible(false)
  private val treeScrollPane = new JBScrollPane(tree)
  add(treeScrollPane, Gbc(0, 1, GridBagConstraints.BOTH))

  onConfigUpdate()  // set initial state

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
  }
}
