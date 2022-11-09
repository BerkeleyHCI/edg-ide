package edg_ide.ui

import com.intellij.execution.{RunManager, RunnerAndConfigurationSettings}
import com.intellij.openapi.project.Project
import com.intellij.ui.components.JBScrollPane
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.intellij.util.concurrency.AppExecutorUtil
import edg_ide.runner.DseRunConfiguration
import edg_ide.swing.CompilerErrorTreeTableModel

import java.awt.BorderLayout
import java.util.concurrent.TimeUnit
import javax.swing.JPanel
import scala.collection.convert.ImplicitConversions.`list asScalaBuffer`


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
  }, 0, 333, TimeUnit.MILLISECONDS)

  protected def onConfigUpdate(): Unit = {
    println(f"updated: $config")
    config match {
      case Some(config) =>
        println(f"with options ${config.options}")
      case _ =>
    }
  }

  private val tree = new TreeTable(new CompilerErrorTreeTableModel(Seq()))
  tree.setShowColumns(true)
  tree.setRootVisible(false)
  private val treeScrollPane = new JBScrollPane(tree)

  setLayout(new BorderLayout())
  add(treeScrollPane)
}
