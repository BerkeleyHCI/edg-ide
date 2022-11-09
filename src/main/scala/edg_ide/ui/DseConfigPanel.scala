package edg_ide.ui

import com.intellij.execution.{RunManager, RunnerAndConfigurationSettings}
import com.intellij.openapi.project.Project
import com.intellij.ui.components.JBScrollPane
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.intellij.util.concurrency.AppExecutorUtil
import edg_ide.swing.CompilerErrorTreeTableModel

import java.awt.BorderLayout
import java.util.concurrent.TimeUnit
import javax.swing.JPanel
import scala.collection.convert.ImplicitConversions.`list asScalaBuffer`


class DseConfigPanel(project: Project) extends JPanel {
  private var settings: Option[RunnerAndConfigurationSettings] = None

  AppExecutorUtil.getAppScheduledExecutorService.scheduleWithFixedDelay(() => {
    val selected = Option(RunManager.getInstance(project).getSelectedConfiguration)
    if (selected != settings) {
      settings = selected
      settings match {
        case Some(settings) => println(f"Updated: ${settings.getClass}: $settings")
          println(settings.getType)
          println(settings.getConfiguration)
          println(settings.getFactory)

        case _ => println(f"Updated: empty")
      }
    }
  }, 0, 333, TimeUnit.MILLISECONDS)

  private val tree = new TreeTable(new CompilerErrorTreeTableModel(Seq()))
  tree.setShowColumns(true)
  tree.setRootVisible(false)
  private val treeScrollPane = new JBScrollPane(tree)

  setLayout(new BorderLayout())
  add(treeScrollPane)
}
