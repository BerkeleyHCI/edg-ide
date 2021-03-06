package edg_ide.actions

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent}
import edg_ide.ui.BlockVisualizerService
import edg_ide.util.ExceptionNotifyImplicits.ExceptOption
import edg_ide.util.exceptionNotify


class RefreshVisualizationAction() extends AnAction() {
  override def actionPerformed(event: AnActionEvent): Unit = {
    exceptionNotify("edg_ide.actions.RefreshFileAction", event.getProject) {
      val visualizer = BlockVisualizerService(event.getProject).visualizerPanelOption.exceptNone("No visualizer")
      visualizer.recompile()
    }
  }
}
