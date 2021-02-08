package edg_ide.actions

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.fileEditor.FileDocumentManager
import edg_ide.ui.{BlockVisualizerService, EdgCompilerService}
import edg_ide.util.ErrorableNotify._


class RefreshFileAction() extends AnAction() {
  override def actionPerformed(event: AnActionEvent): Unit = {
    val document = Errorable(event.getData(CommonDataKeys.EDITOR), "No editor").map { editor =>
      editor.getDocument
    }
    val visualizer = Errorable(BlockVisualizerService(event.getProject).visualizerPanelOption, "No visualizer")

    (document + visualizer).mapOrNotify("edg_ide.actions.RefreshFileAction", event.getProject) { case (document, visualizer) =>
      val documentManager = FileDocumentManager.getInstance()
      documentManager.saveDocument(document)
      new DiscardCacheAction().actionPerformed(event)  // TODO this could integrate better by inlining
      visualizer.update()
    }
  }
}
