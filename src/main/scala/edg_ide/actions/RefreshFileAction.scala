package edg_ide.actions

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.fileEditor.FileDocumentManager
import edg_ide.EdgirUtils
import edg_ide.ui.{BlockVisualizerService, EdgCompilerService}
import edg_ide.util.Errorable


class RefreshFileAction() extends AnAction() {
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.actions.RefreshFileAction")

  override def actionPerformed(event: AnActionEvent): Unit = {
    val document = Errorable(event.getData(CommonDataKeys.EDITOR), "No editor").map { editor =>
      editor.getDocument
    }
    val visualizer = Errorable(BlockVisualizerService(event.getProject).visualizerPanelOption, "No visualizer")

    (document + visualizer) match {
      case Errorable.Success((document, visualizer)) =>
        val documentManager = FileDocumentManager.getInstance()
        documentManager.saveDocument(document)
        new DiscardCacheAction().actionPerformed(event)  // TODO this could integrate better by inlining
        visualizer.update()
      case Errorable.Error(msg) => notificationGroup
          .createNotification(msg, NotificationType.WARNING)
          .notify(event.getProject)
    }
  }
}
