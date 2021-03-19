package edg_ide.actions

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent}
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.progress.{ProgressIndicator, ProgressManager, Task}
import edg_ide.ui.{BlockVisualizerService, EdgCompilerService}
import edg_ide.util.ErrorableNotify.{Errorable, ErrorableNotify}


class FillCacheAction() extends AnAction() {
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("")

  override def actionPerformed(event: AnActionEvent): Unit = {
    val project = event.getProject

    Errorable(BlockVisualizerService.apply(event.getProject).visualizerPanelOption,
      "No visualizer panel").mapOrNotify("edg_ide.actions.FillCacheAction", project) { visualizer =>

      val documentManager = FileDocumentManager.getInstance()
      documentManager.saveAllDocuments()

      ProgressManager.getInstance().run(new Task.Backgroundable(project, "EDG library compiling") {
        override def run(indicator: ProgressIndicator): Unit = {
          val (loadSuccess, loadFailure) = EdgCompilerService(project).fillCache(visualizer.getModule, Some(indicator))

          indicator.setText(s"EDG library compiling: updating view")
          visualizer.updateLibrary(EdgCompilerService(project).pyLib)

          if (loadFailure.isEmpty) {
            notificationGroup.createNotification(
              s"Loaded ${loadSuccess.size} elements",
              NotificationType.INFORMATION)
                .notify(project)
          } else {
            import edg_ide.EdgirUtils
            notificationGroup.createNotification(
              s"Loaded ${loadSuccess.size} elements with ${loadFailure.size} errors", s"",
              s"Errors:\n" + loadFailure.map(EdgirUtils.SimpleLibraryPath).mkString("\n"),
              NotificationType.WARNING)
                .notify(project)
          }
        }
      })

    }
  }
}
