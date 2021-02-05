package edg_ide.actions

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import edg_ide.SplitFileEditor


class BlockVisualizationAction() extends AnAction() {
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.actions.BlockVisualizationAction")

  override def actionPerformed(event: AnActionEvent): Unit = {
    import com.intellij.openapi.fileEditor.impl.EditorWindow
    val window = event.getRequiredData(EditorWindow.DATA_KEY)

    val psiFile = Option(event.getData(CommonDataKeys.PSI_FILE)).getOrElse {
      notificationGroup.createNotification("No PSI file", NotificationType.WARNING)
          .notify(event.getProject)
      return
    }
    psiFile.getVirtualFile

  }
}
