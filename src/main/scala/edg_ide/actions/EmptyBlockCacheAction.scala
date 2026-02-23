package edg_ide.actions

import com.intellij.notification.NotificationType
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent}
import edg_ide.ui.EdgCompilerService

class EmptyBlockCacheAction() extends AnAction() {
  override def actionPerformed(event: AnActionEvent): Unit = {
    EdgCompilerService(event.getProject).pyLib.clearThisCache()

    EdgCompilerService.notificationGroup()
      .createNotification(
        s"Block Cache Emptied",
        NotificationType.INFORMATION
      )
      .notify(event.getProject)
  }
}
