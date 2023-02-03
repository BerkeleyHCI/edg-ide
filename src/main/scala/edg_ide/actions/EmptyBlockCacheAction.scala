package edg_ide.actions

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent}
import edg_ide.ui.EdgCompilerService


class EmptyBlockCacheAction() extends AnAction() {
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.actions.EmptyCacheAction")

  override def actionPerformed(event: AnActionEvent): Unit = {
    EdgCompilerService(event.getProject).pyLib.clearThisCache()

    notificationGroup.createNotification(
      s"Block Cache Emptied",
      NotificationType.INFORMATION
    ).notify(event.getProject)
  }
}
