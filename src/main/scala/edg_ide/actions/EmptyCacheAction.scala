package edg_ide.actions

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import edg_ide.EdgirUtils
import edg_ide.ui.EdgCompilerService


class EmptyCacheAction() extends AnAction() {
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.actions.EmptyCacheAction")

  override def actionPerformed(event: AnActionEvent): Unit = {
    EdgCompilerService(event.getProject).pyLib.clearThisCache()

    notificationGroup.createNotification(
      s"IDE Cache Emptied",
      NotificationType.INFORMATION
    ).notify(event.getProject)
  }
}
