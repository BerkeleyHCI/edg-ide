package edg_ide.util

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.project.Project

import scala.language.implicitConversions


object ErrorableNotify {
  val Errorable = edg_ide.util.Errorable  // import forwarding for convenience

  implicit class ErrorableNotify[T](errorable: Errorable[T]) {
    def mapOrNotify[V](notificationId: String, project: Project)(fn: T => V): Errorable[V] = {
      errorable match {
        case Errorable.Success(_) =>  // ignored
        case Errorable.Error(errMsg) =>
          val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup(notificationId)
          notificationGroup.createNotification(errMsg, NotificationType.WARNING).notify(project)
      }
      errorable.map(fn)
    }
  }
}
