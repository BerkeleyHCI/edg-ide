package edg_ide.util

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.project.Project
import edg.util.Errorable

import scala.language.implicitConversions


object ErrorableNotify {
  val Errorable = edg.util.Errorable  // import forwarding for convenience

  implicit class ErrorableNotify[T](errorable: Errorable[T]) {
    def mapOrNotify[V](notificationId: String, project: Project)(fn: T => V): Errorable[V] = {
      mapOrNotify(NotificationGroup.balloonGroup(notificationId), project)(fn)
    }

    def mapOrNotify[V](notificationGroup: NotificationGroup, project: Project)(fn: T => V): Errorable[V] = {
      errorable match {
        case Errorable.Success(_) =>  // ignored
        case Errorable.Error(errMsg) =>
          notificationGroup.createNotification(errMsg, NotificationType.WARNING).notify(project)
      }
      errorable.map(fn)
    }
  }
}
