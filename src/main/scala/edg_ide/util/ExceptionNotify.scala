package edg_ide.util

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.project.Project

import scala.reflect.ClassTag


class ExceptionNotifyException(val errMsg: String) extends Exception(errMsg)


object exceptionNotify {
  def apply(notificationId: String, project: Project)(fn: => Unit): Unit = {
    try {
      fn
    } catch {
      case e: ExceptionNotifyException =>
        val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup(notificationId)
        notificationGroup.createNotification(e.errMsg, NotificationType.WARNING).notify(project)
    }
  }
}


object ExceptionNotifyImplicits {
  implicit class ExceptNotify[T](obj: T) {
    def exceptNull(errMsg: String): T = {
      if (obj != null) {
        obj
      } else {
        throw new ExceptionNotifyException(errMsg)
      }
    }

    def instanceOfExcept[V](errMsg: String)(implicit tag: ClassTag[V]): V = obj match {
        // Need the implicit tag so this generates a proper runtime check
      case obj: V => obj
      case _ => throw new ExceptionNotifyException(errMsg)
    }
  }
}
