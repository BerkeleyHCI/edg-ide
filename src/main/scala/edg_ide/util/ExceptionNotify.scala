package edg_ide.util

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.project.Project
import edg.util.Errorable

import scala.reflect.ClassTag


case class ExceptionNotifyException(val errMsg: String) extends Exception(errMsg)


object exceptable {
  /** Runs a block of code that may have requireExcept and fail-able ExceptionNotifyImplicits conversions.
    * The result (or failure message) is returned as an Errorable.
    */
  def apply[T](fn: => T): Errorable[T] = {
    try {
      Errorable.Success(fn)
    } catch {
      case ExceptionNotifyException(errMsg) => Errorable.Error(errMsg)
    }
  }
}


object exceptionNotify {
  /** Runs a block of code that may have requireExcept and fail-able ExceptionNotifyImplicits conversions.
    * If any of those fail, terminates execution and displays the failure message.
    */
  def apply(notificationId: String, project: Project)(fn: => Unit): Unit = {
    try {
      fn
    } catch {
      case e: ExceptionNotifyException =>
        val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup(notificationId)
        notificationGroup.createNotification(e.errMsg, NotificationType.WARNING).notify(project)
    }
  }

  /** Similar to exceptionNotify(...), but the function returns a string that is displayed on success.
    */
  def successNotify(notificationId: String, project: Project)(fn: => String): Unit = {
    val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup(notificationId)
    try {
      val result = fn
      notificationGroup.createNotification(result, NotificationType.INFORMATION).notify(project)
    } catch {
      case e: ExceptionNotifyException =>
        notificationGroup.createNotification(e.errMsg, NotificationType.WARNING).notify(project)
    }
  }
}

object requireExcept {
  def apply(cond: Boolean, errMsg: String): Unit = {
    if (!cond) {
      throw ExceptionNotifyException(errMsg)
    }
  }
}

/** Implicit conversions that throw a ExceptionNotifyException on failure, to be used withing an
  * exceptionNotify wrapper
  */
object ExceptionNotifyImplicits {
  implicit class ExceptNotify[T](obj: T) {
    def exceptNull(errMsg: String): T = {
      if (obj != null) {
        obj
      } else {
        throw ExceptionNotifyException(errMsg)
      }
    }

    def instanceOfExcept[V](errMsg: String)(implicit tag: ClassTag[V]): V = obj match {
        // Need the implicit tag so this generates a proper runtime check
      case obj: V => obj
      case _ => throw ExceptionNotifyException(errMsg)
    }
  }

  implicit class ExceptOption[T](obj: Option[T]) {
    def exceptNone(errMsg: String): T = obj match {
      case Some(obj) => obj
      case None => throw ExceptionNotifyException(errMsg)
    }
  }

  implicit class ExceptErrorable[T](obj: Errorable[T]) {
    def exceptError: T = obj match {
      case Errorable.Success(obj) => obj
      case Errorable.Error(msg) => throw ExceptionNotifyException(msg)
    }
  }
}
