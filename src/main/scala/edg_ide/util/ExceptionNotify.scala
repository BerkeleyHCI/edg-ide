package edg_ide.util

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.project.Project
import edg.util.Errorable
import edg_ide.ui.PopupUtils

import java.awt.event.MouseEvent
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
    apply(NotificationGroup.balloonGroup(notificationId), project)(fn)
  }

  def apply(notificationGroup: NotificationGroup, project: Project)(fn: => Unit): Unit = {
    try {
      fn
    } catch {
      case e: ExceptionNotifyException =>
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


object exceptionPopup {
  /** Runs a block of code that may have requireExcept and fail-able ExceptionNotifyImplicits conversions.
    * If any of those fail, terminates execution and displays the failure message as a popup above the cursor.
    */
  def apply(event: MouseEvent)(fn: => Unit): Unit = {
    try {
      fn
    } catch {
      case e: ExceptionNotifyException =>
        PopupUtils.createErrorPopup(e.errMsg, event)
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

  implicit class ExceptSeq[T](obj: Seq[T]) {
    def exceptEmpty(errMsg: String): Seq[T] = obj match {
      case Seq() => throw ExceptionNotifyException(errMsg)
      case _ => obj
    }
    def onlyExcept(errMsg: String): T = obj match {
      case Seq(obj) => obj
      case _ => throw ExceptionNotifyException(errMsg)
    }
  }

  implicit class ExceptErrorable[T](obj: Errorable[T]) {
    def exceptError: T = obj match {
      case Errorable.Success(obj) => obj
      case Errorable.Error(msg) => throw ExceptionNotifyException(msg)
    }
  }

  implicit class ExceptBoolean(obj: Boolean) {
    def exceptTrue(errMsg: String): Boolean = obj match {
      case true => throw ExceptionNotifyException(errMsg)
      case false => obj
    }

    def exceptFalse(errMsg: String): Boolean = obj match {
      case true => obj
      case false => throw ExceptionNotifyException(errMsg)
    }
  }
}
