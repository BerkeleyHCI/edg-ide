package edg_ide.util

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import edg.util.Errorable
import edg_ide.ui.PopupUtils

import java.awt.event.MouseEvent
import scala.reflect.ClassTag


class ExceptionNotifyException(val errMsg: String) extends Exception(errMsg)


object exceptable {
  /** Runs a block of code that may have requireExcept and fail-able ExceptionNotifyImplicits conversions.
    * The result (or failure message) is returned as an Errorable.
    */
  def apply[T](fn: => T): Errorable[T] = {
    try {
      Errorable.Success(fn)
    } catch {
      case e: ExceptionNotifyException => Errorable.Error(e.errMsg)
    }
  }

  def fail(errMsg: String): Nothing = {
    throw new ExceptionNotifyException(errMsg)
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

  def apply(editor: Editor)(fn: => Unit): Unit = {
    try {
      fn
    } catch {
      case e: ExceptionNotifyException =>
        PopupUtils.createErrorPopup(e.errMsg, editor)
    }
  }

  def atMouse(owner: java.awt.Component)(fn: => Unit): Unit = {
    try {
      fn
    } catch {
      case e: ExceptionNotifyException =>
        PopupUtils.createErrorPopupAtMouse(e.errMsg, owner)
    }
  }
}


object requireExcept {
  def apply(cond: Boolean, errMsg: => String): Unit = {
    if (!cond) {
      exceptable.fail(errMsg)
    }
  }
}

/** Implicit conversions that throw a ExceptionNotifyException on failure, to be used withing an
  * exceptionNotify wrapper
  */
object ExceptionNotifyImplicits {
  implicit class ExceptNotify[T](obj: T) {
    def exceptNull(errMsg: => String): T = {
      if (obj != null) {
        obj
      } else {
        exceptable.fail(errMsg)
      }
    }

    def exceptEquals(exceptValue: T, errMsg: => String): T = {
      if (obj != exceptValue) {
        obj
      } else {
        exceptable.fail(errMsg)
      }
    }

    def instanceOfExcept[V](errMsg: => String)(implicit tag: ClassTag[V]): V = obj match {
        // Need the implicit tag so this generates a proper runtime check
      case obj: V => obj
      case _ => exceptable.fail(errMsg)
    }
  }

  implicit class ExceptOption[T](obj: Option[T]) {
    def exceptNone(errMsg: => String): T = obj match {
      case Some(obj) => obj
      case None => exceptable.fail(errMsg)
    }
  }

  implicit class ExceptSeq[T](obj: Seq[T]) {
    def exceptEmpty(errMsg: => String): Seq[T] = obj match {
      case Seq() => exceptable.fail(errMsg)
      case _ => obj
    }
    def onlyExcept(errMsg: => String): T = obj match {
      case Seq(obj) => obj
      case _ => exceptable.fail(errMsg)
    }
  }

  implicit class ExceptErrorable[T](obj: Errorable[T]) {
    def exceptError: T = obj match {
      case Errorable.Success(obj) => obj
      case Errorable.Error(msg) => exceptable.fail(msg)
    }
  }

  implicit class ExceptBoolean(obj: Boolean) {
    def exceptTrue(errMsg: => String): Boolean = obj match {
      case true => exceptable.fail(errMsg)
      case false => obj
    }

    def exceptFalse(errMsg: => String): Boolean = obj match {
      case true => obj
      case false => exceptable.fail(errMsg)
    }
  }
}
