package edg_ide.actions

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import edg_ide.SplitFileEditor


class NavigateToBlockAction() extends AnAction() {
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.actions.NavigateToBlockAction")

  override def actionPerformed(event: AnActionEvent): Unit = {
    val editor = Option(event.getData(CommonDataKeys.EDITOR)).getOrElse {
      notificationGroup.createNotification("No editor", NotificationType.WARNING)
          .notify(event.getProject)
      return
    }
    val psiFile = Option(event.getData(CommonDataKeys.PSI_FILE)).getOrElse {
      notificationGroup.createNotification("No PSI file", NotificationType.WARNING)
          .notify(event.getProject)
      return
    }
    val splitFileEditor = SplitFileEditor.fromTextEditor(editor).getOrElse {
      notificationGroup.createNotification(s"HDL editor not found", NotificationType.WARNING)
          .notify(event.getProject)
      return
    }

    val offset = editor.getCaretModel.getOffset
    val element = Option(psiFile.findElementAt(offset)) match {
      case None => notificationGroup.createNotification("No element at code", NotificationType.WARNING)
          .notify(event.getProject)
        return
      case Some(element) => element
    }

    splitFileEditor.selectFromPsi(element)
  }
}
