package edg_ide

import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.actionSystem.CommonDataKeys

import com.intellij.notification.{NotificationGroup, NotificationType, Notification}


class NavigateToBlockAction() extends AnAction() {
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.navigate_to_block")

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
    val element = psiFile.findElementAt(offset) match {
      case null => notificationGroup.createNotification("No element at code", NotificationType.WARNING)
          .notify(event.getProject)
      case element => element
    }

    // TODO actually implement the action
    notificationGroup.createNotification(
      s"TODO: NavigateToBlockAction", s"", s"selected PSI $element, found $splitFileEditor",
      NotificationType.INFORMATION)
        .notify(event.getProject)
  }

  override def update(event: AnActionEvent): Unit = {
    // Menu item is only visible when a SplitFileEditor is open
    val editor = Option(event.getData(CommonDataKeys.EDITOR)).getOrElse {
      event.getPresentation.setEnabledAndVisible(false)
      return
    }
    event.getPresentation.setEnabledAndVisible(SplitFileEditor.fromTextEditor(editor).isDefined)
  }
}
