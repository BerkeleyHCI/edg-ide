package edg_ide

import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.actionSystem.CommonDataKeys

import com.intellij.notification.{NotificationGroup, NotificationType}


class NavigateToBlockAction() extends AnAction() {
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.NavigateToBlockAction")

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

  override def update(event: AnActionEvent): Unit = {
    // Menu item is only visible when a SplitFileEditor is open
    val editor = Option(event.getData(CommonDataKeys.EDITOR)).getOrElse {
      event.getPresentation.setEnabledAndVisible(false)
      return
    }
    event.getPresentation.setEnabledAndVisible(SplitFileEditor.fromTextEditor(editor).isDefined)
  }
}
