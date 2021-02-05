package edg_ide.actions

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.PyClass
import edg_ide.ui.BlockVisualizerService


class BlockVisualizationAction() extends AnAction() {
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.actions.BlockVisualizationAction")

  override def actionPerformed(event: AnActionEvent): Unit = {
    val visualizer = BlockVisualizerService.getInstance(event.getProject).visualizerPanelOption.getOrElse {
      notificationGroup.createNotification("No visualizer panel", NotificationType.WARNING)
          .notify(event.getProject)
      return
    }

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
    val offset = editor.getCaretModel.getOffset
    val element = Option(psiFile.findElementAt(offset)).getOrElse {
      notificationGroup.createNotification("No element at code", NotificationType.WARNING)
          .notify(event.getProject)
      return
    }

    val containingClass = Option(PsiTreeUtil.getParentOfType(element, classOf[PyClass])).getOrElse {
      notificationGroup.createNotification(s"No encapsulating class of selection", NotificationType.WARNING)
          .notify(event.getProject)
      return
    }

    visualizer.setFileBlock(psiFile.getVirtualFile, containingClass.getNameIdentifier.getText)
  }
}
