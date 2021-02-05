package edg_ide.actions

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.PyClass
import edg_ide.ui.BlockVisualizerService
import edg_ide.util.Errorable


class NavigateToBlockAction() extends AnAction() {
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.actions.NavigateToBlockAction")

  override def actionPerformed(event: AnActionEvent): Unit = {
    val visualizer = Errorable(BlockVisualizerService.getInstance(event.getProject).visualizerPanelOption,
      "No visualizer panel")

    val editor = Errorable(event.getData(CommonDataKeys.EDITOR), "No editor")
    val offset = editor.map { _.getCaretModel.getOffset }
    val psiFile = Errorable(event.getData(CommonDataKeys.PSI_FILE), "No PSI file")
    val containingClass = (psiFile + offset).map("No element") {
      case (psiFile, offset) => psiFile.findElementAt(offset)
    }.map("No containing class") { PsiTreeUtil.getParentOfType(_, classOf[PyClass]) }

    visualizer + (psiFile + containingClass) match {
      case Errorable.Success((visualizer, (psiFile, containingClass))) =>
        ???
      case Errorable.Error(msg) =>
        notificationGroup.createNotification(msg, NotificationType.WARNING).notify(event.getProject)
    }
  }
}
