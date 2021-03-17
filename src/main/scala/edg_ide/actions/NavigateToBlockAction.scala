package edg_ide.actions

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.PyClass
import edg.ElemBuilder
import edg.wir.DesignPath
import edg_ide.ui.{BlockVisualizerService, PopupUtils}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptNotify, ExceptOption, ExceptSeq}
import edg_ide.util.{DesignFindBlockOfType, exceptionPopup}


class NavigateToBlockAction() extends AnAction() {
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.actions.NavigateToBlockAction")

  case class NavigateNode(path: DesignPath, action: () => Unit) {
    override def toString: String = path.toString
  }

  override def actionPerformed(event: AnActionEvent): Unit = {
    val editor = event.getData(CommonDataKeys.EDITOR)
    if (editor == null) {
      notificationGroup.createNotification("No editor", NotificationType.WARNING)
          .notify(event.getProject)
    }

    exceptionPopup(editor) {
      val visualizer = BlockVisualizerService(event.getProject)
      val design = visualizer.getDesign.exceptNone("no design")
      val (contextPath, contextBlock) = visualizer.getContextBlock.exceptNone("no visualizer context")

      val psiFile = event.getData(CommonDataKeys.PSI_FILE).exceptNull("no file")
      val offset = editor.getCaretModel.getOffset
      val element = psiFile.findElementAt(offset).exceptNull(s"invalid caret position in ${psiFile.getName}")
      val containingClass = PsiTreeUtil.getParentOfType(element, classOf[PyClass]).exceptNull("not in a class")
      val modulePath = ModuleUtil.from(event.getProject.getBaseDir, psiFile.getVirtualFile)
          .exceptNone("class has invalid module path")

      val targetType = ElemBuilder.LibraryPath((modulePath :+ containingClass.getName).mkString("."))
      val instancesOfClass = new DesignFindBlockOfType(targetType).map(design)
          .exceptEmpty(s"no ${containingClass.getName} in design")

      val matches = instancesOfClass.filter { case (blockPath, block) =>
        true
      }.exceptEmpty(s"no ${containingClass.getName} containing ???")

      val items = matches.map { case (blockPath, block) =>
        NavigateNode(blockPath, () => ())
      }.exceptEmpty("no navigation targets")

      PopupUtils.createMenuPopup("Navigate to", items, editor) { selected =>
        println(s"selected $selected")
      }

    }
  }
}
