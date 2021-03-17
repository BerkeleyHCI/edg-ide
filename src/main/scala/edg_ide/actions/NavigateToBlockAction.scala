package edg_ide.actions

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.PyClass
import edg.ElemBuilder
import edg.wir.DesignPath
import edg_ide.{EdgirUtils, PsiUtils}
import edg_ide.ui.{BlockVisualizerService, PopupUtils}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption, ExceptSeq}
import edg_ide.util.{DesignFindBlockOfType, exceptionPopup}


class NavigateToBlockAction() extends AnAction() {
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.actions.NavigateToBlockAction")

  case class NavigateNode(path: DesignPath, typeString: String, action: () => Unit) {
    override def toString: String = s"$path of type $typeString"
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

      val refName = PsiUtils.selfReferenceOption(element).exceptError

      // TODO search all known subclasses
      val targetType = ElemBuilder.LibraryPath((modulePath :+ containingClass.getName).mkString("."))
      val instancesOfClass = new DesignFindBlockOfType(targetType).map(design)
          .exceptEmpty(s"no ${containingClass.getName} in design")

      val matchBlockPathTypes = instancesOfClass.collect {
        case (blockPath, block) if block.ports.contains(refName) =>
          (blockPath, blockPath + refName, EdgirUtils.typeOfPortLike(block.ports(refName)))
        case (blockPath, block) if block.blocks.contains(refName) =>
          (blockPath, blockPath + refName, EdgirUtils.typeOfBlockLike(block.blocks(refName)))
        case (blockPath, block) if block.links.contains(refName) =>
          (blockPath, blockPath + refName, EdgirUtils.typeOfLinkLike(block.links(refName)))
      }.exceptEmpty(s"no ${containingClass.getName} containing $refName")

      val items = matchBlockPathTypes.map { case (blockPath, eltPath, desc) =>
        val typeString = desc.map(EdgirUtils.SimpleLibraryPath).getOrElse("???")
        NavigateNode(eltPath, typeString, () => {
          visualizer.setContext(blockPath)
          visualizer.selectPath(eltPath)
        })
      }.exceptEmpty("no navigation targets")

      if (items.length == 1) {
        items.head.action()
      } else {
        PopupUtils.createMenuPopup(s"Navigate to ${containingClass.getName}.$refName", items, editor) { selected =>
          selected.action()
        }
      }
    }
  }
}
