package edg_ide.actions

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.PyClass
import edg_ide.ui.BlockVisualizerService
import edg_ide.util.ErrorableNotify._


class NavigateToBlockAction() extends AnAction() {
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.actions.NavigateToBlockAction")

  override def actionPerformed(event: AnActionEvent): Unit = {
    val visualizer = Errorable(BlockVisualizerService.apply(event.getProject).visualizerPanelOption,
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

  /**
    * Selects the block diagram element associated with the PSI element, in both the block diagram and tree views.
    * Original from SplitFileEditor before that was deleted
    */
//  def selectFromPsi(element: PsiElement) {
//    val containingClass = PsiTreeUtil.getParentOfType(element, classOf[PyClass]) match {
//      case null =>
//        notificationGroup.createNotification(
//          s"No encapsulating class of selection",
//          NotificationType.WARNING)
//            .notify(getEditor.getProject)
//        return
//      case pyClass: PyClass => pyClass.getNameIdentifier.getText
//    }
//
//    val referenceOpt = PsiTreeUtil.getParentOfType(element, classOf[PyReferenceExpression]) match {
//      case expr: PyReferenceExpression => PsiUtils.psiSelfReference(getEditor.getProject, expr)
//      case _ => None
//    }
//    val targetOpt = PsiTreeUtil.getParentOfType(element, classOf[PyTargetExpression]) match {
//      case expr: PyTargetExpression => PsiUtils.psiSelfTarget(getEditor.getProject, expr)
//      case _ => None
//    }
//
//    val name = referenceOpt.getOrElse(targetOpt.getOrElse {
//      notificationGroup.createNotification(
//        s"No reference of form self.(element) selected",
//        NotificationType.WARNING)
//          .notify(getEditor.getProject)
//      return
//    } )
//
//    if (design.contents.isDefined) {
//      // First, try searching for the selected element in the currently selected block
//      val startingBlock = EdgirUtils.ResolvePath(design.contents.get, selectedPath) match {
//        case Some(startingBlock: elem.HierarchyBlock) => startingBlock
//        case startingBlock =>
//          println(s"Failed to resolve current path $selectedPath, got $startingBlock")  // TODO use logging infra
//          return
//      }
//
//      val startingSuperclass = EdgirUtils.SimpleSuperclass(startingBlock.superclasses)
//      if (startingSuperclass == containingClass) {
//        if (startingBlock.blocks.contains(name) || startingBlock.ports.contains(name)) {
//          selectByPath(selectedPath ++ Seq(name))
//          return
//        } else {
//          println(s"Failed to resolve selected PSI $name at selected path $selectedPath")  // TODO use logging infra
//          return
//        }
//      }
//
//      // Next, try searching for the selected element in the parent block, to support multiple navigation operations
//      // which need sibling-level search
//      if (selectedPath.nonEmpty) {
//        val parentPath = selectedPath.slice(0, selectedPath.length - 1)
//        val parentBlock = EdgirUtils.ResolvePath(design.contents.get, parentPath) match {
//          case Some(parentBlock: elem.HierarchyBlock) => parentBlock
//          case parentBlock =>
//            println(s"Failed to resolve current parent path $parentPath, got $parentBlock")  // TODO use logging infra
//            return
//        }
//
//        val parentSuperclass = EdgirUtils.SimpleSuperclass(parentBlock.superclasses)
//        if (parentSuperclass == containingClass) {  // first try searching in the selected block
//          if (parentBlock.blocks.contains(name) || parentBlock.ports.contains(name)) {
//            selectByPath(parentPath ++ Seq(name))
//            return
//          } else {
//            println(s"Failed to resolve selected PSI $name at parent path $selectedPath")  // TODO use logging infra
//            return
//          }
//        }
//      }
//    }
//  }
}
