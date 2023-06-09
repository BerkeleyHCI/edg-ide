package edg_ide.actions

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.application.ReadAction
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.PyClass
import com.jetbrains.python.psi.search.PyClassInheritorsSearch
import edg.EdgirUtils.SimpleLibraryPath
import edg.ElemBuilder
import edg.wir.DesignPath
import edg.wir.ProtoUtil._
import edg_ide.{EdgirUtils, PsiUtils}
import edg_ide.ui.{BlockVisualizerService, PopupUtils}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption, ExceptSeq}
import edg_ide.util.{DesignFindBlockOfTypes, exceptionPopup}

import scala.jdk.CollectionConverters.CollectionHasAsScala

object FocusToElementAction {
  // Sorting function for block paths, comparing them to some context path.
  // Prefers the context path, then subpaths, then parent paths.
  def pathSortFn(contextPath: DesignPath)(compare1: DesignPath, compare2: DesignPath): Boolean = {
    if (compare1 == contextPath && compare2 != contextPath) {
      true // Prefer exact match first
    } else if (compare1.startsWith(contextPath) && !compare2.startsWith(contextPath)) {
      true // Prefer children next
    } else if (contextPath.startsWith(compare1) && !contextPath.startsWith(compare2)) {
      true // Prefer parents next
    } else {
      false
    }
  }
}

class FocusToElementAction() extends AnAction() {
  val notificationGroup: NotificationGroup =
    NotificationGroup.balloonGroup("edg_ide.actions.NavigateToBlockAction")

  case class NavigateNode(desc: String, action: () => Unit) {
    override def toString: String = desc
  }

  override def actionPerformed(event: AnActionEvent): Unit = {
    val editor = event.getData(CommonDataKeys.EDITOR)
    if (editor == null) {
      notificationGroup
        .createNotification("No editor", NotificationType.WARNING)
        .notify(event.getProject)
    }

    exceptionPopup(editor) {
      val visualizer = BlockVisualizerService(event.getProject)
      val design = visualizer.getDesign.exceptNone("no design")
      val (contextPath, contextBlock) = visualizer.getContextBlock.exceptNone("no visualizer context")

      val psiFile = event.getData(CommonDataKeys.PSI_FILE).exceptNull("no file")
      val offset = editor.getCaretModel.getOffset
      val element = psiFile.findElementAt(offset).exceptNull(s"invalid caret position in ${psiFile.getName}")
      val containingClass =
        PsiTreeUtil.getParentOfType(element, classOf[PyClass]).exceptNull("not in a class")

      val refName = PsiUtils.selfReferenceOption(element).exceptError

      val extendedClasses = ReadAction.compute(() => {
        val inheritors = PyClassInheritorsSearch.search(containingClass, true).findAll().asScala
        inheritors.toSeq :+ containingClass
      })
      val targetTypes = extendedClasses.map { pyClass =>
        ElemBuilder.LibraryPath(pyClass.getQualifiedName)
      }.toSet

      val instancesOfClass = new DesignFindBlockOfTypes(targetTypes)
        .map(design)
        .exceptEmpty(s"no ${containingClass.getName} in design")
        .sortWith { case ((blockPath1, block1), (blockPath2, block2)) =>
          FocusToElementAction.pathSortFn(contextPath)(blockPath1, blockPath2)
        }

      val matchBlockPathTypes = instancesOfClass
        .collect {
          case (blockPath, block) if block.ports.toSeqMap.contains(refName) =>
            (blockPath, refName, block, EdgirUtils.typeOfPortLike(block.ports(refName)))
          case (blockPath, block) if block.blocks.toSeqMap.contains(refName) =>
            (blockPath, refName, block, EdgirUtils.typeOfBlockLike(block.blocks(refName)))
          case (blockPath, block) if block.links.toSeqMap.contains(refName) =>
            (blockPath, refName, block, EdgirUtils.typeOfLinkLike(block.links(refName)))
        }
        .exceptEmpty(s"no ${containingClass.getName} containing $refName")

      val items = matchBlockPathTypes
        .map { case (blockPath, refName, block, desc) =>
          val eltTypeStr = desc.map(_.toSimpleString).getOrElse("???")
          val blockTypeStr = block.getSelfClass.toSimpleString
          val descStr = s"$eltTypeStr $refName in $blockTypeStr $blockPath"
          NavigateNode(
            descStr,
            () => {
              visualizer.setContext(blockPath)
              visualizer.selectPath(blockPath + refName)
            }
          )
        }
        .exceptEmpty("no navigation targets")

      if (items.length == 1) {
        items.head.action()
      } else {
        PopupUtils.createMenuPopup(s"Navigate to ${containingClass.getName}.$refName", items, editor) {
          selected =>
            selected.action()
        }
      }
    }
  }
}
