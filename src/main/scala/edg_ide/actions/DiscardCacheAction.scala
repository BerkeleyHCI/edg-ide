package edg_ide.actions

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.PyClass
import edg_ide.EdgirUtils
import edg_ide.ui.{BlockVisualizerService, EdgCompilerService}
import edg_ide.util.Errorable

import scala.collection.mutable


class DiscardCacheAction() extends AnAction() {
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.actions.DiscaradCacheAction")

  override def actionPerformed(event: AnActionEvent): Unit = {
    val psiFile = Errorable(event.getData(CommonDataKeys.PSI_FILE), "No PSI file")
    val module = psiFile.mapOption(s"PSI File $psiFile not in project ${event.getProject.getBaseDir}") { psiFile =>
      ModuleUtil.from(event.getProject.getBaseDir, psiFile.getVirtualFile)
    }
    module match {
      case Errorable.Success(module) =>
        val moduleName = module.mkString(".")
        val discarded = EdgCompilerService(event.getProject).pyLib.clearCache(moduleName)
        if (discarded.nonEmpty) {
          notificationGroup.createNotification(
            s"${discarded.length} items discarded from $moduleName",
            "",
            discarded.map { EdgirUtils.SimpleLibraryPath }.mkString("\n"),
            NotificationType.INFORMATION
          ).notify(event.getProject)
        } else {
          notificationGroup.createNotification(
            s"No items discarded from $moduleName",
            NotificationType.WARNING
          ).notify(event.getProject)
        }
      case Errorable.Error(msg) =>
        notificationGroup.createNotification(msg, NotificationType.WARNING).notify(event.getProject)
    }
  }
}
