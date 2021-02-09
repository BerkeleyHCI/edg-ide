package edg_ide.actions

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import edg_ide.EdgirUtils
import edg_ide.ui.EdgCompilerService
import edg_ide.util.ErrorableNotify._


class DiscardCachedModuleAction() extends AnAction() {
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.actions.DiscardCachedModuleAction")

  override def actionPerformed(event: AnActionEvent): Unit = {
    val psiFile = Errorable(event.getData(CommonDataKeys.PSI_FILE), "No PSI file")
    val module = psiFile.flatMap(s"PSI File $psiFile not in project ${event.getProject.getBaseDir}") { psiFile =>
      ModuleUtil.from(event.getProject.getBaseDir, psiFile.getVirtualFile)
    }

    module.mapOrNotify("edg_ide.actions.DiscardCachedModuleAction", event.getProject) { module =>
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
    }
  }
}
