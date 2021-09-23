package edg_ide.actions

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent}
import edg_ide.ui.{BlockVisualizerService, PopupUtils}
import edg_ide.util.ExceptionNotifyImplicits.ExceptOption
import edg_ide.util.{exceptable, exceptionNotify, requireExcept}

import java.io.File


class SetKicadLibraryDirectoryAction() extends AnAction() {
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.actions.SetKicadLibraryDirectoryAction")

  override def actionPerformed(event: AnActionEvent): Unit = exceptionNotify(notificationGroup, event.getProject) {
    val visualizer = BlockVisualizerService(event.getProject).visualizerPanelOption.exceptNone("no visualizer")

    PopupUtils.createStringEntryPopup("KiCad Library Directory", event.getProject) { dir => exceptable {
      val dirFile = new File(dir)
      requireExcept(dirFile.exists, "invalid path")
      requireExcept(dirFile.isDirectory, "not a directory")
      visualizer.setKicadLibraryDirectory(dirFile.getCanonicalPath)
    }}

    notificationGroup.createNotification(
      s"IDE Cache Emptied",
      NotificationType.INFORMATION
    ).notify(event.getProject)
  }
}
