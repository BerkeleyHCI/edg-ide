package edg_ide

import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.actionSystem.CommonDataKeys
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.Messages
import com.intellij.pom.Navigatable
import org.jetbrains.annotations.NotNull
import org.jetbrains.annotations.Nullable
import javax.swing._


class TestDialogAction() extends AnAction() {
  override def actionPerformed(event: AnActionEvent): Unit = {
    val nav = event.getData(CommonDataKeys.NAVIGATABLE)

    Messages.showMessageDialog(event.getProject,
      s"${event.getPresentation.getText}\nselected element: ${nav}",
      event.getPresentation.getDescription,
      Messages.getInformationIcon)
  }

  override def update(e: AnActionEvent): Unit = {
    val project = e.getProject
    // visible only when project is open
    e.getPresentation.setEnabledAndVisible(project != null)
  }
}
