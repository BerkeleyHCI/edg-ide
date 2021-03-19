package edg_ide.actions

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi._
import edg_ide.ui.PopupUtils
import edg_ide.util.ExceptionNotifyImplicits._
import edg_ide.util.{exceptable, exceptionNotify}


case class TestItem(x: String)

class TestAction() extends AnAction() {
  override def actionPerformed(event: AnActionEvent): Unit = exceptable {
    val editor = event.getData(CommonDataKeys.EDITOR).exceptNull("No editor")
    val offset = editor.getCaretModel.getOffset
    val psiFile = event.getData(CommonDataKeys.PSI_FILE).exceptNull("No PSI file")

    PopupUtils.createMenuPopup("test", Seq(TestItem("item1"), TestItem("item2")), editor) { sel =>
      println(s"selected $sel")
    }
  }
}
