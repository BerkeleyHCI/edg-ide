package edg_ide.actions

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi._
import edg_ide.util.ExceptionNotifyImplicits._
import edg_ide.util.exceptionNotify


class TestAction() extends AnAction() {
  override def actionPerformed(event: AnActionEvent): Unit = {
    exceptionNotify.successNotify("edg_ide.actions.TestAction", event.getProject) {
      val editor = event.getData(CommonDataKeys.EDITOR).exceptNull("No editor")
      val offset = editor.getCaretModel.getOffset
      val psiFile = event.getData(CommonDataKeys.PSI_FILE).exceptNull("No PSI file")
      val psiElement = psiFile.findElementAt(offset).exceptNull("No PSI element")

      val refElement = PsiTreeUtil.getParentOfType(psiElement, classOf[PyReferenceExpression])
          .exceptNull(s"Not a reference expression, got $psiElement")

      refElement.getReference.resolve().toString
    }
  }
}
