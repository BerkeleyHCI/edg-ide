package edg_ide.actions

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.search.searches.ReferencesSearch
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi._
import edg_ide.ui.PopupUtils
import edg_ide.util.ExceptionNotifyImplicits._
import edg_ide.util.{exceptable, exceptionNotify}

import scala.jdk.CollectionConverters.CollectionHasAsScala


case class TestItem(x: String)

class TestAction() extends AnAction() {
  override def actionPerformed(event: AnActionEvent): Unit = exceptable {
    val editor = event.getData(CommonDataKeys.EDITOR).exceptNull("No editor")
    val psiFile = event.getData(CommonDataKeys.PSI_FILE).exceptNull("No PSI file")
    val offset = editor.getCaretModel.getOffset
    val leaf = psiFile.findElementAt(offset).exceptNull(s"invalid caret position in ${psiFile.getName}")
    val element = PsiTreeUtil.getParentOfType(leaf, classOf[PyTargetExpression]).exceptNull("not a TargetExpr")

    println(element + "@ " + element.getText)

    val searchScope = GlobalSearchScope.projectScope(event.getProject)
    val results = ReferencesSearch.search(element, searchScope).findAll().asScala

    println(s"Results ${results.size} " + results.mkString(", "))
  }
}
