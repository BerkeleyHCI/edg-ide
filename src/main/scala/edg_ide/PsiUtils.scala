package edg_ide

import com.intellij.openapi.project.Project
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiDocumentManager, PsiElement}
import com.jetbrains.python.psi.{PyFunction, PyReferenceExpression, PyTargetExpression}
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.{ExceptNotify, ExceptSeq}
import edg_ide.util.exceptable

object PsiUtils {
  def fileLineOf(element: PsiElement, project: Project): Errorable[String] = {
    exceptable {
      val psiFile = element.getContainingFile.exceptNull("no file")
      val psiDocumentManager = PsiDocumentManager.getInstance(project)
      val psiDocument = psiDocumentManager.getDocument(psiFile).exceptNull("no document")
      val lineNumber = psiDocument.getLineNumber(element.getTextOffset)
      s"${psiFile.getName}:${lineNumber + 1}"
    }
  }

  def fileNextLineOf(element: PsiElement, project: Project): Errorable[String] = exceptable {
    val psiFile = element.getContainingFile.exceptNull("no file")
    val psiDocumentManager = PsiDocumentManager.getInstance(project)
    val psiDocument = psiDocumentManager.getDocument(psiFile).exceptNull("no document")
    val endLineNumber = psiDocument.getLineNumber(element.getTextOffset + element.getTextLength)
    s"${psiFile.getName}:${endLineNumber + 2}"
  }

  /** If element is a ReferenceExpression or TargetExpression of the form 'self.xyz', returns Some(xyz).
    * Does not truncate (will fail on self.xyz.abc).
    * Accounts for different self names within a function.
    */
  def selfReferenceOption(element: PsiElement): Errorable[String] = exceptable {
    val containingFunction = PsiTreeUtil.getParentOfType(element, classOf[PyFunction])
        .exceptNull("not in a function")
    val selfName = containingFunction.getParameterList.getParameters.toSeq
        .exceptEmpty(s"function ${containingFunction.getName} has no self")
        .head.getName

    val optRef = Option(PsiTreeUtil.getTopmostParentOfType(element, classOf[PyReferenceExpression]))
    val optTarget = Option(PsiTreeUtil.getTopmostParentOfType(element, classOf[PyTargetExpression]))
    (optRef, optTarget) match {
      case (Some(ref), _) if ref.getQualifier != null && ref.getQualifier.textMatches(selfName) =>
        ref.getName
      case (None, Some(target)) if target.getQualifier != null &&  target.getQualifier.textMatches(selfName) =>
        target.getName
      case _ => exceptable.fail("not in a reference expression")
    }
  }
}
