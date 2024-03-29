package edg_ide.psi_edits

import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.codeInsight.template.{Template, TemplateEditingAdapter}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.{PyClass, PyFunction, PyStatement, PyStatementList}
import edg_ide.util.DesignAnalysisUtils

object TemplateUtils {
  // given some caret position, returns the top-level statement if it's valid for statement insertion
  def getInsertionStmt(caretElt: PsiElement, requiredContextClass: PyClass): Option[PsiElement] = {
    val caretStatement = InsertAction.snapToContainerChild(caretElt, classOf[PyStatementList])
      .getOrElse(return None)
    val containingPsiFn = Option(PsiTreeUtil.getParentOfType(caretStatement, classOf[PyFunction]))
      .getOrElse(return None)
    val containingPsiClass = Option(PsiTreeUtil.getParentOfType(containingPsiFn, classOf[PyClass]))
      .getOrElse(return None)
    if (containingPsiClass != requiredContextClass) return None
    Some(caretStatement)
  }

  // given a class and a list of attributes, returns the last attribute assignment, if any
  def getLastAttributeAssignment(psiClass: PyClass, attrs: Seq[String], project: Project): Option[PyStatement] = {
    attrs.flatMap { attr =>
      DesignAnalysisUtils.findAssignmentsTo(psiClass, attr, project)
    }.sortWith { case (a, b) =>
      DesignAnalysisUtils.elementAfterEdg(a, b, project).getOrElse(false)
    }.lastOption
  }
}

/** Utility on top of TemplateEditingAdapter that provides a templateFinished that provides both the TemplateState (note
  * that some fields may be invalid) and brokenOff in one templateFinished function.
  */
abstract class TemplateFinishedListener extends TemplateEditingAdapter {
  private var finishedTemplateState: Option[TemplateState] = None

  // Called when the template finishes, whether by completion or esc-ing.
  // NOT called when the template is cancelled, by the user editing outside the template.
  def templateFinished(state: TemplateState, brokenOff: Boolean): Unit

  override final def beforeTemplateFinished(state: TemplateState, template: Template): Unit = {
    super.beforeTemplateFinished(state, template)
    finishedTemplateState = Some(state) // save the state for when we have brokenOff
  }

  override final def templateFinished(template: Template, brokenOff: Boolean): Unit = {
    super.templateFinished(template, brokenOff)
    templateFinished(finishedTemplateState.get, brokenOff)
  }
}
