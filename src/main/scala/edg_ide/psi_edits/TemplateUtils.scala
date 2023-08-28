package edg_ide.psi_edits

import com.intellij.codeInsight.template.{Template, TemplateEditingAdapter}
import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.{PyClass, PyFunction, PyStatement}
import edg_ide.util.ExceptionNotifyImplicits.ExceptNotify
import edg_ide.util.requireExcept

import scala.collection.mutable

object TemplateUtils {
  // deletes the template text, ending the template
  // must be run within a writeCommandAction
  def deleteTemplate(templateState: TemplateState): Unit = {
    val templateExpression = templateState.getExpressionContextForSegment(0)
    val templateEndOffset = templateExpression.getTemplateEndOffset
    var templateElem = templateExpression.getPsiElementAtStartOffset

    // separate the traversal from deletion, so the end offsets are stable as we build the deletion list
    val deleteElems = mutable.ListBuffer[PsiElement]()
    while (templateElem != null && templateElem.getTextOffset <= templateEndOffset) {
      if (templateElem.getTextRange.getStartOffset < templateExpression.getTemplateStartOffset) {
        // getPsiElementAtStartOffset may return a non-contained element if the template is empty
        templateElem = templateElem.getNextSibling
      } else if (templateElem.getTextRange.getEndOffset <= templateExpression.getTemplateEndOffset) {
        deleteElems.append(templateElem)
        templateElem = templateElem.getNextSibling
      } else { // otherwise recurse into element to get a partial range
        templateElem = templateElem.getFirstChild
      }
    }

    deleteElems.foreach { deleteElem =>
      if (deleteElem.isValid) { // guard in case the element changed from a prior deletion
        deleteElem.delete()
      }
    }
    templateState.update() // update to end the template
  }

  // given some caret position, returns the top-level statement if it's valid for statement insertion
  def getInsertionStmt(caretElt: PsiElement, requiredContextClass: PyClass): Option[PyStatement] = {
    val caretStatement = InsertAction.snapInsertionEltOfType[PyStatement](caretElt).get
    val containingPsiFn = PsiTreeUtil
      .getParentOfType(caretStatement, classOf[PyFunction])
    if (containingPsiFn == null) return None
    val containingPsiClass = PsiTreeUtil
      .getParentOfType(containingPsiFn, classOf[PyClass])
    if (containingPsiClass == null) return None
    if (containingPsiClass != requiredContextClass) return None
    Some(caretStatement)
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
