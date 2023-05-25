package edg_ide.psi_edits

import com.intellij.codeInsight.template.{Template, TemplateEditingAdapter}
import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.psi.PsiElement

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

    // delete the PSI elements, which also ends the template
    deleteElems.foreach { deleteElem =>
      if (deleteElem.isValid) { // guard in case the element changed from a prior deletion
        deleteElem.delete()
      }
    }
    templateState.update()
  }
}


/** Utility on top of TemplateEditingAdapter that provides a templateFinished that provides both
  * the TemplateState (note that some fields may be invalid) and brokenOff in one templateFinished function.
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
