package edg_ide.psi_edits

import com.intellij.codeInsight.template.{Template, TemplateEditingAdapter}
import com.intellij.codeInsight.template.impl.TemplateState


/** Utility on top of TemplateEditingAdapter that provides a templateFinished that provides both
  * the TemplateState (note that some fields may be invalid) and brokenOff in one templateFinished function.
  */
class TemplateFinishedListener() extends TemplateEditingAdapter {
  private var finishedTemplateState: Option[TemplateState] = None

  def templateFinished(state: TemplateState, brokenOff: Boolean): Unit = { }

  override final def beforeTemplateFinished(state: TemplateState, template: Template): Unit = {
    super.beforeTemplateFinished(state, template)
    finishedTemplateState = Some(state) // save the state for when we have brokenOff
  }

  override final def templateFinished(template: Template, brokenOff: Boolean): Unit = {
    super.templateFinished(template, brokenOff)
    templateFinished(finishedTemplateState.get, brokenOff)
  }
}
