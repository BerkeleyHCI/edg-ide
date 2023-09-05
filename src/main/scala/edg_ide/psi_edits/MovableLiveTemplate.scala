package edg_ide.psi_edits

import com.intellij.codeInsight.template.{TemplateEditingAdapter, TemplateManager}
import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.event.{EditorMouseEvent, EditorMouseListener}
import com.intellij.openapi.project.Project
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.ExceptErrorable
import edg_ide.util.exceptable

import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala

/** Wrapper around a Template that allows the template to move by user clicks */
abstract class MovableLiveTemplate(actionName: String) {
  private lazy val logger = Logger.getInstance(this.getClass)

  protected val kHelpTooltip = "[Enter] next; [Esc] end; [Alt+click] move"

  protected var currentTemplate: Option[TemplateState] = None
  protected var movingTemplateListener: Option[EditorMouseListener] = None
  protected val templateStateListeners = mutable.ListBuffer[TemplateEditingAdapter]()

  // this tracks template variables values, by variable name, between movements
  // and by design persists values of variables that may disappear and reappear across movements
  protected val persistTemplateVariableValues = mutable.HashMap[String, String]()

  // given the PSI element at the current caret,
  // inserts the new element for this template and returns the inserted element
  // this is run in the same writeCommandAction as the template creation
  // implement me
  def createTemplate(): InsertionLiveTemplate

  class MovingMouseListener(template: InsertionLiveTemplate, templateState: TemplateState) extends EditorMouseListener {
    override def mouseClicked(event: EditorMouseEvent): Unit = {
      // note: when the mouse is clocked, the caret moves to the new position, so the position / element
      // does not need to be explicitly managed (instead, createTemplate() should be position-aware)
      if (!event.getMouseEvent.isAltDown) { // only move on mod+click, to allow copy-paste flows
        return
      }
      if (templateState.isFinished) {
        event.getEditor.removeEditorMouseListener(this)
        return
      }
      val offset = event.getOffset
      val expressionContext = templateState.getExpressionContextForSegment(0)
      if (expressionContext.getTemplateStartOffset <= offset && offset < expressionContext.getTemplateEndOffset) {
        return // ignore clicks within the template
      }
      event.consume()

      // save template state before deleting the template
      val templatePrev = (0 until templateState.getCurrentVariableNumber).map {
        templateState.getTemplate.getVariables.get(_).getName
      }
      templateState.getTemplate.getVariables.asScala.foreach { variable =>
        persistTemplateVariableValues.update(variable.getName, templateState.getVariableValue(variable.getName).getText)
      }

      writeCommandAction(templateState.getProject)
        .withName(s"move $actionName")
        .compute(() => {
          template.deleteTemplate()
          templateState.gotoEnd(true) // end the template to avoid overlapping templates
          run(Some((templatePrev, persistTemplateVariableValues.toMap))) match {
            case Errorable.Error(message) => logger.error(message)
            case _ =>
          }
        })
    }
  }

  // starts the movable live template, optionally given the PSI element at the current caret
  // and the prior template state (on a move)
  //
  // prior template state specified as a list of prior variable names (indicating variable position),
  // and map of names to values - this allows the template to do the right thing even as the variable
  // list changes
  //
  // must be called within a writeCommandAction
  protected def run(
      priorTemplatePosValues: Option[(Seq[String], Map[String, String])] = None
  ): Errorable[TemplateState] = exceptable {
    // on error, this fails without updating state variables, as if it wasn't started
    val priorTemplateValues = priorTemplatePosValues.map(_._2).getOrElse(Map())
    val newTemplate = createTemplate()
    val templateState = newTemplate.run(kHelpTooltip, priorTemplateValues).exceptError
    currentTemplate = Some(templateState)
    templateStateListeners.foreach(templateState.addTemplateStateListener(_))
    priorTemplatePosValues.foreach { case (templatePrev, _) => // advance to the previous variable position
      val variables = templateState.getTemplate.getVariables.asScala
      variables.takeWhile(variable => templatePrev.contains(variable.getName)).foreach { variable =>
        templateState.nextTab()
      }
    }

    val editor = templateState.getEditor
    movingTemplateListener.foreach(editor.removeEditorMouseListener)
    movingTemplateListener = Some(new MovingMouseListener(newTemplate, templateState))
    editor.addEditorMouseListener(movingTemplateListener.get)
    templateState
  }

  // Adds a template state listener, installed into the current template (if active) and into
  // any further instantiated moved templates.
  // Not thread-safe, should not be interleaved with run().
  def addTemplateStateListener(listener: TemplateEditingAdapter): Unit = {
    currentTemplate.foreach(_.addTemplateStateListener(listener))
    templateStateListeners.append(listener)
  }

  // call externally to start the live template
  // TODO refactor to not require project, project should be inferred from startTemplate
  def start(project: Project): Errorable[Unit] = exceptable {
    writeCommandAction(project).withName(actionName).compute(() => {
      run().exceptError
    })
  }
}
