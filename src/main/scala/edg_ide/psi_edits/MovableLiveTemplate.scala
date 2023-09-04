package edg_ide.psi_edits

import com.intellij.codeInsight.template.TemplateEditingAdapter
import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.editor.event.{EditorMouseEvent, EditorMouseListener}
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiDocumentManager, PsiElement, PsiWhiteSpace}
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.ExceptErrorable
import edg_ide.util.exceptable

import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala

/** Wrapper around a Template that allows the template to move by user clicks */
abstract class MovableLiveTemplate(actionName: String) {
  protected val kHelpTooltip = "[Enter] next; [Esc] end; [Alt+click] move"

  protected var currentTemplateState: Option[TemplateState] = None
  protected var movingTemplateListener: Option[EditorMouseListener] = None
  protected val templateStateListeners = mutable.ListBuffer[TemplateEditingAdapter]()

  // given the PSI element at the current caret,
  // inserts the new element for this template and returns the inserted element
  // this is run in the same writeCommandAction as the template creation
  // implement me
  def createTemplate(caretEltOpt: Option[PsiElement]): InsertionLiveTemplate

  class MovingMouseListener(templateState: TemplateState) extends EditorMouseListener {
    override def mouseClicked(event: EditorMouseEvent): Unit = {
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

      val project = templateState.getProject
      val psiFile = PsiDocumentManager.getInstance(project).getPsiFile(event.getEditor.getDocument)
      val caretElement =
        psiFile.findElementAt(offset) match { // get the caret element before modifying the AST
          // whitespace may be modified by the template delete and become invalid
          case caretElement: PsiWhiteSpace => caretElement.getPrevSibling
          case caretElement => caretElement
        }

      // save template state before deleting the template
      val templatePrev = (0 until templateState.getCurrentVariableNumber).map {
        templateState.getTemplate.getVariables.get(_).getName
      }
      val templateValues = templateState.getTemplate.getVariables.asScala.map { variable =>
        variable.getName -> templateState.getVariableValue(variable.getName).getText
      }.toMap

      writeCommandAction(project)
        .withName(s"move $actionName")
        .compute(() => {
          TemplateUtils.deleteTemplate(templateState)
          run(Some(caretElement), Some((templatePrev, templateValues)))
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
      caretEltOpt: Option[PsiElement] = None,
      priorTemplatePosValues: Option[(Seq[String], Map[String, String])] = None
  ): Errorable[TemplateState] = exceptable {
    // on error, this fails without updating state variables, as if it wasn't started
    val priorTemplateValues = priorTemplatePosValues.map(_._2).getOrElse(Map())
    val templateState = createTemplate(caretEltOpt).run(kHelpTooltip, priorTemplateValues).exceptError
    currentTemplateState = Some(templateState)
    templateStateListeners.foreach(templateState.addTemplateStateListener(_))
    priorTemplatePosValues.foreach { case (templatePrev, _) => // advance to the previous variable position
      val variables = templateState.getTemplate.getVariables.asScala
      variables.takeWhile(variable => templatePrev.contains(variable.getName)).foreach { variable =>
        templateState.nextTab()
      }
    }

    val editor = templateState.getEditor
    movingTemplateListener.foreach(editor.removeEditorMouseListener)
    movingTemplateListener = Some(new MovingMouseListener(templateState))
    editor.addEditorMouseListener(movingTemplateListener.get)
    templateState
  }

  // Adds a template state listener, installed into the current template (if active) and into
  // any further instantiated moved templates.
  // Not thread-safe, should not be interleaved with run().
  def addTemplateStateListener(listener: TemplateEditingAdapter): Unit = {
    currentTemplateState.foreach(_.addTemplateStateListener(listener))
    templateStateListeners.append(listener)
  }

  // call externally to start the live template
  // TODO refactor to not require project, project should be inferred from startTemplate
  def start(project: Project, caretEltOpt: Option[PsiElement]): Errorable[Unit] = exceptable {
    writeCommandAction(project)
      .withName(actionName)
      .compute(() => {
        run(caretEltOpt).exceptError
      })
  }
}
