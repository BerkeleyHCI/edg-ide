package edg_ide.psi_edits

import com.intellij.codeInsight.template.TemplateEditingAdapter
import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.editor.event.{EditorMouseEvent, EditorMouseListener}
import com.intellij.psi.{PsiDocumentManager, PsiElement, PsiWhiteSpace}

import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala


/** Wrapper around a Template that allows the template to move by user clicks  */
abstract class MovableLiveTemplate(actionName: String) {
  private val kHelpTooltip = "[Enter] next; [Esc] end; [Alt+click] move"

  protected var currentTemplateState: Option[TemplateState] = None
  protected var movingTemplateListener: Option[EditorMouseListener] = None
  protected val templateStateListeners = mutable.ListBuffer[TemplateEditingAdapter]()

  // given the PSI element at the current caret,
  // inserts the new element for this template and returns the inserted element
  // this is run in the same writeCommandAction as the template creation
  // implement me
  def startTemplate(caretEltOpt: Option[PsiElement]): InsertionLiveTemplate

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
      val caretElement = psiFile.findElementAt(offset) match { // get the caret element before modifying the AST
        // whitespace may be modified by the template delete and become invalid
        case caretElement: PsiWhiteSpace => caretElement.getPrevSibling
        case caretElement => caretElement
      }

      val templatePos = templateState.getCurrentVariableNumber  // save template state before deleting the template
      val templateValues = templateState.getTemplate.getVariables.asScala.map { variable =>
        templateState.getVariableValue(variable.getName).getText
      }.toSeq

      writeCommandAction(project).withName(s"move $actionName").compute(() => {
        TemplateUtils.deleteTemplate(templateState)
        run(Some(caretElement), Some((templatePos, templateValues)))
      })
    }
  }

  // starts the movable live template, given the PSI element at the current caret
  // must be called within a writeCommandAction
  def run(caretEltOpt: Option[PsiElement], priorTemplateValues: Option[(Int, Seq[String])] = None): Unit = {
    val tooltipString = priorTemplateValues match {
      case Some(_) => None  // tooltip only shows on initial template insertion, not on moves
      case None => Some(kHelpTooltip)
    }
    val templateState = startTemplate(caretEltOpt).run(tooltipString, priorTemplateValues.map(_._2))
    currentTemplateState = Some(templateState)
    templateStateListeners.foreach(templateState.addTemplateStateListener(_))
    priorTemplateValues.foreach { case (templatePos, _) =>  // advance to the previous variable position
      (0 until templatePos).foreach { i =>
        templateState.nextTab()
      }
    }

    val editor = templateState.getEditor
    movingTemplateListener.foreach(editor.removeEditorMouseListener)
    movingTemplateListener = Some(new MovingMouseListener(templateState))
    editor.addEditorMouseListener(movingTemplateListener.get)
  }

  // Adds a template state listener, installed into the current template (if active) and into
  // any further instantiated moved templates.
  // Not thread-safe, should not be interleaved with run().
  def addTemplateStateListener(listener: TemplateEditingAdapter): Unit = {
    currentTemplateState.foreach(_.addTemplateStateListener(listener))
    templateStateListeners.append(listener)
  }
}
