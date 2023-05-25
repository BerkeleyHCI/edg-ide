package edg_ide.psi_edits

import com.intellij.codeInsight.template.TemplateEditingAdapter
import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.editor.event.{EditorMouseEvent, EditorMouseListener}
import com.intellij.psi.{PsiDocumentManager, PsiElement}
import com.jetbrains.python.psi.PyStatement

import scala.collection.mutable


/** Wrapper around a Template that allows the template to move by user clicks  */
abstract class MovableLiveTemplate(actionName: String) {
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
      val caretElement = psiFile.findElementAt(offset) // get the caret element before modifying the AST
      writeCommandAction(project).withName(s"move $actionName").compute(() => {
        InsertionLiveTemplate.deleteTemplate(templateState) // also cancels the currently active template
        templateState.update
        run(Some(caretElement))
      })
    }
  }

  // starts the movable live template, given the PSI element at the current caret
  // must be called within a writeCommandAction
  def run(caretEltOpt: Option[PsiElement]): Unit = {
    val templateState = startTemplate(caretEltOpt).run()
    currentTemplateState = Some(templateState)
    templateStateListeners.foreach(templateState.addTemplateStateListener(_))

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
