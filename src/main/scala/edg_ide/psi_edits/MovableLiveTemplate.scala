package edg_ide.psi_edits

import com.intellij.codeInsight.template.TemplateEditingAdapter
import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.editor.event.{EditorMouseEvent, EditorMouseListener}
import com.intellij.psi.PsiElement

import scala.collection.mutable


/** Wrapper around a Template that allows the template to move by user clicks  */
abstract class MovableLiveTemplate(actionName: String) {
  protected var currentTemplateState: Option[TemplateState] = None
  protected var movingTemplateListener: Option[EditorMouseListener] = None
  protected val templateStateListeners = mutable.ListBuffer[TemplateEditingAdapter]()

  // given the PSI element at the current caret,
  // inserts the new element for this template and returns the inserted element
  // implement me
  def startTemplate(caretEltOpt: Option[PsiElement]): TemplateState

  // starts the movable live template, given the PSI element at the current caret
  def run(caretEltOpt: Option[PsiElement]): Unit = {
    val templateState = startTemplate(caretEltOpt)
    val editor = templateState.getEditor
    currentTemplateState = Some(templateState)

    if (movingTemplateListener.isEmpty) {
      movingTemplateListener = Some(new EditorMouseListener {
        override def mouseClicked(event: EditorMouseEvent): Unit = {
          if (templateState.isFinished) {
            editor.removeEditorMouseListener(movingTemplateListener.get)
            return
          }
          if (!event.getMouseEvent.isAltDown) { // only move on mod+click, to allow copy-paste flows
            return
          }
          val offset = event.getOffset
          val expressionContext = templateState.getExpressionContextForSegment(0)
          if (expressionContext.getTemplateStartOffset <= offset && offset < expressionContext.getTemplateEndOffset) {
            return // ignore clicks within the template
          }
          event.consume()

          writeCommandAction(editor.getProject).withName(s"move $actionName").compute(() => {
            InsertionLiveTemplate.deleteTemplate(templateState)
          })
        }
      })
      editor.addEditorMouseListener(movingTemplateListener.get)
    }
  }

  // Adds a template state listener, installed into the current template (if active) and into
  // any further instantiated moved templates.
  // Not thread-safe, should not be interleaved with run().
  def addTemplateStateListener(listener: TemplateEditingAdapter): Unit = {
    currentTemplateState.foreach(_.addTemplateStateListener(listener))
    templateStateListeners.append(listener)
  }
}
