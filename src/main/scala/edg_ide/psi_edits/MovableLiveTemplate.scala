package edg_ide.psi_edits

import com.intellij.codeInsight.template.TemplateEditingAdapter
import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.event.{EditorMouseEvent, EditorMouseListener}
import com.intellij.psi.PsiElement

import scala.collection.mutable


/** Wrapper around a Template that allows the template to move by user clicks  */
abstract class MovableLiveTemplate(editor: Editor, actionName: String) {
  protected var currentTemplateState: Option[TemplateState] = None
  protected val templateStateListeners = mutable.ListBuffer[TemplateEditingAdapter]()

  // given the PSI element at the current caret,
  // inserts the new element for this template and returns the inserted element
  // implement me
  def startTemplate(caretElt: PsiElement): TemplateState

  // optional hook for when the template completes
  // not called when the template moves, or when the template is cancelled by the user
  def onTemplateCompleted(state: TemplateState, brokenOff: Boolean): Unit = {
  }

  // starts the movable live template, given the PSI element at the current caret
  def run(caretElt: PsiElement): Unit = {
    val templateState = startTemplate(caretElt)
    currentTemplateState = Some(templateState)

    var movingTemplateListener: EditorMouseListener = null
    movingTemplateListener = new EditorMouseListener {
      override def mouseClicked(event: EditorMouseEvent): Unit = {
        if (templateState.isFinished) {
          editor.removeEditorMouseListener(movingTemplateListener)
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
    }
    editor.addEditorMouseListener(movingTemplateListener)
    // since the Template and TemplateState may change as it moves, it is not returned since it's not stable
  }

  // Adds a template state listener, installed into the current template (if active) and into
  // any further instantiated moved templates.
  // Not thread-safe, should not be interleaved with run().
  def addTemplateStateListener(listener: TemplateEditingAdapter): Unit = {
    currentTemplateState.foreach(_.addTemplateStateListener(listener))
    templateStateListeners.append(listener)
  }
}
