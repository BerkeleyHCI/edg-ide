package edg_ide.psi_edits

import com.intellij.codeInsight.template.TemplateManager
import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.event.{EditorMouseEvent, EditorMouseListener}
import com.intellij.psi.PsiElement


/** Wrapper around a Template that allows the template to move by user clicks  */
abstract class MovableLiveTemplate(editor: Editor, templateState: TemplateState) {
//  // given the PSI element at the current caret,
//  // inserts the new element for this template and returns the inserted element
//  def doInsertion(caretElt: PsiElement): PsiElement
//
//  def run(): TemplateState = {
//    var movingTemplateListener: EditorMouseListener = null
//    movingTemplateListener = new EditorMouseListener {
//      override def mouseClicked(event: EditorMouseEvent): Unit = {
//        if (templateState.isFinished) {
//          editor.removeEditorMouseListener(movingTemplateListener)
//          return
//        }
//        val offset = event.getOffset
//        val expressionContext = templateState.getExpressionContextForSegment(0)
//        if (expressionContext.getTemplateStartOffset <= offset && offset < expressionContext.getTemplateEndOffset) {
//          return // ignore clicks within the template
//        }
//        templateState.gotoEnd(true)
//        TemplateManager.getInstance(editor.getProject).finishTemplate(editor)
//      }
//    }
//    editor.addEditorMouseListener(movingTemplateListener)
//  }
}
