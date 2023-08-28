package edg_ide.psi_edits

import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.{PyClass, PyFunction, PyStatement}
import edgir.elem.elem
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.{ExceptNotify, ExceptOption}
import edg_ide.util.{ConnectBuilder, PortConnects, exceptable, requireExcept}

object LiveTemplateConnect {
  // Creates an action to start a live template to insert the connection
  def createTemplateConnect(
      contextClass: PyClass,
      actionName: String,
      project: Project,
      container: elem.HierarchyBlock,
      baseConnected: ConnectBuilder,
      newConnects: Seq[PortConnects.Base],
      continuation: (String, PsiElement) => Unit
  ): Errorable[() => Unit] = exceptable {

    val movableLiveTemplate = new MovableLiveTemplate(actionName) {
      // TODO startTemplate should be able to fail - Errorable
      override def startTemplate(caretEltOpt: Option[PsiElement]): InsertionLiveTemplate = {
        val caretElt = caretEltOpt.exceptNone("no elt at caret")
        val caretStatement = InsertAction.snapInsertionEltOfType[PyStatement](caretElt).get
        val containingPsiFn = PsiTreeUtil
          .getParentOfType(caretStatement, classOf[PyFunction])
          .exceptNull(s"caret not in a function")
        val containingPsiClass = PsiTreeUtil
          .getParentOfType(containingPsiFn, classOf[PyClass])
          .exceptNull(s"caret not in a class")
        requireExcept(containingPsiClass == contextClass, s"caret not in class of type ${contextClass.getName}")
        caretStatement
      }
    }

    movableLiveTemplate.addTemplateStateListener(new TemplateFinishedListener {
      override def templateFinished(state: TemplateState, brokenOff: Boolean): Unit = {
        ???
      }
    })

    val caretElt = InsertAction.getCaretForNewClassStatement(contextClass, project).toOption
    def insertBlockFlow: Unit = {
      writeCommandAction(project)
        .withName(s"$actionName")
        .compute(() => {
          movableLiveTemplate.run(caretElt)
        })
    }

    () => insertBlockFlow
  }
}
