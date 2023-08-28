package edg_ide.psi_edits

import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.jetbrains.python.psi.PyClass
import edgir.elem.elem
import edg.util.Errorable
import edg_ide.util.{ConnectBuilder, PortConnects, exceptable}

object LiveTemplateConnect {
  // Creates an action to start a live template to insert the connection
  def createTemplateConnect(
      contextClass: PyClass,
      actionName: String,
      project: Project,
      container: elem.HierarchyBlock,
      baseConnected: ConnectBuilder,
      newConnects: PortConnects.ConstraintBase,
      continuation: (String, PsiElement) => Unit
  ): Errorable[() => Unit] = exceptable {

    val movableLiveTemplate = new MovableLiveTemplate(actionName) {
      override def startTemplate(caretEltOpt: Option[PsiElement]): InsertionLiveTemplate = {
        ???
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
