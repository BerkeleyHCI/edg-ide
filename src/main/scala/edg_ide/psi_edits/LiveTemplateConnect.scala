package edg_ide.psi_edits

import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiElement, PsiWhiteSpace}
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.{
  LanguageLevel,
  PyAssignmentStatement,
  PyCallExpression,
  PyClass,
  PyElementGenerator,
  PyFunction,
  PyStatement,
  PyStatementList,
  PyTargetExpression
}
import edgir.elem.elem
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.{ExceptNotify, ExceptOption, ExceptSeq}
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
      continuation: (String, PsiElement) => Unit,
  ): Errorable[() => Unit] = exceptable {
    val languageLevel = LanguageLevel.forElement(contextClass)
    val psiElementGenerator = PyElementGenerator.getInstance(project)

    val movableLiveTemplate = new MovableLiveTemplate(actionName) {
      // TODO startTemplate should be able to fail - Errorable
      override def startTemplate(caretEltOpt: Option[PsiElement]): InsertionLiveTemplate = {
        val validCaretEltOpt = caretEltOpt.flatMap(TemplateUtils.getInsertionStmt(_, contextClass))
        // TODO support append to existing connect
        val insertAfter = validCaretEltOpt
          .getOrElse(InsertAction.findInsertionElements(contextClass, InsertBlockAction.VALID_FUNCTION_NAMES).head)

        val containingPsiFn = PsiTreeUtil.getParentOfType(insertAfter, classOf[PyFunction])
        val containingPsiClass = PsiTreeUtil.getParentOfType(containingPsiFn, classOf[PyClass])
        val selfName = containingPsiFn.getParameterList.getParameters.toSeq
          .exceptEmpty(s"function ${containingPsiFn.getName} has no self")
          .head
          .getName
        val containingStmtList = PsiTreeUtil.getParentOfType(insertAfter, classOf[PyStatementList])

        val newConnectTemplate = psiElementGenerator.createFromText(
          languageLevel,
          classOf[PyAssignmentStatement],
          s"$selfName.name = $selfName.connect()"
        )
        val newConnect =
          containingStmtList.addAfter(newConnectTemplate, insertAfter).asInstanceOf[PyAssignmentStatement]
        val newConnectArgs = newConnect.getAssignedValue
          .asInstanceOf[PyCallExpression]
          .getArgumentList

        // TODO ADD ALL THE CONNECT ELTS

        val nameTemplateVar = new InsertionLiveTemplate.Reference(
          "name",
          newConnect.getTargets.head.asInstanceOf[PyTargetExpression],
          InsertionLiveTemplate.validatePythonName(_, _, Some(containingPsiClass)),
          defaultValue = Some("")
        )

        new InsertionLiveTemplate(newConnect, IndexedSeq(nameTemplateVar))
      }
    }

    movableLiveTemplate.addTemplateStateListener(new TemplateFinishedListener {
      override def templateFinished(state: TemplateState, brokenOff: Boolean): Unit = {
        val expr = state.getExpressionContextForSegment(0)
        if (expr.getTemplateEndOffset <= expr.getTemplateStartOffset) {
          return // ignored if template was deleted, including through moving the template
        }

        val insertedName = state.getVariableValue("name").getText
        if (insertedName.isEmpty && brokenOff) { // canceled by esc while name is empty
          writeCommandAction(project)
            .withName(s"cancel $actionName")
            .compute(() => {
              TemplateUtils.deleteTemplate(state)
            })
        } else { // commit
          var templateElem = state.getExpressionContextForSegment(0).getPsiElementAtStartOffset
          while (templateElem.isInstanceOf[PsiWhiteSpace]) { // ignore inserted whitespace before the statement
            templateElem = templateElem.getNextSibling
          }

          // TODO CLEAN UP CONNECT STMT

          continuation(insertedName, templateElem)
        }
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
