package edg_ide.psi_edits

import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.project.Project
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiElement, PsiWhiteSpace}
import com.jetbrains.python.psi._
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.{ExceptNotify, ExceptOption, ExceptSeq}
import edg_ide.util.{DesignAnalysisUtils, exceptable, requireExcept}

object LiveTemplateInsertBlock {

  /** Creates an action to start a live template to insert a block.
    */
  def createTemplateBlock(
      contextClass: PyClass,
      libClass: PyClass,
      actionName: String,
      project: Project,
      continuation: (String, PsiElement) => Unit
  ): Errorable[() => Unit] = exceptable {
    val languageLevel = LanguageLevel.forElement(libClass)
    val psiElementGenerator = PyElementGenerator.getInstance(project)

    val movableLiveTemplate = new MovableLiveTemplate(actionName) {
      // TODO startTemplate should be able to fail - Errorable
      override def startTemplate(caretEltOpt: Option[PsiElement]): InsertionLiveTemplate = {
        val insertAfter = caretEltOpt.flatMap(TemplateUtils.getInsertionStmt(_, contextClass))
          .getOrElse(InsertAction.findInsertionElements(contextClass, InsertBlockAction.VALID_FUNCTION_NAMES).head)
        val containingPsiFn = PsiTreeUtil.getParentOfType(insertAfter, classOf[PyFunction])
        val containingPsiClass = PsiTreeUtil.getParentOfType(containingPsiFn, classOf[PyClass])
        val selfName = containingPsiFn.getParameterList.getParameters.toSeq
          .exceptEmpty(s"function ${containingPsiFn.getName} has no self")
          .head
          .getName

        val newAssignTemplate = psiElementGenerator.createFromText(
          languageLevel,
          classOf[PyAssignmentStatement],
          s"$selfName.name = $selfName.Block(${libClass.getName}())"
        )
        val containingStmtList = PsiTreeUtil.getParentOfType(insertAfter, classOf[PyStatementList])

        val newAssign =
          containingStmtList.addAfter(newAssignTemplate, insertAfter).asInstanceOf[PyAssignmentStatement]
        val newArgList = newAssign.getAssignedValue
          .asInstanceOf[PyCallExpression]
          .getArgument(0, classOf[PyCallExpression])
          .getArgumentList

        val initParams =
          DesignAnalysisUtils.initParamsOf(libClass, project).toOption.getOrElse((Seq(), Seq()))
        val allParams = initParams._1 ++ initParams._2

        val nameTemplateVar = new InsertionLiveTemplate.Reference(
          "name",
          newAssign.getTargets.head.asInstanceOf[PyTargetExpression],
          InsertionLiveTemplate.validatePythonName(_, _, Some(containingPsiClass)),
          defaultValue = Some("")
        )

        val argTemplateVars = allParams.map { initParam =>
          val paramName = initParam.getName() + (Option(initParam.getAnnotationValue) match {
            case Some(typed) => f": $typed"
            case None => ""
          })

          if (initParam.getDefaultValue == null) { // required argument, needs ellipsis
            newArgList.addArgument(psiElementGenerator.createEllipsis())
            new InsertionLiveTemplate.Variable(paramName, newArgList.getArguments.last)
          } else { // optional argument
            // ellipsis is generated in the AST to give the thing a handle, the template replaces it with an empty
            newArgList.addArgument(
              psiElementGenerator.createKeywordArgument(languageLevel, initParam.getName, "...")
            )
            new InsertionLiveTemplate.Variable(
              f"$paramName (optional)",
              newArgList.getArguments.last.asInstanceOf[PyKeywordArgument].getValueExpression,
              defaultValue = Some("")
            )
          }
        }

        new InsertionLiveTemplate(newAssign, IndexedSeq(nameTemplateVar) ++ argTemplateVars)
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
        } else {
          var templateElem = state.getExpressionContextForSegment(0).getPsiElementAtStartOffset
          while (templateElem.isInstanceOf[PsiWhiteSpace]) { // ignore inserted whitespace before the statement
            templateElem = templateElem.getNextSibling
          }
          try {
            val args = templateElem
              .asInstanceOf[PyAssignmentStatement]
              .getAssignedValue
              .asInstanceOf[PyCallExpression] // the self.Block(...) call
              .getArgument(0, classOf[PyCallExpression]) // the object instantiation
              .getArgumentList // args to the object instantiation
            val deleteArgs = args.getArguments.flatMap { // remove empty kwargs
              case arg: PyKeywordArgument => if (arg.getValueExpression == null) Some(arg) else None
              case _ => None // ignored
            }
            writeCommandAction(project)
              .withName(s"clean $actionName")
              .compute(() => {
                deleteArgs.foreach(_.delete())
              })
          } catch {
            case _: Throwable => // ignore
          }
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
