package edg_ide.psi_edits

import com.intellij.openapi.application.{ModalityState, ReadAction}
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.util.concurrency.AppExecutorUtil
import com.jetbrains.python.psi._
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.{ExceptNotify, ExceptSeq}
import edg_ide.util.{DesignAnalysisUtils, exceptable, requireExcept}

import java.util.concurrent.Callable

object InsertPortAction {
  val VALID_FUNCTION_NAME = "__init__" // unlike blocks, ports can only be in __init__ so they can be used
  val VALID_SUPERCLASS = "edg_core.HierarchyBlock.Block" // TODO dedup w/ InsertBlockAction

  /** Creates an action to insert a port of type libClass after some PSI element after. Validation is performed before
    * the action is generated, though the action itself may also return an error.
    *
    * TODO dedup w/ InsertBlockAction?
    */
  def createInsertPortFlow(
      after: PsiElement,
      libClass: PyClass,
      actionName: String,
      project: Project,
      continuation: (String, PsiElement) => Unit
  ): Errorable[() => Unit] = exceptable {
    val containingPsiList = after.getParent
      .instanceOfExcept[PyStatementList](s"invalid position for insertion in ${after.getContainingFile.getName}")
    val containingPsiFunction = PsiTreeUtil.getParentOfType(containingPsiList, classOf[PyFunction])
      .exceptNull(s"not in a function in ${containingPsiList.getContainingFile.getName}")
    requireExcept(containingPsiFunction.getName == VALID_FUNCTION_NAME, s"not in function $VALID_FUNCTION_NAME")
    val containingPsiClass = PsiTreeUtil.getParentOfType(containingPsiFunction, classOf[PyClass])
      .exceptNull(s"not in a class in ${containingPsiFunction.getContainingFile.getName}")

    def insertPortFlow: Unit = {
      InsertAction.createClassMemberNameEntryPopup("Port Name", containingPsiClass, project) { name =>
        exceptable {
          ReadAction.nonBlocking((() => {
            val languageLevel = LanguageLevel.forElement(after)
            val psiElementGenerator = PyElementGenerator.getInstance(project)
            val selfName = containingPsiFunction.getParameterList.getParameters.toSeq
              .exceptEmpty(s"function ${containingPsiFunction.getName} has no self")
              .head.getName
            val newAssign = psiElementGenerator.createFromText(
              languageLevel,
              classOf[PyAssignmentStatement],
              s"$selfName.$name = $selfName.Port(${libClass.getName}())"
            )

            val initParams = DesignAnalysisUtils.initParamsOf(libClass, project).toOption.getOrElse((Seq(), Seq()))
            val allParams = initParams._1 ++ initParams._2

            // TODO should this insert args for ports?
            for (initParam <- allParams) {
              if (initParam.getName != "model") { // TODO remove once model is removed
                val kwArg = psiElementGenerator.createKeywordArgument(languageLevel, initParam.getName, "...")

                val defaultValue = initParam.getDefaultValue
                if (defaultValue != null) {
                  kwArg.getValueExpression.replace(defaultValue)
                }

                newAssign.getAssignedValue.asInstanceOf[PyCallExpression]
                  .getArgument(0, classOf[PyCallExpression])
                  .getArgumentList.addArgument(kwArg)
              }
            }
            newAssign
          }): Callable[PyAssignmentStatement]).finishOnUiThread(
            ModalityState.defaultModalityState(),
            newAssign => {
              val added = writeCommandAction(project).withName(actionName).compute(() => {
                containingPsiList.addAfter(newAssign, after)
              })
              continuation(name, added)
            }
          ).submit(AppExecutorUtil.getAppExecutorService)
        }
      }
    }
    () => insertPortFlow
  }
}
