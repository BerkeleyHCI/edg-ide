package edg_ide.actions

import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.fileEditor.OpenFileDescriptor
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiElement, PsiParserFacade}
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.{LanguageLevel, PyAssignmentStatement, PyCallExpression, PyClass, PyElementGenerator, PyFunction, PyKeywordArgument, PyStatementList}
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.{ExceptNotify, ExceptSeq}
import edg_ide.util.{DesignAnalysisUtils, exceptable}


object InsertBlockAction {
  val VALID_FUNCTION_NAMES = Seq("__init__", "contents")  // TODO support generators
  val VALID_SUPERCLASS = "edg_core.HierarchyBlock.Block"

  /** Creates an action to insert a block of type libClass after some PSI element after.
    * Validation is performed before the action is generated, though the action itself may also return an error.
    */
  def createInsertBlockFlow(after: PsiElement, libClass: PyClass, actionName: String,
                            project: Project,
                            continuation: (String, PsiElement) => Unit): Errorable[() => Unit] = exceptable {
    val containingPsiList = after.getParent
        .instanceOfExcept[PyStatementList](s"invalid position for insertion in ${after.getContainingFile.getName}")
    val containingPsiFunction = containingPsiList.getParent
        .instanceOfExcept[PyFunction](s"not in a function in ${containingPsiList.getContainingFile.getName}")
    val containingPsiClass = PsiTreeUtil.getParentOfType(containingPsiFunction, classOf[PyClass])
        .exceptNull(s"not in a class in ${containingPsiFunction.getContainingFile.getName}")

    val psiElementGenerator = PyElementGenerator.getInstance(project)
    val selfName = containingPsiFunction.getParameterList.getParameters.toSeq
        .exceptEmpty(s"function ${containingPsiFunction.getName} has no self")
        .head.getName

    val initParams = DesignAnalysisUtils.initParamsOf(libClass, project).toOption.getOrElse((Seq(), Seq()))
    val allParams = initParams._1 ++ initParams._2

    def insertBlockFlow: Unit = {
      InsertAction.createClassMemberNameEntryPopup("Block Name", containingPsiClass, project) { name => exceptable {
        val languageLevel = LanguageLevel.forElement(after)
        val newAssign = psiElementGenerator.createFromText(languageLevel,
          classOf[PyAssignmentStatement], s"$selfName.$name = $selfName.Block(${libClass.getName}())")

        for (initParam <- allParams) {
          // Only create default values for required arguments, ignoring defaults
          // TODO: better detection of "required" args
          val defaultValue = initParam.getDefaultValue
          if (defaultValue.textMatches("RangeExpr()") || defaultValue.textMatches("FloatExpr()")
              || defaultValue.textMatches("IntExpr()") || defaultValue.textMatches("BoolExpr()")
              || defaultValue.textMatches("StringExpr()") || defaultValue == null) {
            val kwArg = psiElementGenerator.createKeywordArgument(languageLevel,
              initParam.getName, "...")

            if (defaultValue != null) {
              kwArg.getValueExpression.replace(defaultValue)
            }

            newAssign.getAssignedValue.asInstanceOf[PyCallExpression]
                .getArgument(0, classOf[PyCallExpression])
                .getArgumentList.addArgument(kwArg)
          }
        }

        val added = writeCommandAction(project).withName(actionName).compute(() => {
          containingPsiList.addAfter(newAssign, after)
        })
        continuation(name, added)
      }}
    }
    () => insertBlockFlow
  }
}
