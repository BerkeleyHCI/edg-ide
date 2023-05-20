package edg_ide.psi_edits

import com.intellij.codeInsight.template.TemplateManager
import com.intellij.openapi.fileEditor.{FileEditorManager, TextEditor}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi._
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.{ExceptNotify, ExceptSeq}
import edg_ide.util.exceptable


object InsertBlockAction {
  // sorted by preference
  val VALID_FUNCTION_NAMES = Seq("contents", "__init__")  // TODO support generators
  val VALID_SUPERCLASS = "edg_core.HierarchyBlock.Block"


  /** Creates an action to insert a block of type libClass after some PSI element after.
    * Validation is performed before the action is generated, though the action itself may also return an error.
    */
  def createInsertBlockFlow(after: PsiElement, libClass: PyClass, actionName: String,
                            project: Project,
                            continuation: (String, PsiElement) => Unit): Errorable[() => Unit] = exceptable {
    val fileEditor = FileEditorManager.getInstance(project).getSelectedEditor(after.getContainingFile.getVirtualFile)
    val editor = fileEditor match {
      case editor: TextEditor => editor.getEditor
      case _ => throw new IllegalArgumentException()
    }
    val manager = TemplateManager.getInstance(project)


    val containingPsiList = after.getParent
        .instanceOfExcept[PyStatementList](s"invalid position for insertion in ${after.getContainingFile.getName}")
    val containingPsiFunction = PsiTreeUtil.getParentOfType(containingPsiList, classOf[PyFunction])
        .exceptNull(s"not in a function in ${containingPsiList.getContainingFile.getName}")
    val containingPsiClass = PsiTreeUtil.getParentOfType(containingPsiFunction, classOf[PyClass])
        .exceptNull(s"not in a class in ${containingPsiFunction.getContainingFile.getName}")

    val languageLevel = LanguageLevel.forElement(after)
    val psiElementGenerator = PyElementGenerator.getInstance(project)
    val selfName = containingPsiFunction.getParameterList.getParameters.toSeq
        .exceptEmpty(s"function ${containingPsiFunction.getName} has no self")
        .head.getName
    val assignAst = psiElementGenerator.createFromText(languageLevel,
      classOf[PyAssignmentStatement], s"$selfName.block = $selfName.Block(${libClass.getName}())")



    () => new InsertionLiveTemplate[PyAssignmentStatement](project, editor, actionName, after, assignAst, IndexedSeq(
      new InsertionLiveTemplate.Reference("name", psi => psi.getTargets.head.asInstanceOf[PyTargetExpression],
        InsertionLiveTemplate.validatePythonName(_, _, Some(containingPsiClass))),
      new InsertionLiveTemplate.Variable("assign", psi => psi.getAssignedValue)
    )).run()

//    val containingPsiList = after.getParent
//        .instanceOfExcept[PyStatementList](s"invalid position for insertion in ${after.getContainingFile.getName}")
//    val containingPsiFunction = PsiTreeUtil.getParentOfType(containingPsiList, classOf[PyFunction])
//        .exceptNull(s"not in a function in ${containingPsiList.getContainingFile.getName}")
//    val containingPsiClass = PsiTreeUtil.getParentOfType(containingPsiFunction, classOf[PyClass])
//        .exceptNull(s"not in a class in ${containingPsiFunction.getContainingFile.getName}")
//
//    def insertBlockFlow: Unit = {
//      InsertAction.createClassMemberNameEntryPopup("Block Name", containingPsiClass, project) { name => exceptable {
//        ReadAction.nonBlocking((() => {  // analyses happen in the background to avoid slow ops in UI thread
//          val languageLevel = LanguageLevel.forElement(after)
//          val psiElementGenerator = PyElementGenerator.getInstance(project)
//          val selfName = containingPsiFunction.getParameterList.getParameters.toSeq
//              .exceptEmpty(s"function ${containingPsiFunction.getName} has no self")
//              .head.getName
//          val newAssign = psiElementGenerator.createFromText(languageLevel,
//            classOf[PyAssignmentStatement], s"$selfName.$name = $selfName.Block(${libClass.getName}())")
//
//          val initParams = DesignAnalysisUtils.initParamsOf(libClass, project).toOption.getOrElse((Seq(), Seq()))
//          val allParams = initParams._1 ++ initParams._2
//
//          // TODO move into DesignAnalysisUtils
//          val kwArgs = allParams.flatMap { initParam =>
//            // Only create default values for required arguments, ignoring defaults
//            // TODO: better detection of "required" args
//            val defaultValue = initParam.getDefaultValue
//            if (defaultValue == null
//                || defaultValue.textMatches("RangeExpr()") || defaultValue.textMatches("FloatExpr()")
//                || defaultValue.textMatches("IntExpr()") || defaultValue.textMatches("BoolExpr()")
//                || defaultValue.textMatches("StringExpr()")) {
//              val kwArg = psiElementGenerator.createKeywordArgument(languageLevel,
//                initParam.getName, "...")
//
//              if (defaultValue != null) {
//                kwArg.getValueExpression.replace(defaultValue)
//              }
//              Some(kwArg)
//            } else {
//              None
//            }
//          }
//          kwArgs.foreach { kwArg =>
//            newAssign.getAssignedValue.asInstanceOf[PyCallExpression]
//                .getArgument(0, classOf[PyCallExpression])
//                .getArgumentList.addArgument(kwArg)
//          }
//
//          newAssign
//        }): Callable[PyAssignmentStatement]).finishOnUiThread(ModalityState.defaultModalityState(), newAssign => {
//          val added = writeCommandAction(project).withName(actionName).compute(() => {
//            containingPsiList.addAfter(newAssign, after)
//          })
//          continuation(name, added)
//        }).submit(AppExecutorUtil.getAppExecutorService)
//      }}
//    }
//    () => insertBlockFlow
  }
}
