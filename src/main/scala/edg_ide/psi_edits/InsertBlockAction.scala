package edg_ide.psi_edits

import com.intellij.codeInsight.template.impl.{ConstantNode, TemplateState}
import com.intellij.codeInsight.template.{Template, TemplateBuilderImpl, TemplateEditingAdapter, TemplateManager}
import com.intellij.openapi.application.{ApplicationManager, ModalityState, ReadAction}
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.fileEditor.{FileEditorManager, OpenFileDescriptor, TextEditor}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi.{PsiDocumentManager, PsiElement}
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.refactoring.rename.inplace.InplaceRefactoring
import com.intellij.util.concurrency.AppExecutorUtil
import com.jetbrains.python.psi._
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.{ExceptNotify, ExceptSeq}
import edg_ide.util.{DesignAnalysisUtils, exceptable}

import java.util.concurrent.Callable


object InsertBlockAction {
  // sorted by preference
  val VALID_FUNCTION_NAMES = Seq("contents", "__init__")  // TODO support generators
  val VALID_SUPERCLASS = "edg_core.HierarchyBlock.Block"

  private class TemplateListener extends TemplateEditingAdapter {
    override def beforeTemplateFinished(state: TemplateState, template: Template): Unit = {
      println(f"TemplateListener::beforeTemplateFinished")
      super.beforeTemplateFinished(state, template)
    }
    override def templateFinished(template: Template, brokenOff: Boolean): Unit = {
      println(f"TemplateListener::templateFinished($brokenOff)")
      super.templateFinished(template, brokenOff)
    }

    override def currentVariableChanged(templateState: TemplateState, template: Template, oldIndex: Int, newIndex: Int): Unit = {
      println(f"TemplateListener::currentVariableChanged($oldIndex, $newIndex)")
      super.currentVariableChanged(templateState, template, oldIndex, newIndex)
    }

    override def waitingForInput(template: Template): Unit = {
      println(f"TemplateListener::waitingForInput")
      super.waitingForInput(template)
    }

    override def templateCancelled(template: Template): Unit = {
      println(f"TemplateListener::templateCancelled")
      super.templateCancelled(template)
    }
  }

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

//    val template: Template = manager.createTemplate("", "")
//    template.addTextSegment("ducks")
////    template.addSelectionStartVariable()
////    template.addTextSegment("selection1")
////    template.addSelectionEndVariable()
////    template.addTextSegment("quacks")
////    template.addVariable("name", "dv", "dv", false)
//
//    template.setToIndent(false)
//    template.setToReformat(false)

    def run: Unit = {
      val newAssign = writeCommandAction(project).withName(actionName).compute(() => {
        val newAssign = containingPsiList.addAfter(assignAst, after)
        PsiDocumentManager.getInstance(project).commitDocument(editor.getDocument)
        newAssign
      })

      ApplicationManager.getApplication.runWriteAction(new Runnable {
        override def run(): Unit = {
          val builder = new TemplateBuilderImpl(containingPsiList)
          val assignText = newAssign.getText
          //        val assignRange = TextRange.create(0, assignText.length)
          builder.replaceElement(newAssign, "kAssignName", new ConstantNode(newAssign.getText), true)
          //    builder.setSelection(after)
          builder.setEndVariableAfter(newAssign)

          val template = builder.buildInlineTemplate()  // specifically must be an inline template (actually replace the PSI elements), otherwise the block of new code is inserted at the caret

          new OpenFileDescriptor(project, after.getContainingFile.getVirtualFile, newAssign.getTextRange.getStartOffset)
            .navigate(true) // sets focus on the text editor so the user can type into the template
          editor.getCaretModel.moveToOffset(containingPsiList.getTextOffset)
          manager.startTemplate(editor, template, new TemplateListener)

          newAssign
        }
      })

    }

    () => run

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
