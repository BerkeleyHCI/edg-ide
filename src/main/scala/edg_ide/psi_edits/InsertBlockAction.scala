package edg_ide.psi_edits

import com.intellij.openapi.editor.event.{EditorMouseAdapter, EditorMouseEvent, EditorMouseListener}
import com.intellij.openapi.fileEditor.{FileEditorManager, TextEditor}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi._
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.{ExceptNotify, ExceptSeq}
import edg_ide.util.{DesignAnalysisUtils, exceptable}


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

    val containingPsiList = after.getParent
        .instanceOfExcept[PyStatementList](s"invalid position for insertion in ${after.getContainingFile.getName}")
    val containingPsiFunction = PsiTreeUtil.getParentOfType(containingPsiList, classOf[PyFunction])
        .exceptNull(s"not in a function in ${containingPsiList.getContainingFile.getName}")
    val containingPsiClass = PsiTreeUtil.getParentOfType(containingPsiFunction, classOf[PyClass])
        .exceptNull(s"not in a class in ${containingPsiFunction.getContainingFile.getName}")

    def insertBlockFlow: Unit = {
      val languageLevel = LanguageLevel.forElement(after)
      val psiElementGenerator = PyElementGenerator.getInstance(project)
      val selfName = containingPsiFunction.getParameterList.getParameters.toSeq
          .exceptEmpty(s"function ${containingPsiFunction.getName} has no self")
          .head.getName
      val newAssign = psiElementGenerator.createFromText(languageLevel,
        classOf[PyAssignmentStatement], s"$selfName.name = $selfName.Block(${libClass.getName}())")
      val argListExtractor = (x: PyAssignmentStatement) => x.getAssignedValue.asInstanceOf[PyCallExpression]
          .getArgument(0, classOf[PyCallExpression])
          .getArgumentList
      val newArgList = argListExtractor(newAssign)

      val initParams = DesignAnalysisUtils.initParamsOf(libClass, project).toOption.getOrElse((Seq(), Seq()))
      val allParams = initParams._1 ++ initParams._2

      val templateVars = allParams.map { initParam =>
        val newArgIndex = newArgList.getArguments.length
        val defaultValue = initParam.getDefaultValue

        val paramName = initParam.getName() + (Option(initParam.getAnnotationValue) match {
          case Some(typed) => f": $typed"
          case None => ""
        })

        val (newArg, newVariable) = if (defaultValue == null) {  // required argument, needs ellipsis
          (psiElementGenerator.createEllipsis(),
              new InsertionLiveTemplate.Variable[PyAssignmentStatement](paramName,
                psi => argListExtractor(psi).getArguments()(newArgIndex))
          )
        } else {  // optional argument
          // ellipsis is generated in the AST to give the thing a handle, the template replaces it with an empty
          (psiElementGenerator.createKeywordArgument(languageLevel, initParam.getName, "..."),
              new InsertionLiveTemplate.Variable[PyAssignmentStatement](f"$paramName (optional)",
                psi => argListExtractor(psi).getArguments()(newArgIndex).asInstanceOf[PyKeywordArgument].getValueExpression,
                defaultValue = Some(""))
          )
        }
        newArgList.addArgument(newArg)

        newVariable
      }

      val templateState = new InsertionLiveTemplate[PyAssignmentStatement](project, editor, actionName, after, newAssign,
        IndexedSeq(
          new InsertionLiveTemplate.Reference[PyAssignmentStatement](
            "name", psi => psi.getTargets.head.asInstanceOf[PyTargetExpression],
            InsertionLiveTemplate.validatePythonName(_, _, Some(containingPsiClass)),
            defaultValue = Some(""))
        ) ++ templateVars
      ).run()
      editor.addEditorMouseListener(new EditorMouseListener {
        override def mouseClicked (event: EditorMouseEvent): Unit = {
          println(event.getOffset)
          templateState.gotoEnd(true)
        }
      })
    }
    () => insertBlockFlow
  }
}
