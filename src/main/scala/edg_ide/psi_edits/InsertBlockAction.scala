package edg_ide.psi_edits

import com.intellij.codeInsight.template.TemplateManager
import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.editor.event.{EditorMouseAdapter, EditorMouseEvent, EditorMouseListener}
import com.intellij.openapi.fileEditor.{FileEditorManager, TextEditor}
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiElement, PsiWhiteSpace}
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi._
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption, ExceptSeq}
import edg_ide.util.{DesignAnalysisUtils, exceptable, requireExcept}


object InsertBlockAction {
  // sorted by preference
  val VALID_FUNCTION_NAMES = Seq("contents", "__init__")  // TODO support generators
  val VALID_SUPERCLASS = "edg_core.HierarchyBlock.Block"


  /** Creates an action to insert a block of type libClass after some PSI element after.
    * Validation is performed before the action is generated, though the action itself may also return an error.
    */
  def createInsertBlockFlow(contextClass: PyClass, libClass: PyClass, actionName: String,
                            project: Project,
                            continuation: (String, PsiElement) => Unit): Errorable[() => Unit] = exceptable {
    val fileEditor = FileEditorManager.getInstance(project).getSelectedEditor(contextClass.getContainingFile.getVirtualFile)
    val editor = fileEditor match {
      case editor: TextEditor => editor.getEditor
      case _ => throw new IllegalArgumentException()
    }

    val languageLevel = LanguageLevel.forElement(libClass)
    val psiElementGenerator = PyElementGenerator.getInstance(project)

    // given some caret position, returns the best insertion position
    def getInsertionContainerAfter(caretEltOpt: Option[PsiElement]): (PsiElement, PsiElement) = {
      val caretAfter = exceptable {
        val caretElt = caretEltOpt.exceptNone("caret not in class")
        val containingPsiList = caretElt.getParent
            .instanceOfExcept[PyStatementList](s"caret not in a statement-list")
        val containingPsiFunction = PsiTreeUtil.getParentOfType(containingPsiList, classOf[PyFunction])
            .exceptNull(s"caret not in a function")
        val containingPsiClass = PsiTreeUtil.getParentOfType(containingPsiFunction, classOf[PyClass])
            .exceptNull(s"caret not in a class")
        requireExcept(containingPsiClass == contextClass, s"caret not in class of type ${libClass.getName}")
        caretElt
      }.toOption.orElse({
        val candidates = InsertAction.findInsertionElements(contextClass, InsertBlockAction.VALID_FUNCTION_NAMES)
        candidates.headOption
      }).get
      (PsiTreeUtil.getParentOfType(caretAfter, classOf[PyStatementList]), caretAfter)
    }

    val movableLiveTemplate = new MovableLiveTemplate(project, actionName) {
      override def startTemplate(caretEltOpt: Option[PsiElement]): TemplateState = {
        val (insertContainer, insertAfter) = getInsertionContainerAfter(caretEltOpt)
        val containingPsiFunction = PsiTreeUtil.getParentOfType(insertContainer, classOf[PyFunction])
        val containingPsiClass = PsiTreeUtil.getParentOfType(containingPsiFunction, classOf[PyClass])
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

        val nameVar = new InsertionLiveTemplate.Reference[PyAssignmentStatement](
          "name", psi => psi.getTargets.head.asInstanceOf[PyTargetExpression],
          InsertionLiveTemplate.validatePythonName(_, _, Some(containingPsiClass)),
          defaultValue = Some("")
        )

        val templateVars = allParams.map { initParam =>
          val newArgIndex = newArgList.getArguments.length
          val defaultValue = initParam.getDefaultValue

          val paramName = initParam.getName() + (Option(initParam.getAnnotationValue) match {
            case Some(typed) => f": $typed"
            case None => ""
          })

          val (newArg, newVariable) = if (defaultValue == null) { // required argument, needs ellipsis
            (psiElementGenerator.createEllipsis(),
                new InsertionLiveTemplate.Variable[PyAssignmentStatement](paramName,
                  psi => argListExtractor(psi).getArguments()(newArgIndex))
            )
          } else { // optional argument
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

        new InsertionLiveTemplate[PyAssignmentStatement](project, editor, insertAfter, newAssign,
          IndexedSeq(nameVar) ++ templateVars
        ).run()
      }
    }

    movableLiveTemplate.addTemplateStateListener(new TemplateFinishedListener {
      override def templateFinished(state: TemplateState, brokenOff: Boolean): Unit = {
        super.templateFinished(state, brokenOff)

        val insertedName = state.getVariableValue("name").getText
        if (insertedName.isEmpty && brokenOff) { // canceled by esc
          writeCommandAction(project).withName(s"cancel $actionName").compute(() => {
            InsertionLiveTemplate.deleteTemplate(state)
          })
        } else {
          var templateElem = state.getExpressionContextForSegment(0).getPsiElementAtStartOffset
          while (templateElem.isInstanceOf[PsiWhiteSpace]) { // ignore inserted whitespace before the statement
            templateElem = templateElem.getNextSibling
          }
          try {
            val templateStmt = templateElem match {
              case stmt: PyAssignmentStatement => stmt
              case _ => return // can't reformat
            }
            val args = templateStmt.getAssignedValue.asInstanceOf[PyCallExpression] // the self.Block(...) call
                .getArgument(0, classOf[PyCallExpression]) // the object instantiation
                .getArgumentList // args to the object instantiation
            val deleteArgs = args.getArguments.flatMap { // remove empty kwargs
              case arg: PyKeywordArgument => if (arg.getValueExpression == null) Some(arg) else None
              case _ => None // ignored
            }
            writeCommandAction(project).withName(s"clean $actionName").compute(() => {
              deleteArgs.foreach(_.delete())
            })
          } finally {
            continuation(insertedName, templateElem)
          }
        }
      }
    })

    val caretElt = InsertAction.getCaretForNewClassStatement(contextClass, project).toOption
    def insertBlockFlow: Unit = {
      writeCommandAction(project).withName(s"$actionName").compute(() => {
        movableLiveTemplate.run(caretElt)
      })
    }
    () => insertBlockFlow
  }
}
