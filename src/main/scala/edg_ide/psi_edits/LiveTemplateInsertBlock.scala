package edg_ide.psi_edits

import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Editor
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi._
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.ExceptSeq
import edg_ide.util.{DesignAnalysisUtils, exceptable}

class BlockInsertionLiveTemplate(
    contextClass: PyClass,
    libClass: PyClass,
    actionName: String,
    continuation: (String, PsiElement) => Unit
) extends InsertionLiveTemplate(contextClass.getContainingFile) {
  private lazy val logger = Logger.getInstance(this.getClass)

  protected val languageLevel = LanguageLevel.forElement(libClass)
  protected val project = contextClass.getProject
  protected val psiElementGenerator = PyElementGenerator.getInstance(project)

  protected var newAssignStmt: Option[PyAssignmentStatement] = None

  override protected def buildTemplateAst(editor: Editor)
      : Errorable[(PsiElement, Seq[PsiElement], Seq[InsertionLiveTemplateVariable])] = exceptable {
    val insertAfter = Option(contextClass.getContainingFile.findElementAt(editor.getCaretModel.getOffset))
      .flatMap(TemplateUtils.getInsertionStmt(_, contextClass))
      .getOrElse(InsertAction.findInsertionElements(contextClass, InsertBlockAction.VALID_FUNCTION_NAMES).head)
    val containingPsiFn = PsiTreeUtil.getParentOfType(insertAfter, classOf[PyFunction])
    val containingPsiClass = PsiTreeUtil.getParentOfType(containingPsiFn, classOf[PyClass])
    val selfName = containingPsiFn.getParameterList.getParameters.toSeq
      .exceptEmpty(s"function ${containingPsiFn.getName} has no self")
      .head.getName

    val newAssignTemplate = psiElementGenerator.createFromText(
      languageLevel,
      classOf[PyAssignmentStatement],
      s"$selfName.name = $selfName.Block(${libClass.getName}())"
    )
    val containingStmtList = PsiTreeUtil.getParentOfType(insertAfter, classOf[PyStatementList])

    val newAssign =
      containingStmtList.addAfter(newAssignTemplate, insertAfter).asInstanceOf[PyAssignmentStatement]
    newAssignStmt = Some(newAssign)

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

    (newAssign, Seq(), nameTemplateVar +: argTemplateVars)
  }

  override def deleteTemplate(): Unit = {
    newAssignStmt.foreach(_.delete())
    newAssignStmt = None
  }

  override protected def cleanCompletedTemplate(state: TemplateState): Unit = {
    super.cleanCompletedTemplate(state)

    // getExpressionContextForSegment seems needed to update the PSI element, otherwise .getAssignedValue returns null
    state.getExpressionContextForSegment(0).getPsiElementAtStartOffset
    try {
      writeCommandAction(project).withName(s"clean $actionName").compute(() => {
        val args = newAssignStmt.get
          .getAssignedValue.asInstanceOf[PyCallExpression] // the self.Block(...) call
          .getArgument(0, classOf[PyCallExpression]) // the object instantiation
          .getArgumentList // args to the object instantiation
        val deleteArgs = args.getArguments.flatMap { // remove empty kwargs
          case arg: PyKeywordArgument if arg.getValueExpression == null => Some(arg)
          case _ => None // ignored
        }
        deleteArgs.foreach(_.delete())
      })
    } catch {
      case exc: Throwable => logger.error("error while completing template", exc)
    }
    continuation(state.getVariableValue("name").getText, newAssignStmt.get)
  }

  override protected def cleanCanceledTemplate(state: TemplateState): Unit = {
    super.cleanCanceledTemplate(state)

    if (state.getVariableValue("name").getText.isEmpty) { // canceled by esc while name is empty
      writeCommandAction(project).withName(s"cancel $actionName").compute(() => {
        deleteTemplate()
      })
    }
  }
}

object LiveTemplateInsertBlock {

  /** Creates an action to start a live template to insert a block.
    */
  def createTemplateBlock(
      contextClass: PyClass,
      libClass: PyClass,
      actionName: String,
      continuation: (String, PsiElement) => Unit
  ): MovableLiveTemplate = {
    new MovableLiveTemplate(actionName) {
      override def createTemplate(): InsertionLiveTemplate = {
        new BlockInsertionLiveTemplate(contextClass, libClass, actionName, continuation)
      }
    }
  }
}
