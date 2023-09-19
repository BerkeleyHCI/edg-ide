package edg_ide.psi_edits

import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Editor
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi._
import edg.util.Errorable
import edg_ide.util.{DesignAnalysisUtils, PortConnects, exceptable}

import scala.collection.mutable

class ConnectInsertionLiveTemplate(
    contextClass: PyClass,
    allConnects: Seq[PortConnects.Base],
    actionName: String,
    continuation: (Option[String], PsiElement) => Unit
) extends InsertionLiveTemplate(contextClass.getContainingFile) {
  protected val kTemplateVariableName = "name (optional)"

  private lazy val logger = Logger.getInstance(this.getClass)

  protected val project = contextClass.getProject
  protected val languageLevel = LanguageLevel.forElement(contextClass)
  protected val psiElementGenerator = PyElementGenerator.getInstance(project)

  protected var createdPsiElts = mutable.ArrayBuffer[PsiElement]()
  protected var newAssignStmt: Option[PyAssignmentStatement] = None // only if an assign was inserted

  // generate the reference Python HDL code for a PortConnect
  protected def connectedToRefCode(selfName: String, connect: PortConnects.Base): String = connect match {
    case PortConnects.BlockPort(blockName, portName) => s"$selfName.$blockName.$portName"
    case PortConnects.BoundaryPort(portName, _) => s"$selfName.$portName"
    case PortConnects.BlockVectorUnit(blockName, portName) => s"$selfName.$blockName.$portName"
    case PortConnects.BlockVectorSlicePort(blockName, portName, suggestedIndex) => suggestedIndex match {
        case Some(suggestedIndex) => s"$selfName.$blockName.$portName.request('$suggestedIndex')"
        case None => s"$selfName.$blockName.$portName.request()"
      }
    case PortConnects.BlockVectorSliceVector(blockName, portName, suggestedIndex) => suggestedIndex match {
        case Some(suggestedIndex) => s"$selfName.$blockName.$portName.request_vector('$suggestedIndex')"
        case None => s"$selfName.$blockName.$portName.request_vector()"
      }
    case PortConnects.BlockVectorSlice(blockName, portName, suggestedIndex) =>
      throw new IllegalArgumentException()
    case PortConnects.BoundaryPortVectorUnit(portName) => s"$selfName.$portName"
  }

  // gets the class member variable (if any) for a PortConnect
  protected def connectedToRequiredAttr(connect: PortConnects.Base): Option[String] = connect match {
    // for blocks that are part of an ElementDict (by heuristic), only take the non-index part
    case PortConnects.BlockPort(blockName, portName) => Some(blockName.takeWhile(_ != '['))
    case PortConnects.BoundaryPort(portName, _) => Some(portName)
    case PortConnects.BlockVectorUnit(blockName, portName) => Some(blockName.takeWhile(_ != '['))
    case PortConnects.BlockVectorSlicePort(blockName, portName, _) => Some(blockName.takeWhile(_ != '['))
    case PortConnects.BlockVectorSliceVector(blockName, portName, _) => Some(blockName.takeWhile(_ != '['))
    case PortConnects.BlockVectorSlice(blockName, portName, _) => Some(blockName.takeWhile(_ != '['))
    case PortConnects.BoundaryPortVectorUnit(portName) => Some(portName)
  }

  // starts the live template insertion as a statement
  protected def startStatementInsertionTemplate(
      insertAfter: PsiElement
  ): Errorable[(PsiElement, Seq[PsiElement], Seq[InsertionLiveTemplateVariable])] = exceptable {
    val containingPsiFn = PsiTreeUtil.getParentOfType(insertAfter, classOf[PyFunction])
    val selfName = containingPsiFn.getParameterList.getParameters.toSeq
      .head.getName
    val containingStmtList = PsiTreeUtil.getParentOfType(insertAfter, classOf[PyStatementList])

    // TODO don't allow name when appending to a named connect (?)
    val newConnectTemplate = psiElementGenerator.createFromText(
      languageLevel,
      classOf[PyAssignmentStatement],
      s"$selfName.name = $selfName.connect()"
    )
    val newConnect =
      containingStmtList.addAfter(newConnectTemplate, insertAfter).asInstanceOf[PyAssignmentStatement]
    newAssignStmt = Some(newConnect)
    createdPsiElts += newConnect

    val newConnectArgs = newConnect.getAssignedValue
      .asInstanceOf[PyCallExpression]
      .getArgumentList

    allConnects.foreach { newConnect =>
      newConnectArgs.addArgument(psiElementGenerator.createExpressionFromText(
        languageLevel,
        connectedToRefCode(selfName, newConnect)
      ))
    }

    val nameTemplateVar = new InsertionLiveTemplate.Reference(
      kTemplateVariableName,
      newConnect.getTargets.head.asInstanceOf[PyTargetExpression],
      defaultValue = Some("")
    )

    (newConnect, Seq(), Seq(nameTemplateVar))
  }

  // if caretElt is in a PyCallExpression that is a connect involving any of the ports in this connection,
  // return the InsertionLiveTemplate, otherwise None
  protected def tryStartAppendTemplate(
      caretElt: PsiElement,
      earliestPosition: Option[PsiElement]
  ): Option[(PsiElement, Seq[PsiElement], Seq[InsertionLiveTemplateVariable])] = {
    val callExpr = Option(PsiTreeUtil.getParentOfType(caretElt, classOf[PyCallExpression]))
      .orElse(Option(PsiTreeUtil.getParentOfType(caretElt, classOf[PyAssignmentStatement])).map(_.getAssignedValue))
      .getOrElse(caretElt) match {
      case caretElt: PyCallExpression => caretElt
      case _ => return None
    }

    val containingPsiFn = PsiTreeUtil.getParentOfType(caretElt, classOf[PyFunction])
    val selfName = containingPsiFn.getParameterList.getParameters.toSeq
      .headOption.getOrElse(return None)
      .getName
    val connectReference = psiElementGenerator.createExpressionFromText(languageLevel, s"$selfName.connect")
    if (!callExpr.getCallee.textMatches(connectReference)) return None

    val isAfterEarliest = earliestPosition.flatMap( // aka is in valid position
      DesignAnalysisUtils.elementAfterEdg(_, callExpr, project)).getOrElse(true)
    if (!isAfterEarliest) return None

    val matchingConnects = callExpr.getArgumentList.getArguments.flatMap(arg =>
      allConnects.flatMap { connect =>
        if (arg.textMatches(connectedToRefCode(selfName, connect))) {
          Some(connect)
        } else {
          None
        }
      }
    )
    if (matchingConnects.isEmpty) return None

    // validation complete, start the template
    val connectsToAdd = allConnects.filter(!matchingConnects.contains(_))
    connectsToAdd.foreach { newConnect =>
      callExpr.getArgumentList.addArgument(psiElementGenerator.createExpressionFromText(
        languageLevel,
        connectedToRefCode(selfName, newConnect)
      )) // unfortunately doesn't return added
      createdPsiElts += callExpr.getArgumentList.getArguments.last
    }

    Some((callExpr.getArgumentList, createdPsiElts.toSeq, Seq()))
  }

  override protected def buildTemplateAst(editor: Editor)
      : Errorable[(PsiElement, Seq[PsiElement], Seq[InsertionLiveTemplateVariable])] = {
    // find earliest insertion position (after all refs are defined)
    val allRequiredAttrs = allConnects.flatMap(connectedToRequiredAttr)
    val earliestPosition = TemplateUtils.getLastAttributeAssignment(contextClass, allRequiredAttrs, project)

    val caretEltOpt = Option(contextClass.getContainingFile.findElementAt(editor.getCaretModel.getOffset))

    caretEltOpt.foreach { caretElt => // check if caret is in a connect
      tryStartAppendTemplate(caretElt, earliestPosition).foreach { rtn =>
        return Errorable.Success(rtn)
      }
    } // otherwise continue to stmt insertion

    val insertAfter = caretEltOpt
      .flatMap(TemplateUtils.getInsertionStmt(_, contextClass))
      .getOrElse(InsertAction.findInsertionElements(
        contextClass,
        InsertAction.kValidFunctionNames
      ).head)

    // adjust insertion position to be after all assignments to required references
    val validInsertAfter = earliestPosition.map { earliestPosition =>
      if (!DesignAnalysisUtils.elementAfterEdg(insertAfter, earliestPosition, project).getOrElse(true)) {
        insertAfter
      } else {
        earliestPosition
      }
    }.getOrElse(insertAfter)

    startStatementInsertionTemplate(validInsertAfter)
  }

  override def deleteTemplate(): Unit = {
    createdPsiElts.foreach { createdPsiElt =>
      createdPsiElt.delete()
    }
    createdPsiElts.clear()
    newAssignStmt = None
  }

  override protected def cleanCompletedTemplate(state: TemplateState): Unit = {
    super.cleanCompletedTemplate(state)

    // getExpressionContextForSegment seems needed to update the PSI element, otherwise .getAssignedValue returns null
    state.getExpressionContextForSegment(0).getPsiElementAtStartOffset

    val nameOpt = Option(state.getVariableValue(kTemplateVariableName)).map(_.getText).filter(_.nonEmpty)
    if (nameOpt.isEmpty && newAssignStmt.nonEmpty) { // if name not specified, make the connect anonymous
      try {
        val templateAssign = newAssignStmt.get
        val replacement = psiElementGenerator.createFromText( // must be wrapped in PyExprStatement
          languageLevel,
          classOf[PyStatement],
          templateAssign.getAssignedValue.getText
        )
        writeCommandAction(project)
          .withName(s"clean $actionName")
          .compute(() => {
            templateAssign.replace(replacement)
          })
      } catch {
        case exc: Throwable => logger.error("error while completing template", exc)
      }
    }

    continuation(nameOpt, createdPsiElts.head)
  }

  override protected def cleanCanceledTemplate(state: TemplateState): Unit = {
    super.cleanCanceledTemplate(state)

    val nameOpt = Option(state.getVariableValue(kTemplateVariableName)).map(_.getText).filter(_.nonEmpty)
    if (nameOpt.isEmpty) { // canceled by esc while name is empty
      writeCommandAction(project).withName(s"cancel $actionName").compute(() => {
        deleteTemplate()
      })
    }
  }
}

object LiveTemplateConnect {
  // Creates an action to start a live template to insert the connection
  def createTemplateConnect(
      contextClass: PyClass,
      allConnects: Seq[PortConnects.Base],
      actionName: String,
      continuation: (Option[String], PsiElement) => Unit,
  ): MovableLiveTemplate = {
    new MovableLiveTemplate(actionName) {
      override def createTemplate(): InsertionLiveTemplate = {
        new ConnectInsertionLiveTemplate(contextClass, allConnects, actionName, continuation)
      }
    }
  }
}
