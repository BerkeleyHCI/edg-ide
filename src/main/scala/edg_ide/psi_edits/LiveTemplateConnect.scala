package edg_ide.psi_edits

import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.project.Project
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiElement, PsiWhiteSpace}
import com.jetbrains.python.psi._
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.{ExceptOption, ExceptSeq}
import edg_ide.util.{DesignAnalysisUtils, PortConnects, exceptable}

object LiveTemplateConnect {
  protected val kTemplateVariableName = "name (optional)"

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
    case PortConnects.BlockVectorSlicePort(blockName, portName, _) => Some(portName)
    case PortConnects.BlockVectorSliceVector(blockName, portName, _) => Some(portName)
    case PortConnects.BlockVectorSlice(blockName, portName, _) => Some(blockName.takeWhile(_ != '['))
    case PortConnects.BoundaryPortVectorUnit(portName) => Some(portName)
  }

  // Creates an action to start a live template to insert the connection
  def createTemplateConnect(
      contextClass: PyClass,
      actionName: String,
      project: Project,
      startingConnect: PortConnects.Base,
      newConnects: Seq[PortConnects.Base],
      continuation: (Option[String], PsiElement) => Unit,
  ): Errorable[() => Unit] = exceptable {
    val languageLevel = LanguageLevel.forElement(contextClass)
    val psiElementGenerator = PyElementGenerator.getInstance(project)
    val allConnects = startingConnect +: newConnects

    val movableLiveTemplate = new MovableLiveTemplate(actionName) {
      // starts the live template insertion as a statement
      protected def startStatementInsertionTemplate(
          insertAfter: PsiElement
      ): InsertionLiveTemplate = {
        val containingPsiFn = PsiTreeUtil.getParentOfType(insertAfter, classOf[PyFunction])
        val selfName = containingPsiFn.getParameterList.getParameters.toSeq
          .headOption.exceptNone(s"function ${containingPsiFn.getName} has no self")
          .getName
        val containingStmtList = PsiTreeUtil.getParentOfType(insertAfter, classOf[PyStatementList])

        // TODO don't allow name when appending to a named connect (?)
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

        new InsertionLiveTemplate(newConnect, IndexedSeq(nameTemplateVar))
      }

      // if caretElt is in a PyCallExpression that is a connect involving any of the ports in this connection,
      // return the InsertionLiveTemplate, otherwise None
      protected def tryStartStatementInsertionTemplate(
          caretElt: PsiElement,
          earliestPosition: Option[PsiElement]
      ): Option[InsertionLiveTemplate] = {
        val callCandidate = PsiTreeUtil.getParentOfType(caretElt, classOf[PyCallExpression])
        if (callCandidate == null) return None

        val containingPsiFn = PsiTreeUtil.getParentOfType(caretElt, classOf[PyFunction])
        val selfName = containingPsiFn.getParameterList.getParameters.toSeq
          .headOption.getOrElse(return None)
          .getName
        val connectReference = psiElementGenerator.createExpressionFromText(languageLevel, s"$selfName.connect")
        if (!callCandidate.getCallee.textMatches(connectReference)) return None

        val isAfterEarliest = earliestPosition.flatMap( // aka is in valid position
          DesignAnalysisUtils.elementAfterEdg(_, callCandidate, project)).getOrElse(true)
        if (!isAfterEarliest) return None

        val matchingConnects = callCandidate.getArgumentList.getArguments.flatMap(arg =>
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
        val newArgs = connectsToAdd.map { newConnect =>
          callCandidate.getArgumentList.addArgument(psiElementGenerator.createExpressionFromText(
            languageLevel,
            connectedToRefCode(selfName, newConnect)
          ))
        }

        Some(new InsertionLiveTemplate(callCandidate, IndexedSeq()))
      }

      // TODO startTemplate should be able to fail - Errorable
      override def startTemplate(caretEltOpt: Option[PsiElement]): InsertionLiveTemplate = {
        // find earliest insertion position (after all refs are defined)
        val allRequiredAttrs = allConnects.flatMap(connectedToRequiredAttr)
        val earliestPosition = TemplateUtils.getLastAttributeAssignment(contextClass, allRequiredAttrs, project)

        caretEltOpt.foreach { caretElt => // check if caret is in a connect
          tryStartStatementInsertionTemplate(caretElt, earliestPosition).foreach {
            return _
          }
        } // otherwise continue to stmt insertion

        val validCaretEltOpt = caretEltOpt.flatMap(TemplateUtils.getInsertionStmt(_, contextClass))
        val preInsertAfter = validCaretEltOpt
          .getOrElse(InsertAction.findInsertionElements(contextClass, InsertBlockAction.VALID_FUNCTION_NAMES).head)

        // adjust insertion position to be after all assignments to required references
        val insertAfter = earliestPosition.map { earliestPosition =>
          if (!DesignAnalysisUtils.elementAfterEdg(preInsertAfter, earliestPosition, project).getOrElse(true)) {
            preInsertAfter
          } else {
            earliestPosition
          }
        }.getOrElse(preInsertAfter)
        startStatementInsertionTemplate(insertAfter)
      }
    }

    movableLiveTemplate.addTemplateStateListener(new TemplateFinishedListener {
      override def templateFinished(state: TemplateState, brokenOff: Boolean): Unit = {
        val expr = state.getExpressionContextForSegment(0)
        if (expr.getTemplateEndOffset <= expr.getTemplateStartOffset) {
          return // ignored if template was deleted, including through moving the template
        }

        val nameVar = Option(state.getVariableValue(kTemplateVariableName))
        val insertedNameOpt = nameVar.map(_.getText).filter(_.nonEmpty)
        if (insertedNameOpt.isEmpty && brokenOff) { // canceled by esc while name is empty
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

          if (nameVar.nonEmpty && insertedNameOpt.isEmpty) { // if name not specified, make the connect anonymous
            try {
              val templateAssign = templateElem.asInstanceOf[PyAssignmentStatement]
              writeCommandAction(project)
                .withName(s"clean $actionName")
                .compute(() => {
                  templateElem.replace(templateAssign.getAssignedValue)
                })
            } catch {
              case _: Throwable => // ignore
            }
          }

          continuation(insertedNameOpt, templateElem)
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
