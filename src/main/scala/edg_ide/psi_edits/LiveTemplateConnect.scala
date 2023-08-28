package edg_ide.psi_edits

import com.intellij.codeInsight.template.impl.TemplateState
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.project.Project
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiElement, PsiWhiteSpace}
import com.jetbrains.python.psi._
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.ExceptSeq
import edg_ide.util.{ConnectBuilder, DesignAnalysisUtils, PortConnects, exceptable}
import edgir.elem.elem

object LiveTemplateConnect {
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
    case PortConnects.BlockPort(blockName, portName) => Some(blockName)
    case PortConnects.BoundaryPort(portName, _) => Some(portName)
    case PortConnects.BlockVectorUnit(blockName, portName) => Some(blockName)
    case PortConnects.BlockVectorSlicePort(blockName, portName, _) => Some(portName)
    case PortConnects.BlockVectorSliceVector(blockName, portName, _) => Some(portName)
    case PortConnects.BlockVectorSlice(blockName, portName, _) => Some(blockName)
    case PortConnects.BoundaryPortVectorUnit(portName) => Some(portName)
  }

  // Creates an action to start a live template to insert the connection
  def createTemplateConnect(
      contextClass: PyClass,
      actionName: String,
      project: Project,
      baseConnected: ConnectBuilder,
      startingConnect: PortConnects.Base,
      newConnects: Seq[PortConnects.Base],
      continuation: (Option[String], PsiElement) => Unit,
  ): Errorable[() => Unit] = exceptable {
    val languageLevel = LanguageLevel.forElement(contextClass)
    val psiElementGenerator = PyElementGenerator.getInstance(project)

    val movableLiveTemplate = new MovableLiveTemplate(actionName) {
      // TODO startTemplate should be able to fail - Errorable
      override def startTemplate(caretEltOpt: Option[PsiElement]): InsertionLiveTemplate = {
        val validCaretEltOpt = caretEltOpt.flatMap(TemplateUtils.getInsertionStmt(_, contextClass))
        // TODO support append to existing connect
        val preInsertAfter = validCaretEltOpt
          .getOrElse(InsertAction.findInsertionElements(contextClass, InsertBlockAction.VALID_FUNCTION_NAMES).head)

        // adjust insertion position to be after all assignments to required references
        val allConnects = startingConnect +: newConnects
        val allRequiredAttrs = allConnects.flatMap(connectedToRequiredAttr)
        val earliestPosition = TemplateUtils.getLastAttributeAssignment(contextClass, allRequiredAttrs, project)
        val insertAfter = earliestPosition.map { earliestPosition =>
          if (!DesignAnalysisUtils.elementAfterEdg(preInsertAfter, earliestPosition, project).getOrElse(true)) {
            preInsertAfter
          } else {
            earliestPosition
          }
        }.getOrElse(preInsertAfter)

        val containingPsiFn = PsiTreeUtil.getParentOfType(insertAfter, classOf[PyFunction])
        val selfName = containingPsiFn.getParameterList.getParameters.toSeq
          .exceptEmpty(s"function ${containingPsiFn.getName} has no self")
          .head
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
          "name",
          newConnect.getTargets.head.asInstanceOf[PyTargetExpression],
          defaultValue = Some("")
        )

        new InsertionLiveTemplate(newConnect, IndexedSeq(nameTemplateVar))
      }
    }

    movableLiveTemplate.addTemplateStateListener(new TemplateFinishedListener {
      override def templateFinished(state: TemplateState, brokenOff: Boolean): Unit = {
        val expr = state.getExpressionContextForSegment(0)
        if (expr.getTemplateEndOffset <= expr.getTemplateStartOffset) {
          return // ignored if template was deleted, including through moving the template
        }

        val insertedName = state.getVariableValue("name").getText
        val insertedNameOpt = if (insertedName.isEmpty) None else Some(insertedName)
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

          if (insertedNameOpt.isEmpty) { // if name not specified, make the connect anonymous
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
