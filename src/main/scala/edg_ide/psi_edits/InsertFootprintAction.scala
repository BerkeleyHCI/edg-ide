package edg_ide.psi_edits

import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.{LanguageLevel, PyCallExpression, PyClass, PyElementGenerator, PyExpression, PyFunction, PyStatement, PyStatementList}
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption, ExceptSeq}
import edg_ide.util.{ExceptionNotifyException, exceptable, requireExcept}


object InsertFootprintAction {
  protected def insertFootprint(after: PsiElement, footprint: String, selfName: String, actionName: String,
                                project: Project): Errorable[() => PsiElement] = exceptable {
    val psiElementGenerator = PyElementGenerator.getInstance(project)
    val languageLevel = LanguageLevel.forElement(after)

    val newCall = psiElementGenerator.createFromText(languageLevel,
      classOf[PyStatement],
      s"""${selfName}.footprint(
         |  'U', '${footprint}',
         |  { },
         |  mfr='[Manufacturer]', part='[Part]',
         |  datasheet='[Datasheet URL]'
         |)
         |""".stripMargin.replace("\r\n", "\n"))  // avoid a "wrong line separator" assert
        .exceptNull("couldn't create footprint statement")

    () => writeCommandAction(project).withName(actionName).compute(() => {
      after.getParent.addAfter(newCall, after)
    })
  }

  protected def createModifyFootprint(call: PyCallExpression, footprint: String, actionName: String,
                                      project: Project): Errorable[() => PsiElement] = exceptable {
    val argExpr = call.getArgument(1, "footprint", classOf[PyExpression])
        .exceptNull("call has no footprint argument")

    val psiElementGenerator = PyElementGenerator.getInstance(project)
    val newExpr = psiElementGenerator.createStringLiteralFromString(footprint)
        .exceptNull("couldn't create footprint string")

    () => writeCommandAction(project).withName(actionName).compute(() => {
      argExpr.replace(newExpr)
    })
  }

  /** Creates a function that inserts or modifies a self.Footprint() call.
    * As much pre-checking as possible should be done here. */
  def createInsertFootprintFlow(container: PyClass, footprint: String,
                                project: Project,
                                continuation: PsiElement => Unit): Errorable[() => Unit] = exceptable {
    // Check for general sanity
    val after = InsertAction.getCaretAtFileOfType(
      container.getContainingFile, classOf[PyStatementList], project, requireClass = false).exceptError
    val containingPsiFunction = PsiTreeUtil.getParentOfType(after, classOf[PyFunction])
        .exceptNull(s"not in a function in ${after.getContainingFile.getName}")
    val containingPsiClass = PsiTreeUtil.getParentOfType(containingPsiFunction, classOf[PyClass])
        .exceptNull(s"not in a class in ${containingPsiFunction.getContainingFile.getName}")
    requireExcept(containingPsiClass == container, s"not in ${container.getName}")

    val selfName = containingPsiFunction.getParameterList.getParameters.toSeq
        .headOption.exceptNone(s"function ${containingPsiFunction.getName} has no self")
        .getName

    // if in a call to self.Footprint, modify that
    val modifyAction = exceptable {
      val containingCall = PsiTreeUtil.getParentOfType(after, classOf[PyCallExpression])
          .exceptNull(s"not in a function call")
      requireExcept(containingCall.getCallee.textMatches(s"$selfName.footprint"), "not in a self.footprint call")
      val modifyAction = createModifyFootprint(containingCall, footprint,
        s"Modify footprint to $footprint for ${container.getName}",
        project).exceptError

      () => continuation(modifyAction())
    }

    // otherwise, see if we can insert it at the caret
    val insertAction = exceptable {
      after.getParent
          .instanceOfExcept[PyStatementList](s"invalid position for insertion in ${after.getContainingFile.getName}")
      val insertAction = insertFootprint(after, footprint, selfName,
        s"Insert footprint $footprint at ${container.getName}.${containingPsiFunction.getName}",
        project).exceptError

      () => continuation(insertAction())
    }

    (modifyAction, insertAction) match {
      case (Errorable.Success(modifyAction), _) => modifyAction
      case (Errorable.Error(_), Errorable.Success(insertAction)) => insertAction
      case (Errorable.Error(modifyErr), Errorable.Error(insertErr)) =>
        throw ExceptionNotifyException(s"$modifyErr and $insertErr")
    }
  }
}
