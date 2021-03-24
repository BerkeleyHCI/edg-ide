package edg_ide.actions

import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.project.Project
import com.jetbrains.python.psi.{LanguageLevel, PyArgumentList, PyAssignmentStatement, PyBinaryExpression, PyCallExpression, PyClass, PyElementGenerator, PyExpression, PyFunction, PyListLiteralExpression, PyParenthesizedExpression, PyPsiFacade, PyReturnStatement}
import edg.wir.DesignPath
import edg.ref.ref
import edg.util.Errorable
import edg_ide.EdgirUtils
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption, ExceptSeq}
import edg_ide.util.{DesignAnalysisUtils, exceptable}


object InsertRefinementAction {
  val REFINEMENT_FN_NAME = "refinements"

  // Inserts the refinement method into the specified class.
  // Validation (that the function doesn't exist before, and the class has appropriate superclasses)
  // should be done prior.
  protected def insertRefinementsMethod(cls: PyClass, project: Project): PyFunction = {
    val psiElementGenerator = PyElementGenerator.getInstance(project)
    val languageLevel = LanguageLevel.forElement(cls)
    val newMethod = psiElementGenerator.createFromText(languageLevel,
      classOf[PyFunction],
      s"""def refinements(self) -> Refinements:
         |  return super().refinements() + Refinements(
         |    )
         |""".stripMargin.replace("\r\n", "\n"))  // avoid a "wrong line separator" assert
    writeCommandAction(project).withName(s"Add refinements method to ${cls.getName}").compute(() => {
      cls.getStatementList.add(newMethod).asInstanceOf[PyFunction]
    })
  }

  // Given an argument list to Refinements(), the refinement type, and key, returns the expression if it exists.
  protected def findRefinementsExprByKey(args: PyArgumentList, kwarg: String,
                                         key: PyExpression): Option[PyParenthesizedExpression] = {
    ???
  }

  /** Creates an action that inserts a refinement in the specified class as a key and value (as PSI exprs).
    * Validation is done before-hand, and will insert the refinements method if none exists.
    */
  protected def createInsertRefinement(kwarg: String, cls: PyClass, key: PyExpression, value: PyExpression,
                                       actionName: String, project: Project): Errorable[() => PyExpression] = exceptable {
    val psiElementGenerator = PyElementGenerator.getInstance(project)
    val languageLevel = LanguageLevel.forElement(cls)

    val refinementsMethod = cls.getMethods.find { method =>
      method.getName == REFINEMENT_FN_NAME
    }

    refinementsMethod match {
      case Some(refinementsMethod) =>
        val argList = refinementsMethod.getStatementList.getStatements.toSeq
            .onlyExcept("unexpected multiple statements in refinements()")
            .instanceOfExcept[PyReturnStatement]("unexpected statement in refinements() body")
            .getExpression.instanceOfExcept[PyBinaryExpression]("unexpected expr in refinements() return")
            .getRightExpression.instanceOfExcept[PyCallExpression]("unexpected expr in refinements() return rhs")
            .getArgumentList

        () => {
          ???
        }
      case None =>
        val kwargExpr = psiElementGenerator.createKeywordArgument(languageLevel,
          kwarg,
          s"""[
             |  (${key.getText}, ${value.getText}),
             |]""".stripMargin.replace("\r\n", "\n")
        )

        () => {
          val newRefinements = insertRefinementsMethod(cls, project)
          val argList = newRefinements.getStatementList.getStatements
              .head.asInstanceOf[PyReturnStatement]
              .getExpression.asInstanceOf[PyBinaryExpression]
              .getRightExpression.asInstanceOf[PyCallExpression]
              .getArgumentList
          writeCommandAction(project).withName(actionName).compute(() => {
            argList.addArgument(kwargExpr)
          })
          argList.getArguments.head
        }
    }
  }

  def createInstanceRefinement(cls: PyClass, path: DesignPath, refinementType: ref.LibraryPath,
                               project: Project): Errorable[() => PyExpression] = exceptable {
    val psiElementGenerator = PyElementGenerator.getInstance(project)
    val languageLevel = LanguageLevel.forElement(cls)
    val keyExpr = psiElementGenerator.createExpressionFromText(languageLevel,
      s"""[${path.steps.map(step => s"'$step'").mkString(", ")}]""")
    val valueClass = DesignAnalysisUtils.pyClassOf(refinementType, project).exceptError
    val valueExpr = psiElementGenerator.createExpressionFromText(languageLevel,
      valueClass.getName)

    createInsertRefinement("instance_refinements", cls, keyExpr, valueExpr,
      s"Refine $path to ${EdgirUtils.SimpleLibraryPath(refinementType)}", project).exceptError
  }
}
