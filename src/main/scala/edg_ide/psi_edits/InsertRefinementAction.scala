package edg_ide.psi_edits

import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiParserFacade
import com.jetbrains.python.psi._
import edg.util.Errorable
import edg.wir.DesignPath
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptSeq}
import edg_ide.util.exceptable

import scala.collection.SeqMap


object InsertRefinementAction {
  val kRefinementsFunctionName = "refinements"
  val kKwargInstanceRefinements = "instance_refinements"  // path-based subclass refinement
  val kKwargInstanceValues = "instance_values"  // path-based value refinement
  val kKwargClassRefinements = "class_refinements"  // class-based subclass refinement (refine all classes of X)
  val kKwargClassValues = "class_values"  // class + subpath-based value refinement

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
  protected def findRefinementsExprByKey(list: PyListLiteralExpression,
                                         key: PyExpression): Option[PyExpression] = {
    list.getElements.filter {
      case elt: PyParenthesizedExpression =>
        elt.getContainedExpression match {
          case elt: PyTupleExpression =>
            elt.getElements.headOption match {
              case Some(head) => head.textMatches(key)
              case _ => false
            }
          case _ => false
        }
      case _ => false
    }.lastOption
  }

  /** Creates an action that inserts a refinement in the specified class as a key and value (as PSI exprs).
    * Validation is done before-hand, and will insert the refinements method if none exists.
    */
  protected def createInsertRefinement(kwargName: String, cls: PyClass, key: PyExpression, value: PyExpression,
                                       actionName: String, project: Project): Errorable[() => PyExpression] = exceptable {
    val psiElementGenerator = PyElementGenerator.getInstance(project)
    val languageLevel = LanguageLevel.forElement(cls)

    val refinementsMethod = cls.getMethods.find { method =>
      method.getName == kRefinementsFunctionName
    }

    // TODO: seriously dedup this across the create new / insert existing paths, perhaps breaking steps into functions
    refinementsMethod match {
      case Some(refinementsMethod) =>
        val argList = refinementsMethod.getStatementList.getStatements.toSeq
            .onlyExcept("unexpected multiple statements in refinements()")
            .instanceOfExcept[PyReturnStatement]("unexpected statement in refinements() body")
            .getExpression.instanceOfExcept[PyBinaryExpression]("unexpected expr in refinements() return")
            .getRightExpression.instanceOfExcept[PyCallExpression]("unexpected expr in refinements() return rhs")
            .getArgumentList

        Option(argList.getKeywordArgument(kwargName)) match {
          case Some(kwarg) =>  // potentially append to keyword arg
            val valueList = kwarg.getValueExpression.instanceOfExcept[PyListLiteralExpression](s"Refinements kwarg $kwarg not a list")
            findRefinementsExprByKey(valueList, key) match {
              case Some(parenExpr) => // replace existing
                val insertTuple = psiElementGenerator.createExpressionFromText(languageLevel,
                  s"(${key.getText}, ${value.getText})")  // note, no trailing comma
                () => {
                  writeCommandAction(project).withName(actionName).compute(() => {
                    parenExpr.replace(insertTuple)
                  }).asInstanceOf[PyExpression]
                }

              case None => // create new tuple
                val insertTuple = psiElementGenerator.createExpressionFromText(languageLevel,
                  s"(${key.getText}, ${value.getText}),")  // note, trailing comma
                () => {
                  // for some reason, PyElementGenerator.getInstance(project).createNewLine inserts two spaces
                  val newline = PsiParserFacade.SERVICE.getInstance(project).createWhiteSpaceFromText("\n")

                  val inserted = writeCommandAction(project).withName(actionName).compute(() => {
                    val inserted = valueList.add(insertTuple)
                    // can't do valueList.addBefore, since that does a check for PyExpr, which whitespace is not
                    inserted.addBefore(newline, inserted.getFirstChild)
                    inserted
                  }).asInstanceOf[PyExpression]
                  inserted
                }
            }
          case None =>  // create new keyword arg
            // TODO: dedup w/ create kwarg below
            val kwargExpr = psiElementGenerator.createKeywordArgument(languageLevel,
              kwargName,
              s"""[
                 |  (${key.getText}, ${value.getText}),
                 |]""".stripMargin.replace("\r\n", "\n")
            )

            () => {
              writeCommandAction(project).withName(actionName).compute(() => {
                argList.addArgument(kwargExpr)
              })
              argList.getArguments.head
            }
        }
      case None =>
        val kwargExpr = psiElementGenerator.createKeywordArgument(languageLevel,
          kwargName,
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

  def createInstanceRefinement(container: PyClass, path: DesignPath, refinementClass: PyClass,
                               project: Project): Errorable[() => PyExpression] = exceptable {
    val psiElementGenerator = PyElementGenerator.getInstance(project)
    val languageLevel = LanguageLevel.forElement(container)
    val keyExpr = psiElementGenerator.createExpressionFromText(languageLevel,
      s"""[${path.steps.map(step => s"'$step'").mkString(", ")}]""")
    val valueExpr = psiElementGenerator.createExpressionFromText(languageLevel,
      refinementClass.getName)

    createInsertRefinement(kKwargInstanceRefinements, container, keyExpr, valueExpr,
      s"Refine $path to ${refinementClass.getName}", project).exceptError
  }

  def createClassRefinement(container: PyClass, refinedClass: PyClass, refinementClass: PyClass,
                               project: Project): Errorable[() => PyExpression] = exceptable {
    val psiElementGenerator = PyElementGenerator.getInstance(project)
    val languageLevel = LanguageLevel.forElement(container)
    val keyExpr = psiElementGenerator.createExpressionFromText(languageLevel,
      refinedClass.getName)
    val valueExpr = psiElementGenerator.createExpressionFromText(languageLevel,
      refinementClass.getName)

    createInsertRefinement(kKwargClassRefinements, container, keyExpr, valueExpr,
      s"Refine ${refinedClass.getName} to ${refinementClass.getName}", project).exceptError
  }
}


class InsertRefinementAction(project: Project, insertIntoClass: PyClass) {
  val psiElementGenerator = PyElementGenerator.getInstance(project)
  val languageLevel = LanguageLevel.forElement(insertIntoClass)

  // Refinements specified as map of kwarg -> [([refinement key expr components], refinement value)]
  // for example, "instance_refinements" -> [([Expr(AbstractResistor)], Expr(GenericResistor))]
  // or, "class_values" -> [([Expr(AbstractResistor), Expr(["resistance"])], Expr(1))]
  // Returns a function that when called, does the insertion and return the newly inserted expressions
  // Refinements are inserted as one action
  // Inserts surrounding infrastructure as needed, handling cases where no refinements block or kwarg is present
  def createInsertRefinements(refinements: SeqMap[String, Seq[(Seq[PyExpression], PyExpression)]]): Errorable[() => Seq[PyExpression]] = {

  }
}
