package edg_ide.psi_edits

import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.ThrowableComputable
import com.intellij.psi.PsiParserFacade
import com.jetbrains.python.psi._
import edg.EdgirUtils.SimpleLibraryPath
import edg.compiler._
import edg.util.Errorable
import edg.wir.{DesignPath, Refinements}
import edg_ide.psi_edits.InsertRefinementAction.{kKwargClassRefinements, kKwargClassValues, kKwargInstanceRefinements, kKwargInstanceValues}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptSeq}
import edg_ide.util.exceptable
import edgir.ref.ref

import scala.collection.SeqMap


object InsertRefinementAction {
  val kRefinementsFunctionName = "refinements"
  val kKwargInstanceRefinements = "instance_refinements"  // path-based subclass refinement
  val kKwargInstanceValues = "instance_values"  // path-based value refinement
  val kKwargClassRefinements = "class_refinements"  // class-based subclass refinement (refine all classes of X)
  val kKwargClassValues = "class_values"  // class + subpath-based value refinement
}


class InsertRefinementAction(project: Project, insertIntoClass: PyClass) {
  val psiElementGenerator = PyElementGenerator.getInstance(project)
  val languageLevel = LanguageLevel.forElement(insertIntoClass)
  val newline = PsiParserFacade.SERVICE.getInstance(project).createWhiteSpaceFromText("\n")

  // Must be called within writeCommandAction
  // Inserts the refinement kwarg and value into the target PyArgumentList
  private def insertRefinementKwarg(into: PyArgumentList, kwargName: String,
                                    refinements: Seq[(Seq[PyExpression], PyExpression)]): Seq[PyElement] = {
    val refinementsText = refinements.map { case (keyElts, value) =>
      s"  (${keyElts.map(_.getText).mkString(", ")}, ${value.getText})"
    }
    val kwargExpr = psiElementGenerator.createKeywordArgument(languageLevel,
      kwargName,
      s"""[
         |${refinementsText.mkString("\n")}
         |]""".stripMargin.replace("\r\n", "\n")
    )
    into.addArgument(kwargExpr)
    Seq(into.getArguments.last)
  }

  // Given an argument list to Refinements(), the refinement type, and key, returns the expression if it exists.
  private def findRefinementsExprByKey(list: PyListLiteralExpression,
                                       key: Seq[PyExpression]): Option[PyTupleExpression] = {
    list.getElements.collect {
      case elt: PyParenthesizedExpression => elt.getContainedExpression match {
        case elt: PyTupleExpression if elt.getElements.length == key.length + 1 =>
          val eltMatches = (elt.getElements.init zip key).forall { case (eltElt, keyElt) => eltElt.textMatches(keyElt) }
          if (eltMatches) {
            Some(elt)
          } else {
            None
          }
        case _ => None
      }
    }.flatten.lastOption
  }

  // Creates a function that when called within a writeCommandAction,
  // merges the target refinements into a PyArgumentList
  private def createMergeRefinementKwarg(into: PyArgumentList, kwargName: String,
                                         refinements: Seq[(Seq[PyExpression], PyExpression)]): Errorable[() => Seq[PyElement]] = exceptable {
    Option(into.getKeywordArgument(kwargName)) match {
      case Some(kwarg) =>  // merge into list
        val valueList = kwarg.getValueExpression.instanceOfExcept[PyListLiteralExpression](s"Refinements kwarg $kwarg not a list")
        val eltActions = refinements.map { case (keyElts, value) =>
          findRefinementsExprByKey(valueList, keyElts) match {
            case Some(existingRefinementTuple) => () => {
              Seq(existingRefinementTuple.getElements.last.replace(value).asInstanceOf[PyExpression])
            }
            case None => () => {
              val insertTuple = psiElementGenerator.createExpressionFromText(languageLevel,
                s"(${keyElts.map(_.getText).mkString(", ")}, ${value.getText})")
              // for some reason, PyElementGenerator.getInstance(project).createNewLine inserts two spaces
              val inserted = valueList.add(insertTuple).asInstanceOf[PyExpression]
              // can't do valueList.addBefore, since that does a check for PyExpr, which whitespace is not
              inserted.addBefore(newline, inserted.getFirstChild)
              Seq(inserted)
            }
          }
        }
        () => {
          eltActions.flatMap(fn => fn())
        }
      case None =>  // kwarg doesn't exist, create a new one
        () => {
          insertRefinementKwarg(into, kwargName, refinements)
        }
    }
  }

  // Refinements specified as map of kwarg -> [([refinement key expr components], refinement value)]
  // for example, "instance_refinements" -> [([Expr(AbstractResistor)], Expr(GenericResistor))]
  // or, "class_values" -> [([Expr(AbstractResistor), Expr(["resistance"])], Expr(1))]
  // Returns a function that when called, does the insertion and return the newly inserted expressions
  // Refinements are inserted as one action
  // Inserts surrounding infrastructure as needed, handling cases where no refinements block or kwarg is present
  def createInsertRefinements(refinements: SeqMap[String, Seq[(Seq[PyExpression], PyExpression)]]):
      Errorable[() => Seq[PyElement]] = exceptable {
    val refinementsMethod = insertIntoClass.getMethods.find { method =>
      method.getName == InsertRefinementAction.kRefinementsFunctionName
    }
    val insertRefinementsAction: ThrowableComputable[Seq[PyElement], Nothing] = refinementsMethod match {
      case Some(refinementsMethod) =>  // append to existing refinements method
        val argList = refinementsMethod.getStatementList.getStatements.toSeq
          .onlyExcept("unexpected multiple statements in refinements()")
          .instanceOfExcept[PyReturnStatement]("unexpected statement in refinements() body")
          .getExpression.instanceOfExcept[PyBinaryExpression]("unexpected expr in refinements() return")
          .getRightExpression.instanceOfExcept[PyCallExpression]("unexpected expr in refinements() return rhs")
          .getArgumentList
        val mergeRefinements = refinements.toSeq.map { case (kwargName, refinements) =>
          createMergeRefinementKwarg(argList, kwargName, refinements).exceptError
        }
        () => {
          mergeRefinements.flatMap { fn => fn() }
        }
      case None =>  // insert new refinements method
        () => {
          val newFn = psiElementGenerator.createFromText(languageLevel,
            classOf[PyFunction],
            s"""def refinements(self) -> Refinements:
               |  return super().refinements() + Refinements(
               |    )
               |""".stripMargin.replace("\r\n", "\n")) // avoid a "wrong line separator" assert
          val refinementsFn = insertIntoClass.getStatementList.add(newFn).asInstanceOf[PyFunction]
          val argList = refinementsFn.getStatementList.getStatements
            .head.asInstanceOf[PyReturnStatement]
            .getExpression.asInstanceOf[PyBinaryExpression]
            .getRightExpression.asInstanceOf[PyCallExpression]
            .getArgumentList
          refinements.foreach { case (kwargName, refinements) =>
            insertRefinementKwarg(argList, kwargName, refinements)
          }
          Seq(refinementsFn)
        }
    }
    () => writeCommandAction(project)
      .withName(s"Add refinements to ${insertIntoClass.getName}")
      .compute(insertRefinementsAction)
  }

  def createInsertRefinements(refinements: Refinements): Errorable[() => Seq[PyElement]] = exceptable {
    val classRefinementsEntry = refinements.classRefinements.map { case (sourceClass, targetClass) =>
      Seq(refFromLibrary(sourceClass)) -> refFromLibrary(targetClass)
    }.toSeq
    val instanceRefinementsEntry = refinements.instanceRefinements.map { case (sourcePath, targetClass) =>
      Seq(keyFromPath(sourcePath)) -> refFromLibrary(targetClass)
    }.toSeq
    val classValuesEntry = refinements.classValues.map { case ((sourceClass, targetPath), targetValue) =>
      Seq(refFromLibrary(sourceClass), keyFromLocalPath(targetPath).exceptError) -> valueFromExpr(targetValue).exceptError
    }.toSeq
    val instanceValuesEntry = refinements.instanceValues.map { case (sourcePath, targetValue) =>
      Seq(keyFromPath(sourcePath)) -> valueFromExpr(targetValue).exceptError
    }.toSeq
    createInsertRefinements(SeqMap.from(Seq(
      if (classRefinementsEntry.isEmpty) Seq() else Seq(kKwargClassRefinements -> classRefinementsEntry),
      if (instanceRefinementsEntry.isEmpty) Seq() else Seq(kKwargInstanceRefinements -> instanceRefinementsEntry),
      if (classValuesEntry.isEmpty) Seq() else Seq(kKwargClassValues -> classValuesEntry),
      if (instanceValuesEntry.isEmpty) Seq() else Seq(kKwargInstanceValues -> instanceValuesEntry),
    ).flatten)).exceptError
  }

  // Utility functions for generating refinement exprs
  //
  def keyFromPath(path: DesignPath): PyExpression = psiElementGenerator.createExpressionFromText(languageLevel,
    s"""[${path.steps.map(step => s"'$step'").mkString(", ")}]""")

  def keyFromLocalPath(path: ref.LocalPath): Errorable[PyExpression] = exceptable {
    val listExpr = psiElementGenerator.createListLiteral()
    path.steps.foreach { step => step.step match {
      case ref.LocalStep.Step.Name(name) => listExpr.add(psiElementGenerator.createStringLiteralFromString(name))
      case _ => exceptable.fail(f"path ${ExprToString(path)} not compatible with refinement")
    } }
    listExpr
  }

  def valueFromExpr(expr: ExprValue): Errorable[PyExpression] = exceptable { expr match {
    case FloatValue(value) => psiElementGenerator.createExpressionFromText(languageLevel, value.toString)
    case IntValue(value) => psiElementGenerator.createExpressionFromText(languageLevel, value.toString)
    case RangeValue(lower, upper) => psiElementGenerator.createExpressionFromText(languageLevel,
      f"Range($lower, $upper)")
    case RangeEmpty => exceptable.fail("can't create empty range")
    case BooleanValue(value) => psiElementGenerator.createExpressionFromText(languageLevel,
      if (value) "True" else "False")
    case TextValue(value) => psiElementGenerator.createStringLiteralFromString(value)
    case ArrayValue(values) =>
      val listExpr = psiElementGenerator.createListLiteral()
      values.foreach { value =>
        val inserted = listExpr.add(valueFromExpr(value).exceptError)
        inserted.addBefore(newline, inserted.getFirstChild)
      }
      listExpr
  }}

  def refFromClass(cls: PyClass): PyExpression = psiElementGenerator.createExpressionFromText(languageLevel,
    cls.getName)

  // TODO: instead resolve to PyClass and create a PyReferenceExpression from that
  def refFromLibrary(cls: ref.LibraryPath): PyExpression = psiElementGenerator.createExpressionFromText(languageLevel,
    cls.toSimpleString)

}
