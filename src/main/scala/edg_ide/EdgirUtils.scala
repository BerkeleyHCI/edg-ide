package edg_ide

import edg.expr.expr
import edg.ref.ref


object EdgirUtils {
  def StringToLibraryPath(path: String): ref.LibraryPath = {
    ref.LibraryPath(target=Some(ref.LocalStep(step=ref.LocalStep.Step.Name(path))))
  }

  def LibraryPathToString(path: ref.LibraryPath): String = {
    // TODO handle LibraryName and Namespace
    path.target match {
      case Some(target) => target.step match {
        case ref.LocalStep.Step.Name(step) => step
        case ref.LocalStep.Step.ReservedParam(step) => step.toString
        case ref.LocalStep.Step.Empty => "(empty LocalStep)"
      }
      case None => "(LibraryPath with no target)"
    }
  }

  def SimpleLibraryPathToString(path: ref.LibraryPath): String = {
    // TODO once namespaces are handled properly, this should use that instead of string ops
    LibraryPathToString(path).split('.').last
  }

  /**
    * Converts a ValueExpr containing a LocalPath ref to a Seq[String] of the ref's components
    * Errors out with an exception if things aren't just right
    * TODO: cleaner error handling?
    */
  def RefExprToSeqString(valueExpr: expr.ValueExpr): Seq[String] = {
    valueExpr.expr match {
      case expr.ValueExpr.Expr.Ref(refExpr) => refExpr.steps.map { step =>
        step.step match {
          case ref.LocalStep.Step.ReservedParam(reservedParam) =>
            throw new Exception(s"Expected path type without reserved_param $reservedParam for ValueExpr $valueExpr")
          case ref.LocalStep.Step.Name(name) => name
        }
      }
      case _ => throw new Exception(s"Expected path type for ValueExpr $valueExpr")
    }
  }

  /**
    * Converts a Seq[String] to a LocalPath ValueExpr, where each element in the input seq is
    * treated as a discrete LocalStep
    */
  def SeqStringToRefExpr(path: Seq[String]): expr.ValueExpr = {
    expr.ValueExpr(
      expr=expr.ValueExpr.Expr.Ref(ref.LocalPath(
        steps=path.map { pathElt =>
          ref.LocalStep(step=ref.LocalStep.Step.Name(pathElt))
        }
      ))
    )
  }

}
