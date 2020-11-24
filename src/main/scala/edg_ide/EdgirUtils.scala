package edg_ide

import edg.ref.ref.{LibraryPath, LocalStep}


object EdgirUtils {
  def LibraryPathToString(path: LibraryPath): String = {
    // TODO handle LibraryName and Namespace
    path.target match {
      case Some(target) => target.step match {
        case LocalStep.Step.Name(step) => step
        case LocalStep.Step.ReservedParam(step) => step.toString
        case LocalStep.Step.Empty => "(empty LocalStep)"
      }
      case None => "(LibraryPath with no target)"
    }
  }
}
