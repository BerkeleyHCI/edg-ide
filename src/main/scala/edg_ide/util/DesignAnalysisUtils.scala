package edg_ide.util

import com.intellij.openapi.project.Project
import com.jetbrains.python.psi.{PyAssignmentStatement, PyClass, PyPsiFacade}
import edg.ref.ref
import edg.schema.schema
import edg.util.Errorable
import edg.wir.DesignPath
import edg_ide.{EdgirUtils, PsiUtils}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptOption}


object DesignAnalysisUtils {
  def pyClassOf(path: ref.LibraryPath, project: Project): Errorable[PyClass] = {
    val pyPsi = PyPsiFacade.getInstance(project)
    Errorable(pyPsi.findClass(path.getTarget.getName), "no class")
  }

  def allAssignsTo(path: DesignPath, topDesign: schema.Design,
                   project: Project): Errorable[Seq[PyAssignmentStatement]] = exceptable {
    requireExcept(path.steps.nonEmpty, "node at top")
    val (parentPath, blockName) = path.split
    val parentBlock = EdgirUtils.resolveBlockFromBlock(parentPath, topDesign.getContents)
        .exceptNone(s"no block at parent path $parentPath")
    requireExcept(parentBlock.superclasses.length == 1,
      s"invalid parent class ${EdgirUtils.SimpleSuperclass(parentBlock.superclasses)}")
    val parentPyClass = pyClassOf(parentBlock.superclasses.head, project).exceptError
    val assigns = PsiUtils.findAssignmentsTo(parentPyClass, blockName, project).filter(_.canNavigateToSource)
    requireExcept(assigns.nonEmpty, s"no assigns to $blockName found in ${parentPyClass.getName}")
    assigns
  }
}
