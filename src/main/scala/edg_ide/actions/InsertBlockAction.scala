package edg_ide.actions

import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.{LanguageLevel, PyAssignmentStatement, PyClass, PyElementGenerator, PyFunction, PyStatementList}
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.{ExceptNotify, ExceptSeq}
import edg_ide.util.exceptable


object InsertBlockAction {
  val VALID_FUNCTION_NAMES = Set("__init__", "contents")  // TODO support generators
  val VALID_SUPERCLASS = "edg_core.HierarchyBlock.Block"

  /** Creates an action to insert a block of type libClass after some PSI element after.
    * Validation is performed before the action is generated, though the action itself may also return an error.
    */
  def createInsertBlockFlow(after: PsiElement, libClass: PyClass, actionName: String,
                            project: Project,
                            continuation: (String, PsiElement) => Unit): Errorable[() => Unit] = exceptable {
    val containingPsiList = after.getParent
        .instanceOfExcept[PyStatementList](s"invalid position for insertion")
    val containingPsiFunction = containingPsiList.getParent
        .instanceOfExcept[PyFunction]("not in a function")
    val containingPsiClass = PsiTreeUtil.getParentOfType(containingPsiList, classOf[PyClass])
        .exceptNull("not in a class")

    val psiElementGenerator = PyElementGenerator.getInstance(project)
    val selfName = containingPsiFunction.getParameterList.getParameters.toSeq
        .exceptEmpty(s"function ${containingPsiFunction.getName} has no self")
        .head.getName

    def insertBlockFlow: Unit = {
      InsertAction.createNameEntryPopup("Block Name", containingPsiClass, project) { name => exceptable {
        val newAssign = psiElementGenerator.createFromText(LanguageLevel.forElement(after),
          classOf[PyAssignmentStatement], s"$selfName.$name = $selfName.Block(${libClass.getName}())")

        val added = writeCommandAction(project).withName(actionName).compute(() => {
          containingPsiList.addAfter(newAssign, after)
        })
        continuation(name, added)
      }}
    }
    () => insertBlockFlow
  }
}
