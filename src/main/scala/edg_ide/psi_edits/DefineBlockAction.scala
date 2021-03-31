package edg_ide.psi_edits

import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiElement, PsiFile}
import com.jetbrains.python.psi.{LanguageLevel, PyClass, PyElementGenerator}
import edg.util.Errorable
import edg_ide.util.ExceptionNotifyImplicits.ExceptNotify
import edg_ide.util.exceptable


object DefineBlockAction {
  def createDefineBlockFlow(after: PsiElement, superclass: PyClass, actionName: String,
                            project: Project,
                            continuation: (String, PsiElement) => Unit): Errorable[() => Unit] = exceptable {
    val containingPsiFile = after.getParent
        .instanceOfExcept[PsiFile](s"must insert at top-level")

    val psiElementGenerator = PyElementGenerator.getInstance(project)

    def defineBlockFlow: Unit = {
      InsertAction.createNameEntryPopup("Block Name", project) { name => exceptable {
        val newClass = psiElementGenerator.createFromText(LanguageLevel.forElement(after),
          classOf[PyClass],
          s"""class $name(${superclass.getName}):
             |  def __init__(self) -> None:
             |    super().__init__()
             |    # block interface (ports, parameters) definition here
             |
             |  def contents(self) -> None:
             |    super().contents()
             |    # block implementation (subblocks, internal connections, footprint) here
             |""".stripMargin.replace("\r\n", "\n"))

        val added = writeCommandAction(project).withName(actionName).compute(() => {
          containingPsiFile.addAfter(newClass, after)
        })

        continuation(name, added)
      }}
    }
    () => defineBlockFlow
  }
}
