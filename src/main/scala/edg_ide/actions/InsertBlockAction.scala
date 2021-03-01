package edg_ide.actions

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.command.WriteCommandAction.writeCommandAction
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.types.TypeEvalContext
import com.jetbrains.python.psi.{LanguageLevel, PyAssignmentStatement, PyClass, PyElementGenerator, PyFunction, PyStatementList}
import edg_ide.ui.BlockVisualizerService
import edg_ide.util.{exceptionNotify, requireExcept}
import edg_ide.util.ExceptionNotifyImplicits._


object InsertBlockAction {
  val VALID_FUNCTION_NAMES = Set("__init__", "contents")
  val VALID_SUPERCLASS = "edg_core.HierarchyBlock.Block"
}


class InsertBlockAction() extends AnAction() {
  override def actionPerformed(event: AnActionEvent): Unit = {
    exceptionNotify("edg_ide.actions.InsertBlockAction", event.getProject) {
      val visualizer = BlockVisualizerService.apply(event.getProject).visualizerPanelOption
          .exceptNull("No visualizer panel")

      val editor = event.getData(CommonDataKeys.EDITOR).exceptNull("No editor")
      val offset = editor.getCaretModel.getOffset
      val psiFile = event.getData(CommonDataKeys.PSI_FILE).exceptNull("No PSI file")
      val psiElement = psiFile.findElementAt(offset).exceptNull("No PSI element")

      val psiClass = PsiTreeUtil.getParentOfType(psiElement, classOf[PyClass])
          .exceptNull("No containing PSI class")
      requireExcept(psiClass.isSubclass(InsertBlockAction.VALID_SUPERCLASS, TypeEvalContext.codeCompletion(event.getProject, psiFile)),
        s"Containing class ${psiClass.getName} is not a subclass of ${InsertBlockAction.VALID_FUNCTION_NAMES}")

      val psiContainingList = psiElement.getParent.instanceOfExcept[PyStatementList](s"Invalid location to insert block")
      val psiContainingFunction = psiContainingList.getParent.instanceOfExcept[PyFunction]("Not in a function")
      requireExcept(InsertBlockAction.VALID_FUNCTION_NAMES.contains(psiContainingFunction.getName),
        s"Containing function ${psiContainingFunction.getName} not valid for block insertion")

      val selfName = psiContainingFunction.getParameterList.getParameters()(0).getName

      val psiElementGenerator = PyElementGenerator.getInstance(event.getProject)
      val newAssign = psiElementGenerator.createFromText(LanguageLevel.forElement(psiElement),
        classOf[PyAssignmentStatement],
        s"$selfName.target_name = $selfName.Block(TargetClass())"
      )

      writeCommandAction(event.getProject).withName("Insert Block").run(() => {
        psiContainingList.addAfter(newAssign, psiElement)
      })
    }
  }
}
