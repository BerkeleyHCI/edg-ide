package edg_ide.actions

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.PyClass
import edg_ide.ui.BlockVisualizerService
import edg_ide.util.ExceptionNotifyImplicits.{ExceptNotify, ExceptOption}
import edg_ide.util.exceptionNotify


class SetVisualizerTopAction() extends AnAction() {
  override def actionPerformed(event: AnActionEvent): Unit = exceptionNotify(this.getClass.getCanonicalName, event.getProject) {
    val visualizer = BlockVisualizerService(event.getProject).visualizerPanelOption.exceptNone("No visualizer panel")

    val editor = event.getData(CommonDataKeys.EDITOR).exceptNull("No editor")
    val offset = editor.getCaretModel.getOffset
    val psiFile = event.getData(CommonDataKeys.PSI_FILE).exceptNull("No PSI file")
    val caretElement = psiFile.findElementAt(offset).exceptNull("No element")
    val containingClass = PsiTreeUtil.getParentOfType(caretElement, classOf[PyClass]).exceptNull("No containing class")
    val module = ModuleUtil.from(event.getProject.getBaseDir, psiFile.getVirtualFile).exceptNone(
      s"PSI File $psiFile not in project ${event.getProject.getBaseDir}")

    visualizer.setFileBlock(module.mkString("."), containingClass.getNameIdentifier.getText)
  }
}
