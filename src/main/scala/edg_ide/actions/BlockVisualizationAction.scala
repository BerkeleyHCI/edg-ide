package edg_ide.actions

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.PyClass
import edg_ide.ui.BlockVisualizerService
import edg_ide.util.ErrorableNotify._


class BlockVisualizationAction() extends AnAction() {
  override def actionPerformed(event: AnActionEvent): Unit = {
    val visualizer = Errorable(BlockVisualizerService(event.getProject).visualizerPanelOption,
      "No visualizer panel")

    val editor = Errorable(event.getData(CommonDataKeys.EDITOR), "No editor")
    val offset = editor.map { _.getCaretModel.getOffset }
    val psiFile = Errorable(event.getData(CommonDataKeys.PSI_FILE), "No PSI file")
    val containingClass = (psiFile + offset).map("No element") {
      case (psiFile, offset) => psiFile.findElementAt(offset)
    }.map("No containing class") { PsiTreeUtil.getParentOfType(_, classOf[PyClass]) }
    val module = psiFile.flatMap(s"PSI File $psiFile not in project ${event.getProject.getBaseDir}") { psiFile =>
      ModuleUtil.from(event.getProject.getBaseDir, psiFile.getVirtualFile)
    }

    (visualizer + (module + containingClass)).mapOrNotify(
      this.getClass.getCanonicalName, event.getProject) {
      case (visualizer, (module, containingClass)) =>
        visualizer.setFileBlock(module.mkString("."), containingClass.getNameIdentifier.getText)
    }
  }
}
