package edg_ide.runner

import com.intellij.codeInsight.daemon.LineMarkerInfo.LineMarkerGutterIconRenderer
import com.intellij.codeInsight.daemon.{LineMarkerInfo, LineMarkerProvider}
import com.intellij.icons.AllIcons
import com.intellij.openapi.actionSystem.{ActionGroup, AnAction, AnActionEvent, DefaultActionGroup}
import com.intellij.openapi.editor.markup.GutterIconRenderer
import com.intellij.psi.PsiElement
import com.intellij.psi.impl.source.tree.LeafPsiElement
import com.jetbrains.python.PyTokenTypes
import com.jetbrains.python.psi.PyClass
import com.jetbrains.python.psi.types.TypeEvalContext
import edg_ide.ui.BlockVisualizerService
import edg_ide.util.ExceptionNotifyImplicits.ExceptOption
import edg_ide.util.{DesignAnalysisUtils, exceptionNotify}


class BlockNavigateToAction(pyClass: PyClass) extends AnAction(s"Navigate to ${pyClass.getName}") {
  override def actionPerformed(event: AnActionEvent): Unit = exceptionNotify(this.getClass.getCanonicalName, event.getProject) {
    // TODO Implement Me
//    val visualizer = BlockVisualizerService(event.getProject).visualizerPanelOption.exceptNone("No visualizer panel")
//    val classNameParts = pyClass.getQualifiedName.split('.')
//    visualizer.setFileBlock(classNameParts.init.mkString("."), classNameParts.last)
  }
}


class BlockLineMarkerInfo(identifier: PsiElement, pyClass: PyClass) extends
    LineMarkerInfo[PsiElement](identifier, identifier.getTextRange, AllIcons.Toolwindows.ToolWindowHierarchy,
      null, null, GutterIconRenderer.Alignment.RIGHT) {
  override def createGutterRenderer(): GutterIconRenderer = new LineMarkerGutterIconRenderer[PsiElement](this) {
    override def getClickAction: AnAction = new BlockNavigateToAction(pyClass)
    override def getPopupMenuActions: ActionGroup = new DefaultActionGroup(getClickAction)
  }
}


// Adds the line markers for DesignTop-based classes.
// See https://developerlife.com/2021/03/13/ij-idea-plugin-advanced/#add-line-marker-provider-in-your-plugin
class BlockLineMarkerContributor extends LineMarkerProvider  {
  override def getLineMarkerInfo(element: PsiElement): LineMarkerInfo[PsiElement] = {
    element match {
      case element: LeafPsiElement if element.getElementType == PyTokenTypes.IDENTIFIER =>
      case _ => return null
    }
    element.getParent match {
      case parent: PyClass =>
        val project = element.getProject
        // shouldn't fail, and if it does it should fail noisily
        val designTopClass = DesignAnalysisUtils.pyClassOf("edg_core.DesignTop.DesignTop", project).get
        val blockClass = DesignAnalysisUtils.pyClassOf("edg_core.Block.Block", project).get
        if (parent.isSubclass(blockClass, TypeEvalContext.codeAnalysis(project, null)) &&
            !parent.isSubclass(designTopClass, TypeEvalContext.codeAnalysis(project, null))) {
          new BlockLineMarkerInfo(element, parent)
        } else {
          null
        }
      case _ => null
    }
  }
}
