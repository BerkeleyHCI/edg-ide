package edg_ide.runner

import com.intellij.codeInsight.daemon.{LineMarkerInfo, LineMarkerProvider}
import com.intellij.icons.AllIcons
import com.intellij.openapi.editor.markup.GutterIconRenderer
import com.intellij.psi.PsiElement
import com.intellij.psi.impl.source.tree.LeafPsiElement
import com.jetbrains.python.PyTokenTypes
import com.jetbrains.python.psi.PyClass
import com.jetbrains.python.psi.types.TypeEvalContext
import edg_ide.util.DesignAnalysisUtils


class DesignTopLineMarkerContributor extends LineMarkerProvider  {
  override def getLineMarkerInfo(element: PsiElement): LineMarkerInfo[PsiElement] = {
    element match {
      case element: LeafPsiElement if element.getElementType == PyTokenTypes.IDENTIFIER =>
      case _ => return null
    }
    val project = element.getProject
    // shouldn't fail, and if it does it should fail noisily
    val designTopClass = DesignAnalysisUtils.pyClassOf("edg_core.DesignTop.DesignTop", project).get

    element.getParent match {
      case parent: PyClass if parent.isSubclass(designTopClass, TypeEvalContext.codeAnalysis(project, null)) =>
        new LineMarkerInfo(element, element.getTextRange, AllIcons.Toolwindows.ToolWindowHierarchy,
          null, null, GutterIconRenderer.Alignment.RIGHT)
      case _ => null
    }
  }
}
