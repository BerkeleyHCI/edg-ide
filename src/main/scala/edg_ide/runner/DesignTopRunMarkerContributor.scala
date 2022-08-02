package edg_ide.runner

import com.intellij.execution.lineMarker.RunLineMarkerContributor.Info
import com.intellij.execution.lineMarker.{ExecutorAction, RunLineMarkerContributor}
import com.intellij.icons.AllIcons
import com.intellij.psi.PsiElement
import com.intellij.psi.impl.source.tree.LeafPsiElement
import com.jetbrains.python.PyTokenTypes
import com.jetbrains.python.psi.PyClass
import com.jetbrains.python.psi.types.TypeEvalContext
import edg_ide.util.DesignAnalysisUtils


class DesignTopRunMarkerContributor extends RunLineMarkerContributor {
  override def getInfo(element: PsiElement): RunLineMarkerContributor.Info = {
    element match {
      case element: LeafPsiElement if element.getElementType == PyTokenTypes.IDENTIFIER =>
      case _ => return null
    }
    element.getParent match {
      case parent: PyClass =>
        val project = element.getProject
        // shouldn't fail, and if it does it should fail noisily
        val designTopClass = DesignAnalysisUtils.pyClassOf("edg_core.DesignTop.DesignTop", project).get
        if (parent.isSubclass(designTopClass, TypeEvalContext.codeAnalysis(project, null))) {
          new Info(AllIcons.RunConfigurations.TestState.Run, ExecutorAction.getActions(), null)
        } else {
          null
        }
      case _ => null
    }
  }
}
