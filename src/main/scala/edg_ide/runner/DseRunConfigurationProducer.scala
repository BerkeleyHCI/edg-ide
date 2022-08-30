package edg_ide.runner

import com.intellij.execution.actions.{ConfigurationContext, LazyRunConfigurationProducer}
import com.intellij.execution.configurations.ConfigurationFactory
import com.intellij.openapi.util.Ref
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.PyClass
import com.jetbrains.python.psi.types.TypeEvalContext
import edg_ide.util.DesignAnalysisUtils


class DseRunConfigurationProducer extends LazyRunConfigurationProducer[DseRunConfiguration] {
  override def getConfigurationFactory: ConfigurationFactory = {
    new DseConfigurationFactory(new DseRunConfigurationType)
  }

  override def setupConfigurationFromContext(configuration: DseRunConfiguration, context: ConfigurationContext,
                                             sourceElement: Ref[PsiElement]): Boolean = {
    return false  // DSE is still experimental, so isn't being plumbed through to the UI

    Option(PsiTreeUtil.getParentOfType(sourceElement.get(), classOf[PyClass])) match {
      case Some(psiPyClass) =>
        val project = psiPyClass.getProject
        // shouldn't fail, and if it does it should fail noisily
        val designTopClass = DesignAnalysisUtils.pyClassOf("edg_core.DesignTop.DesignTop", project).get
        if (psiPyClass.isSubclass(designTopClass, TypeEvalContext.codeAnalysis(project, null))) {
          configuration.setName(psiPyClass.getQualifiedName)
          configuration.options.designName = psiPyClass.getQualifiedName
          true
        } else {
          false
        }
      case _ => false
    }
  }

  override def isConfigurationFromContext(configuration: DseRunConfiguration,
                                          context: ConfigurationContext): Boolean = {
    Option(PsiTreeUtil.getParentOfType(context.getLocation.getPsiElement, classOf[PyClass])) match {
      case Some(psiPyClass) =>
        psiPyClass.getQualifiedName == configuration.options.designName
      case None => false
    }
  }
}
