package edg_ide.runner

import com.intellij.execution.actions.{ConfigurationContext, LazyRunConfigurationProducer}
import com.intellij.execution.configurations.ConfigurationFactory
import com.intellij.openapi.util.Ref
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.PyClass
import com.jetbrains.python.psi.types.TypeEvalContext
import edg_ide.util.DesignAnalysisUtils

class DesignTopRunConfigurationProducer extends LazyRunConfigurationProducer[DesignTopRunConfiguration] {
  override def getConfigurationFactory: ConfigurationFactory = {
    new DesignTopConfigurationFactory(new DesignTopRunConfigurationType)
  }

  override def setupConfigurationFromContext(configuration: DesignTopRunConfiguration, context: ConfigurationContext,
                                             sourceElement: Ref[PsiElement]): Boolean = {
    sourceElement match {
      case sourceElement: PyClass =>
        val project = sourceElement.getProject
        // shouldn't fail, and if it does it should fail noisily
        val designTopClass = DesignAnalysisUtils.pyClassOf("edg_core.DesignTop.DesignTop", project).get
        if (sourceElement.isSubclass(designTopClass, TypeEvalContext.codeAnalysis(project, null))) {
          configuration.options.designName = sourceElement.getQualifiedName
          true
        } else {
          false
        }
      case _ => false
    }
  }

  override def isConfigurationFromContext(configuration: DesignTopRunConfiguration,
                                          context: ConfigurationContext): Boolean = {
    Option(PsiTreeUtil.getParentOfType(context.getLocation.getPsiElement, classOf[PyClass])) match {
      case Some(psiElement) => psiElement.getQualifiedName == configuration.options.designName
      case None => false
    }
  }
}
