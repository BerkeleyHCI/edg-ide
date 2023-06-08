package edg_ide.runner

import com.intellij.execution.actions.{ConfigurationContext, LazyRunConfigurationProducer}
import com.intellij.execution.configurations.ConfigurationFactory
import com.intellij.openapi.util.Ref
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.jetbrains.python.psi.PyClass
import com.jetbrains.python.psi.types.TypeEvalContext
import edg_ide.util.DesignAnalysisUtils

import java.io.File

class DesignTopRunConfigurationProducer extends LazyRunConfigurationProducer[DesignTopRunConfiguration] {
  override def getConfigurationFactory: ConfigurationFactory = {
    new DesignTopConfigurationFactory(new DesignTopRunConfigurationType)
  }

  override def setupConfigurationFromContext(
      configuration: DesignTopRunConfiguration,
      context: ConfigurationContext,
      sourceElement: Ref[PsiElement]
  ): Boolean = {
    Option(PsiTreeUtil.getParentOfType(sourceElement.get(), classOf[PyClass])) match {
      case Some(psiPyClass) =>
        val project = psiPyClass.getProject
        // shouldn't fail, and if it does it should fail noisily
        val designTopClass = DesignAnalysisUtils.pyClassOf("edg_core.DesignTop.DesignTop", project).get
        if (psiPyClass.isSubclass(designTopClass, TypeEvalContext.codeAnalysis(project, null))) {
          configuration.setName(psiPyClass.getQualifiedName)
          configuration.options.designName = psiPyClass.getQualifiedName
          val containingDirectory = new File(psiPyClass.getContainingFile.getContainingDirectory.getVirtualFile.getPath)
          val designDirectory = new File(containingDirectory, psiPyClass.getName)
          val netlistFile = new File(designDirectory, psiPyClass.getName + ".net")
          configuration.options.netlistFile = netlistFile.getAbsolutePath
          val bomFile = new File(designDirectory, psiPyClass.getName + ".csv")
          configuration.options.bomFile = bomFile.getAbsolutePath
          configuration.options.pdfFile = ""
          true
        } else {
          false
        }
      case _ => false
    }
  }

  override def isConfigurationFromContext(
      configuration: DesignTopRunConfiguration,
      context: ConfigurationContext
  ): Boolean = {
    Option(PsiTreeUtil.getParentOfType(context.getLocation.getPsiElement, classOf[PyClass])) match {
      case Some(psiPyClass) =>
        psiPyClass.getQualifiedName == configuration.options.designName
      case None => false
    }
  }
}
