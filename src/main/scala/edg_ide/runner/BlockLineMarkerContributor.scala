package edg_ide.runner

import com.intellij.codeInsight.daemon.LineMarkerInfo.LineMarkerGutterIconRenderer
import com.intellij.codeInsight.daemon.{LineMarkerInfo, LineMarkerProvider}
import com.intellij.icons.AllIcons
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent}
import com.intellij.openapi.application.{ModalityState, ReadAction}
import com.intellij.openapi.editor.markup.GutterIconRenderer
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.psi.PsiElement
import com.intellij.psi.impl.source.tree.LeafPsiElement
import com.intellij.util.concurrency.AppExecutorUtil
import com.jetbrains.python.PyTokenTypes
import com.jetbrains.python.psi.PyClass
import com.jetbrains.python.psi.search.PyClassInheritorsSearch
import com.jetbrains.python.psi.types.TypeEvalContext
import edg.EdgirUtils.SimpleLibraryPath
import edg.ElemBuilder
import edg.wir.DesignPath
import edg_ide.actions.FocusToElementAction
import edg_ide.ui.{BlockVisualizerService, PopupUtils}
import edg_ide.util.ExceptionNotifyImplicits.ExceptOption
import edg_ide.util.{DesignAnalysisUtils, DesignFindBlockOfTypes, exceptionPopup}
import edgir.elem.elem

import java.util.concurrent.Callable
import scala.jdk.CollectionConverters.CollectionHasAsScala


class FocusToBlockSelectAction(identifier: PsiElement, pyClass: PyClass)
    extends AnAction(s"Visualizer Focus to ${pyClass.getName}") {
  val project = pyClass.getProject
  val editor = FileEditorManager.getInstance(project).getSelectedTextEditor


  case class NavigateNode(path: DesignPath, block: elem.HierarchyBlock) {
    override def toString: String = s"Visualizer Focus to ${block.getSelfClass.toSimpleString} at $path"
  }

  override def actionPerformed(e: AnActionEvent): Unit = exceptionPopup(editor) {
    val visualizer = BlockVisualizerService(project)
    val design = visualizer.getDesign.exceptNone("no design")
    val (contextPath, contextBlock) = visualizer.getContextBlock.exceptNone("no visualizer context")

    ReadAction.nonBlocking((() => {
      val inheritors = PyClassInheritorsSearch.search(pyClass, true).findAll().asScala
      val extendedClasses = inheritors.toSeq :+ pyClass
      val targetTypes = extendedClasses.map { pyClass =>
        ElemBuilder.LibraryPath(pyClass.getQualifiedName)
      }.toSet

      val instancesOfClass = new DesignFindBlockOfTypes(targetTypes).map(design)
          .sortWith { case ((blockPath1, block1), (blockPath2, block2)) =>
            FocusToElementAction.pathSortFn(contextPath)(blockPath1, blockPath2)
          }
      instancesOfClass.map { case (path, block) => NavigateNode(path, block) }
    }): Callable[Seq[NavigateNode]]).finishOnUiThread(ModalityState.defaultModalityState(), items => {
      PopupUtils.createMenuPopup(s"Visualizer focus to ${pyClass.getName}", items, editor) { selected =>
        visualizer.setContext(selected.path)
      }
    }).submit(AppExecutorUtil.getAppExecutorService)
  }
}


class BlockLineMarkerInfo(identifier: PsiElement, pyClass: PyClass) extends
    LineMarkerInfo[PsiElement](identifier, identifier.getTextRange, AllIcons.Toolwindows.ToolWindowHierarchy,
      {elem: PsiElement => s"Visualizer Focus to ${pyClass.getName}"}, null, GutterIconRenderer.Alignment.RIGHT,
      () => "Visualizer Focus Block") {
  override def createGutterRenderer(): GutterIconRenderer = new LineMarkerGutterIconRenderer[PsiElement](this) {
    override def getClickAction: AnAction = new FocusToBlockSelectAction(identifier, pyClass)
  }
}


// Adds the line markers for DesignTop-based classes.
// See https://developerlife.com/2021/03/13/ij-idea-plugin-advanced/#add-line-marker-provider-in-your-plugin
// TODO not actually sure how useful this UI is, it's definitely questionable
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
        val blockClass = DesignAnalysisUtils.pyClassOf("edg_core.HierarchyBlock.Block", project).get
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
