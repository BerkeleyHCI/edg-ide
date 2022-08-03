package edg_ide.runner

import com.intellij.codeInsight.daemon.LineMarkerInfo.LineMarkerGutterIconRenderer
import com.intellij.codeInsight.daemon.{LineMarkerInfo, LineMarkerProvider}
import com.intellij.icons.AllIcons
import com.intellij.openapi.actionSystem.{ActionGroup, AnAction, AnActionEvent, DefaultActionGroup}
import com.intellij.openapi.application.ReadAction
import com.intellij.openapi.editor.markup.GutterIconRenderer
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.psi.PsiElement
import com.intellij.psi.impl.source.tree.LeafPsiElement
import com.jetbrains.python.PyTokenTypes
import com.jetbrains.python.psi.PyClass
import com.jetbrains.python.psi.search.PyClassInheritorsSearch
import com.jetbrains.python.psi.types.TypeEvalContext
import edg.EdgirUtils.SimpleLibraryPath
import edg.ElemBuilder
import edg.wir.DesignPath
import edg_ide.ui.{BlockVisualizerService, PopupUtils}
import edg_ide.util.ExceptionNotifyImplicits.ExceptOption
import edg_ide.util.{DesignAnalysisUtils, DesignFindBlockOfTypes, exceptionNotify}
import edgir.elem.elem

import scala.jdk.CollectionConverters.CollectionHasAsScala


class FocusToBlockAction(visualizer: BlockVisualizerService, path: DesignPath, block: elem.HierarchyBlock)
    extends AnAction(s"Visualizer Focus to ${block.getSelfClass.toSimpleString} at $path") {
  override def actionPerformed(event: AnActionEvent): Unit = exceptionNotify(this.getClass.getCanonicalName, event.getProject) {
    visualizer.setContext(path)
  }
}


class BlockLineMarkerInfo(identifier: PsiElement, pyClass: PyClass) extends
    LineMarkerInfo[PsiElement](identifier, identifier.getTextRange, AllIcons.Toolwindows.ToolWindowHierarchy,
      {elem: PsiElement => "Visualizer Focus to Block"}, null, GutterIconRenderer.Alignment.RIGHT) {
  val project = identifier.getProject

  override def createGutterRenderer(): GutterIconRenderer = new LineMarkerGutterIconRenderer[PsiElement](this) {
    override def getClickAction: AnAction = getActions match {
      case Seq(action) => action
      case Seq() => new AnAction() {  // dummy action, doesn't do anything
        override def actionPerformed(e: AnActionEvent): Unit = {
          PopupUtils.createErrorPopup(s"No instances of ${pyClass.getName}",
            FileEditorManager.getInstance(project).getSelectedTextEditor)}
      }
      case _ => new AnAction() {  // dummy action, doesn't do anything
        override def actionPerformed(e: AnActionEvent): Unit = {
          PopupUtils.createErrorPopup(s"Multiple instances of ${pyClass.getName}",
            FileEditorManager.getInstance(project).getSelectedTextEditor)}
      }
    }
    override def getPopupMenuActions: ActionGroup = {
      val actions = getActions
      val actionGroup = new DefaultActionGroup()
      if (actions.isEmpty) {
        actionGroup.addSeparator(s"No instances of ${pyClass.getName}")
      } else {
        getActions.foreach( action => actionGroup.addAction(action) )
      }
      actionGroup
    }

    // Traverses the design graph and looks for the class, adding them as navigation events
    protected def getActions: Seq[AnAction] = {
      val visualizer = BlockVisualizerService(project)
      val design = visualizer.getDesign.exceptNone("no design")
      val (contextPath, contextBlock) = visualizer.getContextBlock.exceptNone("no visualizer context")

      val extendedClasses = ReadAction.compute(() => {
        val inheritors = PyClassInheritorsSearch.search(pyClass, true).findAll().asScala
        inheritors.toSeq :+ pyClass
      })
      val targetTypes = extendedClasses.map { pyClass =>
        ElemBuilder.LibraryPath(pyClass.getQualifiedName)
      }.toSet

      val instancesOfClass = new DesignFindBlockOfTypes(targetTypes).map(design)
          .sortWith { case ((blockPath1, block1), (blockPath2, block2)) =>
            if (blockPath1 == contextPath && blockPath2 != contextPath) {
              true  // Prefer exact match first
            } else if (blockPath1.startsWith(contextPath) && !blockPath2.startsWith(contextPath)) {
              true  // Prefer children next
            } else if (contextPath.startsWith(blockPath1) && !contextPath.startsWith(blockPath2)) {
              true  // Prefer parents next
            } else {
              false
            }
          }
      instancesOfClass.map { case (path, block) => new FocusToBlockAction(visualizer, path, block) }
    }
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
