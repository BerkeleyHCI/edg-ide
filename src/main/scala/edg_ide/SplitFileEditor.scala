package edg_ide

import com.intellij.codeHighlighting.BackgroundEditorHighlighter
import com.intellij.ide.structureView.StructureViewBuilder
import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.editor._
import com.intellij.openapi.fileEditor._
import com.intellij.openapi.fileChooser._
import com.intellij.openapi.ui.{TextBrowseFolderListener, TextFieldWithBrowseButton}
import com.intellij.openapi.util.Disposer
import com.intellij.openapi.util.UserDataHolderBase
import com.intellij.openapi.vfs._
import com.intellij.pom.Navigatable
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.ui.JBSplitter
import com.intellij.ui.components.JBScrollPane
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.jetbrains.python.psi.{LanguageLevel, PyClass, PyElementGenerator, PyReferenceExpression, PyTargetExpression}

import javax.swing._
import java.awt._
import java.beans.PropertyChangeListener
import java.io._
import edg.elem.elem
import edg.schema.schema
import edg_ide.edgir_graph.{CollapseBridgeTransform, CollapseLinkTransform, EdgirGraph, HierarchyGraphElk, InferEdgeDirectionTransform, PruneDepthTransform, SimplifyPortTransform}
import org.eclipse.elk.graph.ElkGraphElement


class SplitFileEditor(private val textEditor: TextEditor, private val file: VirtualFile)
    extends UserDataHolderBase with TextEditor {
  // State
  //
  var edgFileAbsPath: Option[String] = None
  var edgLibraryAbsPath: Option[String] = None
  var library = new EdgirLibrary(schema.Library())

  // GUI-facing state
  //
  var selectedPath: Seq[String] = Seq()  // root implicitly selected by default
  var design = schema.Design()

  // Build GUI components
  //
  textEditor.getComponent.setVisible(true)

  val mainSplitter = new JBSplitter(false, 0.5f, 0.1f, 0.9f)
  mainSplitter.setFirstComponent(textEditor.getComponent)

  val ideSplitter = new JBSplitter(true, 0.5f)
  mainSplitter.setSecondComponent(ideSplitter)

  def makeGbc(gridx: Int, gridy: Int, fill: Int = GridBagConstraints.NONE,
              xsize: Int = 1, ysize: Int = 1,
              xweight: Float = 0.0f, yweight: Float = 0.0f): GridBagConstraints = {
    val gbc = new GridBagConstraints()
    gbc.gridx = gridx
    gbc.gridy = gridy
    gbc.fill = fill
    if (xweight == 0 && (fill == GridBagConstraints.HORIZONTAL || fill == GridBagConstraints.BOTH)) {
      gbc.weightx = 1  // default fill weight
    } else {
      gbc.weightx = xweight
    }
    if (yweight == 0 && (fill == GridBagConstraints.VERTICAL || fill == GridBagConstraints.BOTH)) {
      gbc.weighty = 1
    } else {
      gbc.weighty = yweight
    }
    gbc.gridwidth = xsize
    gbc.gridheight = ysize
    gbc
  }

  //
  //  Visualization Panel
  //
  val visualizationPanel = new JPanel(new GridBagLayout())
  visualizationPanel.setBorder(BorderFactory.createEtchedBorder())
  ideSplitter.setFirstComponent(visualizationPanel)

  val fileBrowser = new TextFieldWithBrowseButton()
  visualizationPanel.add(fileBrowser, makeGbc(0, 0, GridBagConstraints.HORIZONTAL))
  val fileLabel = new JLabel("No file")
  visualizationPanel.add(fileLabel, makeGbc(0, 1, GridBagConstraints.HORIZONTAL))
  val libraryBrowser = new TextFieldWithBrowseButton()
  visualizationPanel.add(libraryBrowser, makeGbc(0, 2, GridBagConstraints.HORIZONTAL))
  val libraryLabel = new JLabel("No library")
  visualizationPanel.add(libraryLabel, makeGbc(0, 3, GridBagConstraints.HORIZONTAL))

  val graph = new JElkGraph(HierarchyGraphElk.HGraphNodeToElk(
    EdgirGraph.blockToNode(elem.HierarchyBlock(), "empty", library))) {
    override def onSelected(node: ElkGraphElement): Unit = {
      selectedPath = getSelectedByPath
      notificationGroup.createNotification(
        s"selected path $selectedPath", NotificationType.WARNING)
          .notify(getEditor.getProject)
    }
  }
  val graphScrollPane = new JBScrollPane(graph) with ZoomingScrollPane
  visualizationPanel.add(graphScrollPane, makeGbc(0, 4, GridBagConstraints.BOTH))

  val fileDescriptor: FileChooserDescriptor = FileChooserDescriptorFactory.createSingleFileDescriptor()
  fileBrowser.addBrowseFolderListener(new TextBrowseFolderListener(fileDescriptor, null) {
    override def onFileChosen(chosenFile: VirtualFile) {
      val file = VfsUtilCore.virtualToIoFile(chosenFile)
      openEdgFile(file)
    }
  })
  libraryBrowser.addBrowseFolderListener(new TextBrowseFolderListener(fileDescriptor, null) {
    override def onFileChosen(chosenFile: VirtualFile) {
      val file = VfsUtilCore.virtualToIoFile(chosenFile)
      openEdgLibrary(file)
    }
  })

  //
  // Tree Panel
  //
  val treePanel = new JPanel(new GridBagLayout())
  ideSplitter.setSecondComponent(treePanel)

  val designTree = new TreeTable(new EdgTreeTableModel(edg.elem.elem.HierarchyBlock()))
  designTree.setShowColumns(true)
  val designTreeScrollPane = new JBScrollPane(designTree)
  treePanel.add(designTreeScrollPane, makeGbc(0, 0, GridBagConstraints.BOTH))

  val libraryTree = new TreeTable(new EdgirLibraryTreeTableModel(new EdgirLibrary(edg.schema.schema.Library())))
  libraryTree.setShowColumns(true)
  val libraryTreeScrollPane = new JBScrollPane(libraryTree)
  treePanel.add(libraryTreeScrollPane, makeGbc(1, 0, GridBagConstraints.BOTH))

  //
  // Interaction Implementations
  //
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.SplitFileEditor")

  def openEdgFile(file: File): Unit = {
    val absolutePath = file.getAbsolutePath
    fileBrowser.setText(absolutePath)

    val fileInputStream = new FileInputStream(file)
    design = schema.Design.parseFrom(fileInputStream)
    design.contents match {
      case Some(block) =>
        edgFileAbsPath = Some(absolutePath)
        val edgirGraph = EdgirGraph.blockToNode(block, "root", library)
        val layoutGraphRoot = HierarchyGraphElk.HGraphNodeToElk(
          CollapseBridgeTransform(CollapseLinkTransform(
            InferEdgeDirectionTransform(SimplifyPortTransform(
              PruneDepthTransform(edgirGraph, 2))))))  // TODO configurable depth
        fileLabel.setText(s"${block.getClass.toString}, " +
            s"automatic layout ${layoutGraphRoot.getWidth}*${layoutGraphRoot.getHeight}")
        graph.setGraph(layoutGraphRoot)
        designTree.setModel(new EdgTreeTableModel(block))
        designTree.setRootVisible(false)  // this seems to get overridden when the model is updated
      case None =>
        edgFileAbsPath = None
        fileLabel.setText(s"Invalid file format: $absolutePath")
    }
    fileInputStream.close()
  }

  def openEdgLibrary(file: File): Unit = {
    val absolutePath = file.getAbsolutePath
    libraryBrowser.setText(absolutePath)

    val fileInputStream = new FileInputStream(file)
    val libraryProto: schema.Library = schema.Library.parseFrom(fileInputStream)
    libraryProto.root match {
      case Some(namespace) =>
        edgLibraryAbsPath = Some(absolutePath)
        libraryLabel.setText(s"Library with ${namespace.members.keys.size} elements")
        library = new EdgirLibrary(libraryProto)
        libraryTree.setModel(new EdgirLibraryTreeTableModel(library))
        libraryTree.setRootVisible(false)  // this seems to get overridden when the model is updated
        // TODO: actual loading here
      case None =>
        edgLibraryAbsPath = None
        libraryLabel.setText(s"Invalid or empty library: $absolutePath")
        library = new EdgirLibrary(schema.Library())
    }
    fileInputStream.close()
  }

  /**
    * Selects the block diagram element associated with the PSI element, in both the block diagram and tree views.
    */
  def selectFromPsi(element: PsiElement) {
    val containingClass = PsiTreeUtil.getParentOfType(element, classOf[PyClass]) match {
      case null =>
        notificationGroup.createNotification(
          s"No encapsulating class of selection",
          NotificationType.WARNING)
            .notify(getEditor.getProject)
        return
      case pyClass: PyClass => pyClass.getNameIdentifier.getText
    }

    val referenceOpt = PsiTreeUtil.getParentOfType(element, classOf[PyReferenceExpression]) match {
      case expr: PyReferenceExpression => PsiUtils.psiSelfReference(getEditor.getProject, expr)
      case _ => None
    }
    val targetOpt = PsiTreeUtil.getParentOfType(element, classOf[PyTargetExpression]) match {
      case expr: PyTargetExpression => PsiUtils.psiSelfTarget(getEditor.getProject, expr)
      case _ => None
    }

    val name = referenceOpt.getOrElse(targetOpt.getOrElse {
      notificationGroup.createNotification(
        s"No reference of form self.(element) selected",
        NotificationType.WARNING)
          .notify(getEditor.getProject)
      return
    } )

    // TODO IMPLEMENT ME
    if (design.contents.isDefined) {
      val startingBlock = EdgirUtils.ResolvePath(design.contents.get, selectedPath) match {
        case Some(startingBlock: elem.HierarchyBlock) => startingBlock
        case startingBlock =>
          println(s"Failed to resolve current path $selectedPath, got $startingBlock")  // TODO use logging infra
          return
      }

      val startingSuperclass = EdgirUtils.SimpleSuperclassesToString(startingBlock.superclasses)
      if (startingSuperclass == containingClass) {
        
      }

      notificationGroup.createNotification(
        s"SelectFromPsi", s"$startingSuperclass", s"selected PSI $containingClass  $name",
        NotificationType.INFORMATION)
          .notify(getEditor.getProject)
    }
  }

  //
  // Implementation for abstract TextEditor
  //
  override def getName = "PyCharm with EDG Live Visualization"

  override def getComponent: JComponent = mainSplitter
  override def getPreferredFocusedComponent: JComponent = textEditor.getPreferredFocusedComponent

  override def getState(level: FileEditorStateLevel) =
    new SplitFileEditorState(
      edgFileAbsPath,
      edgLibraryAbsPath,
      textEditor.getState(level)
    )
  override def setState(state: FileEditorState): Unit = state match {
    case state: SplitFileEditorState =>
      state.edgFileAbsPath.foreach { absPath => openEdgFile(new File(absPath)) }
      state.edgLibraryAbsPath.foreach { absPath => openEdgLibrary(new File(absPath)) }
      textEditor.setState(state.textState)
    case _ =>  // discard state type
  }

  override def isModified: Boolean = textEditor.isModified
  override def isValid: Boolean = textEditor.isValid
  override def selectNotify(): Unit = textEditor.selectNotify()
  override def deselectNotify(): Unit = textEditor.deselectNotify()

  override def addPropertyChangeListener(listener: PropertyChangeListener): Unit =
    textEditor.addPropertyChangeListener(listener)
  override def removePropertyChangeListener(listener: PropertyChangeListener): Unit =
    textEditor.removePropertyChangeListener(listener)

  override def getBackgroundHighlighter: BackgroundEditorHighlighter = textEditor.getBackgroundHighlighter
  override def getCurrentLocation: FileEditorLocation = textEditor.getCurrentLocation
  override def getStructureViewBuilder: StructureViewBuilder = textEditor.getStructureViewBuilder

  override def dispose(): Unit = Disposer.dispose(textEditor)

  override def getEditor: Editor = textEditor.getEditor
  override def canNavigateTo(navigatable: Navigatable): Boolean = textEditor.canNavigateTo(navigatable)
  override def navigateTo(navigatable: Navigatable): Unit = textEditor.navigateTo(navigatable)
}


// Container state around TextEditor
class SplitFileEditorState(val edgFileAbsPath: Option[String], val edgLibraryAbsPath: Option[String],
    val textState: FileEditorState)
    extends FileEditorState {
  override def canBeMergedWith(otherState: FileEditorState, level: FileEditorStateLevel): Boolean =
    otherState match {
      case otherState: SplitFileEditorState => textState.canBeMergedWith(otherState.textState, level)
      case _ => false
    }
}


object SplitFileEditor {
  /** Given a TextEditor, return the SplitFileEditor instance containing it.
    */
  def fromTextEditor(editor: Editor): Option[SplitFileEditor] = {
    val project = Option(editor.getProject)
    val document = editor.getDocument
    val file = Option(FileDocumentManager.getInstance().getFile(document))
    (project, file) match {
      case (None, _) | (_, None) => None
      case (Some(project), Some(file)) =>
        FileEditorManager.getInstance(project).getSelectedEditor(file) match {
          case editor: SplitFileEditor => Some(editor)
          case _ => None
        }
    }
  }
}
