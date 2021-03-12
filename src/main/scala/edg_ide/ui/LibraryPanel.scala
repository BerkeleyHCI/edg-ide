package edg_ide.ui

import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.ui.components.{JBScrollPane, JBTextArea}
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.intellij.ui.{JBSplitter, TreeTableSpeedSearch}
import edg.ref.ref
import edg.elem.elem
import edg.util.Errorable
import edg.wir.DesignPath
import edg.{IrPort, wir}
import edg_ide.actions.{InsertAction, InsertBlockAction}
import edg_ide.edgir_graph.{CollapseBridgeTransform, CollapseLinkTransform, EdgirGraph, ElkEdgirGraphUtils, HierarchyGraphElk, InferEdgeDirectionTransform, PruneDepthTransform, SimplifyPortTransform}
import edg_ide.swing.{EdgirLibraryTreeNode, EdgirLibraryTreeTableModel, FilteredTreeTableModel, JElkGraph}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption, ExceptSeq}
import edg_ide.util.{DesignAnalysisUtils, exceptable, exceptionNotify, exceptionPopup, requireExcept}
import edg_ide.{EdgirUtils, PsiUtils}

import java.awt.{BorderLayout, GridBagConstraints, GridBagLayout}
import java.awt.event.{MouseAdapter, MouseEvent}
import javax.swing.event.{DocumentEvent, DocumentListener, TreeSelectionEvent, TreeSelectionListener}
import javax.swing.tree.TreePath
import javax.swing.{JLabel, JPanel, JPopupMenu, JTextField, SwingUtilities}


class LibraryBlockPopupMenu(libType: ref.LibraryPath, project: Project) extends JPopupMenu {
  val libName = EdgirUtils.SimpleLibraryPath(libType)
  add(new JLabel(s"Library Block: $libName"))
  addSeparator()

  private val libPyClass = DesignAnalysisUtils.pyClassOf(libType, project)
  private val (contextPath, _) = BlockVisualizerService(project).getContextBlock.get
  private val contextPyClass = InsertAction.getPyClassOfContext(project)
  private val contextPyName = contextPyClass.mapToString(_.getName)

  // Edit actions
  private val caretPsiElement = exceptable {
    val contextPsiFile = contextPyClass.exceptError.getContainingFile.exceptNull("no file")
    InsertAction.getCaretAtFile(contextPsiFile, contextPyClass.exceptError, project).exceptError
  }
  private def insertContinuation(name: String, added: PsiElement): Unit = {
    InsertAction.navigateElementFn(name, added)

    val library = EdgCompilerService(project).pyLib
    val fastPathUtil = new DesignFastPathUtil(library)

    exceptionNotify("edg.ui.LibraryPanel", project) {
      val visualizerPanel = BlockVisualizerService(project).visualizerPanelOption
          .exceptNone("no visualizer panel")
      visualizerPanel.currentDesignModifyBlock(contextPath) { _.update(
        _.blocks :+= (name, fastPathUtil.instantiateStubBlockLike(libType).exceptError)
      )}
    }
  }

  val caretInsertAction: Errorable[() => Unit] = exceptable {
    InsertBlockAction.createInsertBlockFlow(caretPsiElement.exceptError, libPyClass.exceptError,
        s"Insert $libName at $contextPyName caret",
        project, insertContinuation).exceptError
  }
  private val caretFileLine = exceptable {
    caretInsertAction.exceptError
    PsiUtils.fileNextLineOf(caretPsiElement.exceptError, project).exceptError
  }.mapToStringOrElse(fileLine => s" ($fileLine)", err => "")

  private val caretInsertItem = PopupMenuUtils.MenuItemFromErrorable(
    caretInsertAction, s"Insert at $contextPyName caret$caretFileLine")
  add(caretInsertItem)

  private val insertionPairs = exceptable {
    InsertAction.findInsertionPoints(contextPyClass.exceptError, project).exceptError
        .map { fn =>
          val fileLine = PsiUtils.fileNextLineOf(fn.getStatementList.getLastChild, project)
              .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
          val label = s"Insert at ${contextPyName}.${fn.getName}$fileLine"
          val action = InsertBlockAction.createInsertBlockFlow(fn.getStatementList.getStatements.last, libPyClass.exceptError,
            s"Insert $libName at $contextPyName.${fn.getName}",
            project, insertContinuation)
          (label, action)
        } .collect {
          case (fn, Errorable.Success(action)) => (fn, action)
        }.exceptEmpty("no insertion points")
  }
  PopupMenuUtils.MenuItemsFromErrorableSeq(insertionPairs, s"Insert into $contextPyName")
      .foreach(add)
  addSeparator()

  // Navigation actions
  val gotoDefinitionAction: Errorable[() => Unit] = exceptable {
    requireExcept(libPyClass.exceptError.canNavigateToSource, "class not navigatable")
    () => libPyClass.exceptError.navigate(true)
  }
  private val libFileLine = libPyClass.flatMap(PsiUtils.fileLineOf(_, project))
      .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
  private val gotoDefinitionItem = PopupMenuUtils.MenuItemFromErrorable(gotoDefinitionAction,
    s"Goto Definition$libFileLine")
  add(gotoDefinitionItem)
}


class LibraryPreview(project: Project) extends JPanel {
  // State
  //

  // GUI Components
  //
  setLayout(new GridBagLayout)
  private val nameLabel = new JLabel("No Library Element Selected")
  add(nameLabel, Gbc(0, 0, GridBagConstraints.HORIZONTAL))

  private val emptyHGraph = HierarchyGraphElk.HGraphNodeToElk(
    EdgirGraph.blockToNode(DesignPath(), elem.HierarchyBlock()))

  private val graph = new JElkGraph(emptyHGraph)
  add(graph, Gbc(0, 1, GridBagConstraints.BOTH))

  // Actions
  //
  def setBlock(library: wir.Library, blockType: ref.LibraryPath): Unit = {
    val blockGraph = exceptable {
      val fastPath = new DesignFastPathUtil(library)
      val block = fastPath.instantiateStubBlock(blockType).exceptError
      val edgirGraph = EdgirGraph.blockToNode(DesignPath(), block)
      val transformedGraph = CollapseBridgeTransform(CollapseLinkTransform(
        InferEdgeDirectionTransform(SimplifyPortTransform(
          PruneDepthTransform(edgirGraph, 2)))))
      HierarchyGraphElk.HGraphNodeToElk(transformedGraph,
        Seq(ElkEdgirGraphUtils.PortSideMapper, ElkEdgirGraphUtils.PortConstraintMapper),
        true)  // need to make a root so root doesn't have ports
    }
    blockGraph match {
      case Errorable.Success(blockGraph) =>
        graph.setGraph(blockGraph)
        nameLabel.setText(EdgirUtils.SimpleLibraryPath(blockType))
      case Errorable.Error(errMsg) =>
        graph.setGraph(emptyHGraph)
        nameLabel.setText(s"Error loading ${EdgirUtils.SimpleLibraryPath(blockType)}: $errMsg")
    }
  }
}


class LibraryPanel(project: Project) extends JPanel {
  // State
  //
  private var library: wir.Library = EdgCompilerService(project).pyLib

  // GUI Components
  //
  private val splitter = new JBSplitter(false, 0.5f, 0.1f, 0.9f)

  private val preview = new LibraryPreview(project)
  splitter.setSecondComponent(preview)

  private val libraryTreePanel = new JPanel(new GridBagLayout())
  private val libraryTreeSearch = new JTextField()
  libraryTreePanel.add(libraryTreeSearch, Gbc(0, 0, GridBagConstraints.HORIZONTAL))
  private val libraryTreeStatus = new JLabel("All Library Elements")
  libraryTreePanel.add(libraryTreeStatus, Gbc(0, 1, GridBagConstraints.HORIZONTAL))

  private var libraryTreeModel = new FilteredTreeTableModel(new EdgirLibraryTreeTableModel(library))
  private val libraryTree = new TreeTable(libraryTreeModel)
  new TreeTableSpeedSearch(libraryTree)
  private val libraryTreeListener = new TreeSelectionListener {  // an object so it can be re-used since a model change wipes everything out
    override def valueChanged(e: TreeSelectionEvent): Unit = {
      e.getPath.getLastPathComponent match {
        case node: EdgirLibraryTreeNode.BlockNode =>
          preview.setBlock(library, node.path)
        case node =>
      }
    }
  }
  libraryTree.getTree.addTreeSelectionListener(libraryTreeListener)
  private val libraryMouseListener = new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = {
      val selectedTreePath = libraryTree.getTree.getPathForLocation(e.getX, e.getY)
      if (selectedTreePath == null) {
        return
      }
      val selectedPath = selectedTreePath.getLastPathComponent match {
        case selected: EdgirLibraryTreeNode.BlockNode => selected.path
        case _ => return  // any other type ignored
      }

      if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2) {
        // double click quick insert at caret
        exceptionPopup(e) {
          (new LibraryBlockPopupMenu(selectedPath, project).caretInsertAction.exceptError)()
        }
      } else if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) {
        // right click context menu
        new LibraryBlockPopupMenu(selectedPath, project).show(e.getComponent, e.getX, e.getY)
      }
    }
  }
  libraryTree.addMouseListener(libraryMouseListener)
  libraryTree.setRootVisible(false)
  libraryTree.setShowColumns(true)

  private val libraryTreeScrollPane = new JBScrollPane(libraryTree)
  libraryTreePanel.add(libraryTreeScrollPane, Gbc(0, 2, GridBagConstraints.BOTH))

  splitter.setFirstComponent(libraryTreePanel)

  def updateFilter(): Unit = {
    def recursiveExpandPath(path: TreePath): Unit = {
      if (path != null) {
        recursiveExpandPath(path.getParentPath)
        libraryTree.getTree.expandPath(path)
      }
    }

    val searchText = libraryTreeSearch.getText
    if (searchText.isEmpty) {
      libraryTreeStatus.setText("All Library Elements")
      libraryTreeModel.setFilter(_ => true)
    } else {
      libraryTreeStatus.setText(s"Filter By '$searchText'")
      val filteredPaths = libraryTreeModel.setFilter {
        case node: EdgirLibraryTreeNode.BlockNode =>
          EdgirUtils.SimpleLibraryPath(node.path).toLowerCase().contains(searchText.toLowerCase())
        case node: EdgirLibraryTreeNode.PortNode =>
          EdgirUtils.SimpleLibraryPath(node.path).toLowerCase().contains(searchText.toLowerCase())
        case other => false
      }
      filteredPaths.foreach { filteredPath =>
        recursiveExpandPath(filteredPath)
      }

    }
  }
  libraryTreeSearch.getDocument.addDocumentListener(new DocumentListener() {
    override def insertUpdate(e: DocumentEvent): Unit = update(e)
    override def removeUpdate(e: DocumentEvent): Unit = update(e)
    override def changedUpdate(e: DocumentEvent): Unit = update(e)

    def update(e: DocumentEvent): Unit = {
      updateFilter()
    }
  })

  setLayout(new BorderLayout())
  add(splitter)

  // Actions
  //
  def setLibrary(library: wir.Library): Unit = {
    this.library = library
    this.libraryTreeModel = new FilteredTreeTableModel(new EdgirLibraryTreeTableModel(this.library))
    libraryTree.setModel(this.libraryTreeModel)
    updateFilter()
    libraryTree.getTree.addTreeSelectionListener(libraryTreeListener)
    libraryTree.setRootVisible(false)  // this seems to get overridden when the model is updated
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
    state.panelLibrarySplitterPos = splitter.getProportion
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
    splitter.setProportion(state.panelLibrarySplitterPos)
  }
}
