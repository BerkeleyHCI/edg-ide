package edg_ide.ui

import com.intellij.openapi.application.{ApplicationManager, ModalityState, ReadAction}
import com.intellij.openapi.project.Project
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiElement, PsiFile}
import com.intellij.ui.components.JBScrollPane
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.intellij.ui.{JBSplitter, TreeTableSpeedSearch}
import com.intellij.util.concurrency.AppExecutorUtil
import com.jetbrains.python.psi.types.TypeEvalContext
import com.jetbrains.python.psi.{PyClass, PyNamedParameter}
import edg.EdgirUtils.SimpleLibraryPath
import edg.ExprBuilder.{Ref, ValueExpr}
import edg.util.{Errorable, NameCreator}
import edg.wir
import edg.wir.ProtoUtil._
import edg.wir.{DesignPath, Refinements}
import edg_ide.edgir_graph._
import edg_ide.psi_edits._
import edg_ide.swing._
import edg_ide.swing.blocks.JBlockDiagramVisualizer
import edg_ide.util.ExceptionNotifyImplicits.{ExceptBoolean, ExceptErrorable, ExceptNotify, ExceptOption}
import edg_ide.util._
import edg_ide.{EdgirUtils, PsiUtils}
import edgir.elem.elem
import edgir.ref.ref
import org.eclipse.elk.graph.ElkNode

import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.{BorderLayout, GridBagConstraints, GridBagLayout}
import java.util.concurrent.Callable
import javax.swing._
import javax.swing.event._
import javax.swing.tree.TreePath

class LibraryBlockPopupMenu(blockType: ref.LibraryPath, project: Project) extends JPopupMenu {
  val blockTypeName = blockType.toSimpleString
  add(new JLabel(s"Library Block: $blockTypeName"))
  addSeparator()

  private val blockPyClassOpt = DesignAnalysisUtils.pyClassOf(blockType, project)
  private val (contextPath, _) = BlockVisualizerService(project).getContextBlock.get
  private val contextPyClassOpt = InsertAction.getPyClassOfContext(project)
  private val contextPyName = contextPyClassOpt.mapToString(_.getName)

  // Edit actions
  private def insertContinuation(name: String, added: PsiElement): Unit = {
    InsertAction.navigateToEnd(added)

    val library = EdgCompilerService(project).pyLib
    val fastPathUtil = new DesignFastPathUtil(library)

    exceptionNotify("edg.ui.LibraryPanel", project) {
      val visualizerPanel = BlockVisualizerService(project).visualizerPanelOption
        .exceptNone("no visualizer panel")
      visualizerPanel.currentDesignModifyBlock(contextPath) {
        _.update(
          _.blocks :+= (name, fastPathUtil.instantiateStubBlockLike(blockType).exceptError).toPb
        )
      }
      visualizerPanel.addStaleBlocks(Seq(contextPath + name))
    }
  }

  val insertAction: Errorable[() => Unit] = exceptable {
    val contextPyClass = contextPyClassOpt.exceptError
    val blockPyClass = blockPyClassOpt.exceptError
    EdgirUtils.isCategory(blockType).exceptTrue("can't insert category")
    () =>
      LiveTemplateInsertBlock
        .createTemplateBlock(
          contextPyClass,
          blockPyClass,
          s"Insert $blockTypeName",
          insertContinuation
        ).start(project).exceptError
  }
  add(ContextMenuUtils.MenuItemFromErrorable(insertAction, s"Insert into $contextPyName"))

  addSeparator()

  // Refinements action
  private val selectedPathLibraryClass = exceptable {
    val visualizerPanel = BlockVisualizerService(project).visualizerPanelOption
      .exceptNone("no visualizer panel")
    val blockPyClass = blockPyClassOpt.exceptError

    val selectedPath = visualizerPanel.getSelectedPath.exceptNone("no selection")
    val (resolvedPath, resolvedElt) = EdgirUtils
      .resolveDeepest(selectedPath, visualizerPanel.getDesign)
      .exceptNone(s"can't resolve $selectedPath")
    requireExcept(selectedPath == resolvedPath, s"mismatch resolving $selectedPath")
    val selectedBlock = resolvedElt.instanceOfExcept[elem.HierarchyBlock]("selected not a block")
    val selectedType = selectedBlock.prerefineClass.getOrElse(selectedBlock.getSelfClass)
    val selectedClass = DesignAnalysisUtils.pyClassOf(selectedType, project).exceptError

    requireExcept(
      blockPyClass.isSubclass(selectedClass, TypeEvalContext.codeCompletion(project, null)),
      s"${blockPyClass.getName} not a subtype of ${selectedClass.getName}"
    )

    (selectedPath, selectedType, selectedClass)
  }
  private val topClass = exceptable {
    val topType = BlockVisualizerService(project).visualizerPanelOption
      .exceptNone("no visualizer panel")
      .getDesign
      .getContents
      .getSelfClass
    DesignAnalysisUtils.pyClassOf(topType, project).exceptError
  }
  private val insertInstanceRefinementAction: Errorable[() => Unit] = exceptable {
    val (selectedPath, selectedLibrary, selectedClass) = selectedPathLibraryClass.exceptError
    val insertAction = new InsertRefinementAction(project, topClass.exceptError)
      .createInsertRefinements(
        Refinements(
          instanceRefinements = Map(selectedPath -> blockType)
        )
      )
      .exceptError
    () => { // TODO standardized continuation?
      val inserted = insertAction().head
      InsertAction.navigateToEnd(inserted)
    }
  }
  private val selectedPathName = selectedPathLibraryClass.map(_._1.toString).toOption.getOrElse("selection")
  add(
    ContextMenuUtils.MenuItemFromErrorable(
      insertInstanceRefinementAction,
      s"Refine instance $selectedPathName to $blockTypeName"
    )
  )

  private val insertClassRefinementAction: Errorable[() => Unit] = exceptable {
    val (selectedPath, selectedLibrary, selectedClass) = selectedPathLibraryClass.exceptError
    val insertAction = new InsertRefinementAction(project, topClass.exceptError)
      .createInsertRefinements(
        Refinements(
          classRefinements = Map(selectedLibrary -> blockType)
        )
      )
      .exceptError
    () => { // TODO standardized continuation?
      val inserted = insertAction().head
      InsertAction.navigateToEnd(inserted)
    }
  }
  private val selectedClassName =
    selectedPathLibraryClass.map(_._2.toSimpleString).toOption.getOrElse("of selection")
  add(
    ContextMenuUtils.MenuItemFromErrorable(
      insertClassRefinementAction,
      s"Refine class $selectedClassName to $blockTypeName"
    )
  )
  addSeparator()

  // Navigation actions
  val gotoDefinitionAction: Errorable[() => Unit] = exceptable {
    val blockPyClass = blockPyClassOpt.exceptError
    requireExcept(blockPyClass.canNavigateToSource, "class not navigatable")
    () => blockPyClass.navigate(true)
  }
  private val blockFileLine = blockPyClassOpt
    .flatMap(PsiUtils.fileLineOf(_, project))
    .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
  private val gotoDefinitionItem =
    ContextMenuUtils.MenuItemFromErrorable(gotoDefinitionAction, s"Goto Definition$blockFileLine")
  add(gotoDefinitionItem)
}

class LibraryPortPopupMenu(portType: ref.LibraryPath, project: Project) extends JPopupMenu {
  val portTypeName = portType.toSimpleString
  add(new JLabel(s"Library Port: $portTypeName"))
  addSeparator()

  private val portPyClass = DesignAnalysisUtils.pyClassOf(portType, project)

  // Navigation actions
  val gotoDefinitionAction: Errorable[() => Unit] = exceptable {
    requireExcept(portPyClass.exceptError.canNavigateToSource, "class not navigatable")
    () => portPyClass.exceptError.navigate(true)
  }
  private val portFileLine = portPyClass
    .flatMap(PsiUtils.fileLineOf(_, project))
    .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
  private val gotoDefinitionItem =
    ContextMenuUtils.MenuItemFromErrorable(gotoDefinitionAction, s"Goto Definition$portFileLine")
  add(gotoDefinitionItem)
}

class LibraryPreview(project: Project) extends JPanel {
  // State
  //

  // GUI Components
  //
  private val splitter = new JBSplitter(true, 0.5f, 0.1f, 0.9f)

  private val textField = new JEditorPane()
  textField.setContentType("text/html")
  textField.setEditable(false)
  textField.setBackground(null)
  textField.addHyperlinkListener(new HyperlinkListener {
    override def hyperlinkUpdate(e: HyperlinkEvent): Unit = {
      println(e.getDescription)
    }
  })
  private val textFieldScrollPane = new JBScrollPane(textField)
  textFieldScrollPane.setBorder(null)
  splitter.setFirstComponent(textFieldScrollPane)

  private val emptyHGraph = HierarchyGraphElk.HGraphNodeToElk(
    EdgirGraph.blockToNode(DesignPath(), elem.HierarchyBlock()),
    ""
  )
  private val graph = new JBlockDiagramVisualizer(emptyHGraph)
  splitter.setSecondComponent(graph)

  setLayout(new BorderLayout())
  add(splitter)

  // Actions
  //
  protected def paramsToString(initArgs: Seq[PyNamedParameter], initKwargs: Seq[PyNamedParameter]): String = {
    def formatArg(arg: PyNamedParameter): String = {
      val containingClass = PsiTreeUtil.getParentOfType(arg, classOf[PyClass])
      // TODO hyperlinks? s"""<a href="arg:${containingClass.getName}_${arg.getName}">${arg.getName}</a>"""
      s"""<b>${arg.getName}</b>"""
    }
    val initString = if (initArgs.nonEmpty) {
      Some(s"positional args: ${initArgs.map(formatArg).mkString(", ")}")
    } else {
      None
    }
    val initKwString = if (initKwargs.nonEmpty) {
      Some(s"keyword args: ${initKwargs.map(formatArg).mkString(", ")}")
    } else {
      None
    }
    if (initString.isEmpty && initKwString.isEmpty) {
      "(no args)"
    } else {
      Seq(initString, initKwString).flatten.mkString("; ")
    }
  }

  protected def superclassToString(superclasses: Seq[ref.LibraryPath]): String = {
    if (superclasses.isEmpty) {
      "(none)"
    } else {
      superclasses
        .map { superclass =>
          // TODO hyperlinks? s"""<a href="lib:${superclass.getTarget.getName}">${EdgirUtils.SimpleLibraryPath(superclass)}</a>"""
          s"""<b>${superclass.toSimpleString}</b>"""
        }
        .mkString(", ")
    }
  }

  def setBlock(library: wir.Library, blockType: ref.LibraryPath): Unit = {
    ReadAction
      .nonBlocking((() => {
        exceptable {
          val fastPath = new DesignFastPathUtil(library)
          val block = library.getBlock(blockType).exceptError
          val stubBlock = fastPath.instantiateStubBlock(blockType).exceptError
          val edgirGraph = EdgirGraph.blockToNode(DesignPath(), stubBlock)
          val transformedGraph = CollapseBridgeTransform(
            CollapseLinkTransform(
              InferEdgeDirectionTransform(
                SimplifyPortTransform(
                  PruneDepthTransform(edgirGraph, 2)
                )
              )
            )
          )
          val blockGraph = HierarchyGraphElk.HGraphNodeToElk(
            transformedGraph,
            "", // no name, the class is already shown as the class name
            Seq(ElkEdgirGraphUtils.SimplePortSideMapper, ElkEdgirGraphUtils.PortConstraintMapper),
            true
          ) // need to make a root so root doesn't have ports

          val pyClass = DesignAnalysisUtils.pyClassOf(blockType, project).exceptError
          val (initArgs, initKwargs) = DesignAnalysisUtils.initParamsOf(pyClass, project).exceptError

          val callString = paramsToString(initArgs, initKwargs)
          val docstring = Option(pyClass.getDocStringValue).getOrElse("")
          val superclassString = superclassToString(block.superclasses)

          val textFieldText = s"<b>${blockType.toSimpleString}</b> " +
            s"extends: $superclassString\n" +
            s"takes: $callString<hr>" +
            docstring

          (blockGraph, textFieldText)
        } match {
          case Errorable.Success(value) => value
          case Errorable.Error(errMsg) => (emptyHGraph, s"${blockType.toSimpleString}: $errMsg")
        }
      }): Callable[(ElkNode, String)])
      .finishOnUiThread(
        ModalityState.defaultModalityState(),
        { case (blockGraph, textFieldText) =>
          graph.setGraph(blockGraph)
          textField.setText(SwingHtmlUtil.wrapInHtml(textFieldText, this.getFont))
        }
      )
      .submit(AppExecutorUtil.getAppExecutorService)
  }

  def setPort(library: wir.Library, portType: ref.LibraryPath): Unit = {
    // TODO combine w/ setBlock
    ReadAction
      .nonBlocking((() => {
        exceptable {
          val pyClass = DesignAnalysisUtils.pyClassOf(portType, project).exceptError
          val (initArgs, initKwargs) = DesignAnalysisUtils.initParamsOf(pyClass, project).exceptError

          val callString = paramsToString(initArgs, initKwargs)
          val docstring = Option(pyClass.getDocStringValue).getOrElse("")

          s"<b>${portType.toSimpleString}</b>\n" +
            s"takes: $callString<hr>" +
            docstring
        } match {
          case Errorable.Success(value) => value
          case Errorable.Error(errMsg) => s"${portType.toSimpleString}: $errMsg"
        }
      }): Callable[String])
      .finishOnUiThread(
        ModalityState.defaultModalityState(),
        textFieldText => {
          graph.setGraph(emptyHGraph)
          textField.setText(SwingHtmlUtil.wrapInHtml(textFieldText, this.getFont))
        }
      )
      .submit(AppExecutorUtil.getAppExecutorService)
  }

  def clear(): Unit = {
    graph.setGraph(emptyHGraph)
    textField.setText(SwingHtmlUtil.wrapInHtml("", this.getFont))
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
    state.libraryPreviewSplitterPos = splitter.getProportion
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
    splitter.setProportion(state.libraryPreviewSplitterPos)
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

  private val libraryTreeSearchLabel = new JLabel("Filter")
  libraryTreePanel.add(libraryTreeSearchLabel, Gbc(0, 0, GridBagConstraints.NONE))

  private val libraryTreeSearch = new JTextField()
  libraryTreePanel.add(libraryTreeSearch, Gbc(1, 0, GridBagConstraints.HORIZONTAL))

  private var libraryTreeModel = new FilteredTreeTableModel(new EdgirLibraryTreeTableModel(project, library))
  private val libraryTree = new TreeTable(libraryTreeModel) with ProvenTreeTableMixin
  new TreeTableSpeedSearch(libraryTree)
  private val libraryTreeListener =
    new TreeSelectionListener { // an object so it can be re-used since a model change wipes everything out
      override def valueChanged(e: TreeSelectionEvent): Unit = {
        e.getPath.getLastPathComponent match {
          case node: EdgirLibraryNode#BlockNode =>
            preview.setBlock(library, node.path)
          case node: EdgirLibraryNode#PortNode =>
            preview.setPort(library, node.path)
          case node =>
            preview.clear()
        }
      }
    }
  libraryTree.getTree.addTreeSelectionListener(libraryTreeListener)
  libraryTree.getTree.expandPath( // expand the blocks node by default
    new TreePath(libraryTreeModel.getRootNode).pathByAddingChild(libraryTreeModel.getRootNode.children.head))

  private val libraryMouseListener = new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = {
      val selectedTreePath = TreeTableUtils
        .getPathForRowLocation(libraryTree, e.getX, e.getY)
        .getOrElse(
          return
        )
      selectedTreePath.getLastPathComponent match {
        case selected: EdgirLibraryNode#BlockNode => // insert actions / menu for blocks
          if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2) {
            // double click quick insert at caret
            exceptionPopup(e) {
              new LibraryBlockPopupMenu(selected.path, project).insertAction.exceptError()
            }
          } else if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) {
            // right click context menu
            new LibraryBlockPopupMenu(selected.path, project).show(e.getComponent, e.getX, e.getY)
          }

        case selected: EdgirLibraryNode#PortNode => // insert actions / menu for ports
          if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) {
            // right click context menu
            new LibraryPortPopupMenu(selected.path, project).show(e.getComponent, e.getX, e.getY)
          }

        case _ => // any other type ignored
      }
    }
  }
  libraryTree.addMouseListener(libraryMouseListener)
  libraryTree.setRootVisible(false)
  libraryTree.setShowColumns(true)
  libraryTree.setTreeCellRenderer(new EdgirLibraryTreeRenderer())

  private val libraryTreeScrollPane = new JBScrollPane(libraryTree)
  libraryTreePanel.add(libraryTreeScrollPane, Gbc(0, 1, GridBagConstraints.BOTH, xsize = 2))

  splitter.setFirstComponent(libraryTreePanel)

  def updateFilter(): Unit = { // TODO spinny working indicator, incremental adding
    def recursiveExpandPath(path: TreePath): Unit = {
      if (path != null) {
        recursiveExpandPath(path.getParentPath)
        libraryTree.getTree.expandPath(path)
      }
    }

    val searchTerms = libraryTreeSearch.getText
      .split(' ')
      .filterNot(_.isEmpty)
      .map(_.toLowerCase())
    if (searchTerms.isEmpty) {
      libraryTreeModel.setFilter(_ => true)
    } else {
      val filteredPaths = libraryTreeModel.setFilter {
        case node: EdgirLibraryNode#BlockNode =>
          searchTerms.forall(searchTerm => node.path.toSimpleString.toLowerCase().contains(searchTerm))
        case node: EdgirLibraryNode#PortNode =>
          searchTerms.forall(searchTerm => node.path.toSimpleString.toLowerCase().contains(searchTerm))
        case other => false
      }.filter { path =>
        !path.getPath.exists {
          case node: EdgirLibraryNode#BlockNode if EdgirUtils.isInternal(node.path) =>
            true // don't expand InternalBlock
          case _ => false
        }
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
    libraryTreeModel = new FilteredTreeTableModel(new EdgirLibraryTreeTableModel(project, this.library))
    ApplicationManager.getApplication.invokeLater(() => {
      TreeTableUtils.updateModel(libraryTree, libraryTreeModel)
      updateFilter()
      libraryTree.getTree.addTreeSelectionListener(libraryTreeListener)
      libraryTree.getTree.expandPath( // TODO - this is a hack because restoring prev expanded doesn't work
        new TreePath(libraryTreeModel.getRootNode)
          .pathByAddingChild(libraryTreeModel.getRootNode.children.head))
    })
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
    state.panelLibrarySplitterPos = splitter.getProportion
    preview.saveState(state)
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
    splitter.setProportion(state.panelLibrarySplitterPos)
    preview.loadState(state)
  }
}
