package edg_ide.ui

import com.intellij.openapi.application.ReadAction
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.ThrowableComputable
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.ui.components.JBScrollPane
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.intellij.ui.{JBSplitter, TreeTableSpeedSearch}
import com.jetbrains.python.psi.types.TypeEvalContext
import com.jetbrains.python.psi.{PyClass, PyNamedParameter}
import edg.elem.elem
import edg.ref.ref
import edg.util.{Errorable, NameCreator}
import edg.wir
import edg.wir.DesignPath
import edg_ide.actions.{InsertAction, InsertBlockAction, InsertPortAction, InsertRefinementAction}
import edg_ide.edgir_graph._
import edg_ide.swing._
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption, ExceptSeq}
import edg_ide.util._
import edg_ide.{EdgirUtils, PsiUtils}
import edg.ExprBuilder.{Ref, ValueExpr}

import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.{BorderLayout, GridBagConstraints, GridBagLayout}
import javax.swing._
import javax.swing.event._
import javax.swing.tree.TreePath


class LibraryBlockPopupMenu(blockType: ref.LibraryPath, project: Project) extends JPopupMenu {
  val blockTypeName = EdgirUtils.SimpleLibraryPath(blockType)
  add(new JLabel(s"Library Block: $blockTypeName"))
  addSeparator()

  private val blockPyClass = DesignAnalysisUtils.pyClassOf(blockType, project)
  private val (contextPath, _) = BlockVisualizerService(project).getContextBlock.get
  private val contextPyClass = InsertAction.getPyClassOfContext(project)
  private val contextPyName = contextPyClass.mapToString(_.getName)

  // Edit actions
  private val caretPsiElement = exceptable {
    val contextPsiFile = contextPyClass.exceptError.getContainingFile.exceptNull("no file")
    InsertAction.getCaretAtFile(contextPsiFile, contextPyClass.exceptError, project).exceptError
  }
  private def insertContinuation(name: String, added: PsiElement): Unit = {
    InsertAction.navigateToEnd(added)

    val library = EdgCompilerService(project).pyLib
    val fastPathUtil = new DesignFastPathUtil(library)

    exceptionNotify("edg.ui.LibraryPanel", project) {
      val visualizerPanel = BlockVisualizerService(project).visualizerPanelOption
          .exceptNone("no visualizer panel")
      visualizerPanel.currentDesignModifyBlock(contextPath) { _.update(
        _.blocks :+= (name, fastPathUtil.instantiateStubBlockLike(blockType).exceptError)
      )}
      visualizerPanel.addStaleBlocks(Seq(contextPath + name))
    }
  }

  val caretInsertAction: Errorable[() => Unit] = exceptable {
    InsertBlockAction.createInsertBlockFlow(caretPsiElement.exceptError, blockPyClass.exceptError,
        s"Insert $blockTypeName at $contextPyName caret",
        project, insertContinuation).exceptError
  }
  private val caretFileLine = exceptable {
    caretInsertAction.exceptError
    PsiUtils.fileNextLineOf(caretPsiElement.exceptError, project).exceptError
  }.mapToStringOrElse(fileLine => s" ($fileLine)", err => "")

  private val caretInsertItem = ContextMenuUtils.MenuItemFromErrorable(
    caretInsertAction, s"Insert at $contextPyName caret$caretFileLine")
  add(caretInsertItem)

  private val insertionPairs = exceptable {
    InsertAction.findInsertionPoints(contextPyClass.exceptError, InsertBlockAction.VALID_FUNCTION_NAMES).exceptError
        .map { fn =>
          val fileLine = PsiUtils.fileNextLineOf(fn.getStatementList.getLastChild, project)
              .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
          val label = s"Insert at ${contextPyName}.${fn.getName}$fileLine"
          val action = InsertBlockAction.createInsertBlockFlow(fn.getStatementList.getStatements.last, blockPyClass.exceptError,
            s"Insert $blockTypeName at $contextPyName.${fn.getName}",
            project, insertContinuation)
          (label, action)
        } .collect {
          case (fn, Errorable.Success(action)) => (fn, action)
        }.exceptEmpty("no insertion points")
  }
  ContextMenuUtils.MenuItemsFromErrorableSeq(insertionPairs, s"Insert into $contextPyName")
      .foreach(add)
  addSeparator()

  // Refinements action
  private val selectedPathClass = exceptable {
    val visualizerPanel = BlockVisualizerService(project).visualizerPanelOption
        .exceptNone("no visualizer panel")
    val blockClass = blockPyClass.exceptError

    val selectedPath = visualizerPanel.getSelectedPath.exceptNone("no selection")
    val (resolvedPath, resolvedElt) = EdgirUtils.resolveDeepest(selectedPath, visualizerPanel.getDesign)
        .exceptNone(s"can't resolve $selectedPath")
    requireExcept(selectedPath == resolvedPath, s"mismatch resolving $selectedPath")
    val selectedBlock = resolvedElt.instanceOfExcept[elem.HierarchyBlock]("selected not a block")
    val selectedType = selectedBlock.prerefineClass.getOrElse(
      selectedBlock.superclasses.onlyExcept("invalid class of selected"))
    val selectedClass = DesignAnalysisUtils.pyClassOf(selectedType, project).exceptError

    requireExcept(blockClass.isSubclass(selectedClass, TypeEvalContext.codeCompletion(project, null)),
      s"${blockClass.getName} not a subtype of ${selectedClass.getName}")

    (selectedPath, selectedClass)
  }
  private val topClass = exceptable {
    val topType = BlockVisualizerService(project).visualizerPanelOption
        .exceptNone("no visualizer panel")
        .getDesign.getContents
        .superclasses.onlyExcept("invalid class of top")
    DesignAnalysisUtils.pyClassOf(topType, project).exceptError
  }
  private val insertInstanceRefinementAction: Errorable[() => Unit] = exceptable {
    val (selectedPath, selectedClass) = selectedPathClass.exceptError

    val insertAction = InsertRefinementAction.createInstanceRefinement(
      topClass.exceptError, selectedPath, blockPyClass.exceptError, project)
        .exceptError
    () => {  // TODO standardized continuation?
      val inserted = insertAction()
      InsertAction.navigateToEnd(inserted)
    }
  }
  private val selectedPathName = selectedPathClass.map(_._1.toString).toOption.getOrElse("selection")
  add(ContextMenuUtils.MenuItemFromErrorable(insertInstanceRefinementAction, s"Refine instance $selectedPathName to $blockTypeName"))

  private val insertClassRefinementAction: Errorable[() => Unit] = exceptable {
    val (selectedPath, selectedClass) = selectedPathClass.exceptError

    val insertAction = InsertRefinementAction.createClassRefinement(
      topClass.exceptError, selectedClass, blockPyClass.exceptError, project)
        .exceptError
    () => {  // TODO standardized continuation?
      val inserted = insertAction()
      InsertAction.navigateToEnd(inserted)
    }
  }
  private val selectedClassName = selectedPathClass.map(_._2.getName).toOption.getOrElse("of selection")
  add(ContextMenuUtils.MenuItemFromErrorable(insertClassRefinementAction, s"Refine class $selectedClassName to $blockTypeName"))
  addSeparator()

  // Navigation actions
  val gotoDefinitionAction: Errorable[() => Unit] = exceptable {
    requireExcept(blockPyClass.exceptError.canNavigateToSource, "class not navigatable")
    () => blockPyClass.exceptError.navigate(true)
  }
  private val blockFileLine = blockPyClass.flatMap(PsiUtils.fileLineOf(_, project))
      .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
  private val gotoDefinitionItem = ContextMenuUtils.MenuItemFromErrorable(gotoDefinitionAction,
    s"Goto Definition$blockFileLine")
  add(gotoDefinitionItem)
}


class LibraryPortPopupMenu(portType: ref.LibraryPath, project: Project) extends JPopupMenu {
  val portTypeName = EdgirUtils.SimpleLibraryPath(portType)
  add(new JLabel(s"Library Port: $portTypeName"))
  addSeparator()

  private val portPyClass = DesignAnalysisUtils.pyClassOf(portType, project)
  private val (contextPath, _) = BlockVisualizerService(project).getContextBlock.get
  private val contextPyClass = InsertAction.getPyClassOfContext(project)
  private val contextPyName = contextPyClass.mapToString(_.getName)

  // Edit actions
  private val caretPsiElement = exceptable {
    val contextPsiFile = contextPyClass.exceptError.getContainingFile.exceptNull("no file")
    InsertAction.getCaretAtFile(contextPsiFile, contextPyClass.exceptError, project).exceptError
  }
  private def insertContinuation(name: String, added: PsiElement): Unit = {
    InsertAction.navigateToEnd(added)

    val library = EdgCompilerService(project).pyLib
    val fastPathUtil = new DesignFastPathUtil(library)

    exceptionNotify("edg.ui.LibraryPanel", project) {
      val visualizerPanel = BlockVisualizerService(project).visualizerPanelOption
          .exceptNone("no visualizer panel")
      visualizerPanel.currentDesignModifyBlock(contextPath) { block =>
        val namer = NameCreator.fromBlock(block)
        block.update(
          _.ports :+= (name, fastPathUtil.instantiatePortLike(portType).exceptError),
          _.constraints :+= (namer.newName(s"_new_(reqd)$name"), ValueExpr.Ref(Ref.IsConnected(Ref(name)))),
      )}
      visualizerPanel.addStaleBlocks(Seq(contextPath))
    }
  }

  val caretInsertAction: Errorable[() => Unit] = exceptable {
    requireExcept(contextPath != DesignPath(), "can't insert port at design top")
    InsertPortAction.createInsertPortFlow(caretPsiElement.exceptError, portPyClass.exceptError,
      s"Insert $portTypeName at $contextPyName caret",
      project, insertContinuation).exceptError
  }
  private val caretFileLine = exceptable {
    caretInsertAction.exceptError
    PsiUtils.fileNextLineOf(caretPsiElement.exceptError, project).exceptError
  }.mapToStringOrElse(fileLine => s" ($fileLine)", err => "")

  private val caretInsertItem = ContextMenuUtils.MenuItemFromErrorable(
    caretInsertAction, s"Insert at $contextPyName caret$caretFileLine")
  add(caretInsertItem)

  private val insertionPairs = exceptable {
    requireExcept(contextPath != DesignPath(), "can't insert port at design top")
    InsertAction.findInsertionPoints(contextPyClass.exceptError, Seq(InsertPortAction.VALID_FUNCTION_NAME)).exceptError
        .map { fn =>
          val fileLine = PsiUtils.fileNextLineOf(fn.getStatementList.getLastChild, project)
              .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
          val label = s"Insert at ${contextPyName}.${fn.getName}$fileLine"
          val action = InsertPortAction.createInsertPortFlow(fn.getStatementList.getStatements.last, portPyClass.exceptError,
            s"Insert $portTypeName at $contextPyName.${fn.getName}",
            project, insertContinuation)
          (label, action)
        } .collect {
      case (fn, Errorable.Success(action)) => (fn, action)
    }.exceptEmpty("no insertion points")
  }
  ContextMenuUtils.MenuItemsFromErrorableSeq(insertionPairs, s"Insert into $contextPyName")
      .foreach(add)
  addSeparator()

  // Navigation actions
  val gotoDefinitionAction: Errorable[() => Unit] = exceptable {
    requireExcept(portPyClass.exceptError.canNavigateToSource, "class not navigatable")
    () => portPyClass.exceptError.navigate(true)
  }
  private val portFileLine = portPyClass.flatMap(PsiUtils.fileLineOf(_, project))
      .mapToStringOrElse(fileLine => s" ($fileLine)", err => "")
  private val gotoDefinitionItem = ContextMenuUtils.MenuItemFromErrorable(gotoDefinitionAction,
    s"Goto Definition$portFileLine")
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
    "")
  private val graph = new JElkGraph(emptyHGraph)
  splitter.setSecondComponent(graph)

  setLayout(new BorderLayout())
  add(splitter)

  // Actions
  //
  def setBlock(library: wir.Library, blockType: ref.LibraryPath): Unit = {
    val blockGraph = exceptable {
      val fastPath = new DesignFastPathUtil(library)
      val block = library.getBlock(blockType).exceptError
      val stubBlock = fastPath.instantiateStubBlock(blockType).exceptError
      val edgirGraph = EdgirGraph.blockToNode(DesignPath(), stubBlock)
      val transformedGraph = CollapseBridgeTransform(CollapseLinkTransform(
        InferEdgeDirectionTransform(SimplifyPortTransform(
          PruneDepthTransform(edgirGraph, 2)))))
      val blockGraph = HierarchyGraphElk.HGraphNodeToElk(transformedGraph,
        "",  // no name, the class is already shown as the class name
        Seq(ElkEdgirGraphUtils.PortSideMapper, ElkEdgirGraphUtils.PortConstraintMapper),
        true)  // need to make a root so root doesn't have ports
      (block, blockGraph)
    }

    val (callString, docstring) = ReadAction.compute(new ThrowableComputable[(Errorable[String], Errorable[String]), Throwable] {
      override def compute: (Errorable[String], Errorable[String]) = {
        val pyClassErrorable = DesignAnalysisUtils.pyClassOf(blockType, project)
        val callString = exceptable {
          val pyClass = pyClassErrorable.exceptError
          val (initArgs, initKwargs) = DesignAnalysisUtils.initParamsOf(pyClass, project).exceptError

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
        val docstring = exceptable {
          val pyClass = pyClassErrorable.exceptError
          Option(pyClass.getDocStringValue).getOrElse("")
        }

        (callString, docstring)
      }
    } )

    blockGraph match {
      case Errorable.Success((block, blockGraph)) =>
        graph.setGraph(blockGraph)
        val superclassString = if (block.superclasses.isEmpty) {
          "(none)"
        } else {
          block.superclasses.map { superclass =>
            // TODO hyperlinks? s"""<a href="lib:${superclass.getTarget.getName}">${EdgirUtils.SimpleLibraryPath(superclass)}</a>"""
            s"""<b>${EdgirUtils.SimpleLibraryPath(superclass)}</b>"""
          }.mkString(", ")
        }
        val textFieldText = s"<b>${EdgirUtils.SimpleLibraryPath(blockType)}</b> " +
            s"extends: ${superclassString}\n" +
            s"takes: ${callString.mapToString(identity)}<hr>" +
            docstring.mapToString(identity)

        textField.setText(SwingHtmlUtil.wrapInHtml(textFieldText,
          this.getFont))
      case Errorable.Error(errMsg) =>
        graph.setGraph(emptyHGraph)
        textField.setText(SwingHtmlUtil.wrapInHtml(s"${EdgirUtils.SimpleLibraryPath(blockType)}: $errMsg",
          this.getFont))
    }
  }

  def setPort(library: wir.Library, portType: ref.LibraryPath): Unit = {
    // TODO combine w/ setBlock
    val (callString, docstring) = ReadAction.compute(new ThrowableComputable[(Errorable[String], Errorable[String]), Throwable] {
      override def compute: (Errorable[String], Errorable[String]) = {
        val pyClassErrorable = DesignAnalysisUtils.pyClassOf(portType, project)
        val callString = exceptable {
          val pyClass = pyClassErrorable.exceptError
          val (initArgs, initKwargs) = DesignAnalysisUtils.initParamsOf(pyClass, project).exceptError

          def formatArg(arg: PyNamedParameter): String = {
            // TODO hyperlinks? s"""<a href="arg:${arg.getName}">${arg.getName}</a>"""
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
        val docstring = exceptable {
          val pyClass = pyClassErrorable.exceptError
          Option(pyClass.getDocStringValue).getOrElse("")
        }

        (callString, docstring)
      }
    } )

    graph.setGraph(emptyHGraph)
    val textFieldText = s"<b>${EdgirUtils.SimpleLibraryPath(portType)}</b>\n" +
        s"takes: ${callString.mapToString(identity)}<hr>" +
        docstring.mapToString(identity)

    textField.setText(SwingHtmlUtil.wrapInHtml(textFieldText,
      this.getFont))
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
        case node: EdgirLibraryTreeNode.PortNode =>
          preview.setPort(library, node.path)
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

      selectedTreePath.getLastPathComponent match {
        case selected: EdgirLibraryTreeNode.BlockNode =>  // insert actions / menu for blocks
          if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2) {
            // double click quick insert at caret
            exceptionPopup(e) {
              (new LibraryBlockPopupMenu(selected.path, project).caretInsertAction.exceptError)()
            }
          } else if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) {
            // right click context menu
            new LibraryBlockPopupMenu(selected.path, project).show(e.getComponent, e.getX, e.getY)
          }

        case selected: EdgirLibraryTreeNode.PortNode =>  // insert actions / menu for ports
          if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount == 2) {
            // double click quick insert at caret
            exceptionPopup(e) {
              (new LibraryPortPopupMenu(selected.path, project).caretInsertAction.exceptError)()
            }
          } else if (SwingUtilities.isRightMouseButton(e) && e.getClickCount == 1) {
            // right click context menu
            new LibraryPortPopupMenu(selected.path, project).show(e.getComponent, e.getX, e.getY)
          }

        case _ => return  // any other type ignored
      }


    }
  }
  libraryTree.addMouseListener(libraryMouseListener)
  libraryTree.setRootVisible(false)
  libraryTree.setShowColumns(true)
  private val libraryTreeRenderer = new EdgirLibraryTreeRenderer()
  libraryTree.setTreeCellRenderer(libraryTreeRenderer)

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
    libraryTree.setTreeCellRenderer(libraryTreeRenderer)
    libraryTree.setRootVisible(false)  // this seems to get overridden when the model is updated
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
