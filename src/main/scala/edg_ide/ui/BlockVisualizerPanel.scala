package edg_ide.ui

import com.intellij.openapi.application.{ApplicationManager, ModalityState, ReadAction}
import com.intellij.openapi.project.Project
import com.intellij.openapi.wm.ToolWindow
import com.intellij.ui.components.{JBScrollPane, JBTabbedPane}
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.intellij.ui.{JBIntSpinner, JBSplitter, TreeTableSpeedSearch}
import com.intellij.util.concurrency.AppExecutorUtil
import edg.EdgirUtils.SimpleLibraryPath
import edg.ElemModifier
import edg.compiler.{Compiler, CompilerError, DesignMap, PythonInterfaceLibrary}
import edg.wir.{DesignPath, Library}
import edg_ide.EdgirUtils
import edg_ide.build.BuildInfo
import edg_ide.edgir_graph._
import edg_ide.swing._
import edg_ide.swing.blocks.JBlockDiagramVisualizer
import edg_ide.ui.tools.{BaseTool, DefaultTool, ToolInterface}
import edg_ide.util.{DesignFindBlockOfTypes, DesignFindDisconnected}
import edgir.elem.elem
import edgir.ref.ref
import edgir.schema.schema
import edgir.schema.schema.Design
import edgrpc.hdl.{hdl => edgrpc}
import org.eclipse.elk.graph.{ElkGraphElement, ElkNode}

import java.awt.datatransfer.DataFlavor
import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.{BorderLayout, GridBagConstraints, GridBagLayout}
import java.io.{File, FileInputStream}
import java.util.concurrent.{Callable, TimeUnit}
import javax.swing.event.{ChangeEvent, ChangeListener, TreeSelectionEvent, TreeSelectionListener}
import javax.swing.tree.TreePath
import javax.swing.{JLabel, JPanel, TransferHandler}
import scala.collection.{SeqMap, mutable}
import scala.jdk.CollectionConverters.ListHasAsScala
import scala.util.Using


object Gbc {
  def apply(gridx: Int, gridy: Int, fill: Int = GridBagConstraints.NONE,
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
}


class BlockVisualizerPanel(val project: Project, toolWindow: ToolWindow) extends JPanel {
  // Internal State
  //
  private var design = schema.Design()
  private var refinements = edgrpc.Refinements()
  private var compiler = new Compiler(design, EdgCompilerService(project).pyLib)

  // Shared state, access should be synchronized on the variable
  private val staleTypes = mutable.Set[ref.LibraryPath]()
  private val stalePaths = mutable.Set[DesignPath]()

  // GUI-facing state
  //
  private var focusPath: DesignPath = DesignPath()  // visualize from root by default
  private var ignoreSelect: Boolean = false  // ignore select operation to prevent infinite recursion
  private var selectionPath: DesignPath = DesignPath()  // selection of detail view and graph selection

  // Tool
  //
  private def pathsToGraphNodes(paths: Set[DesignPath]): Set[ElkGraphElement] = {
    paths.flatMap { path =>
      ElkEdgirGraphUtils.follow(path, graph.getGraph)
    }
  }

  private val toolInterface = new ToolInterface {
    override def scrollGraphToVisible(path: DesignPath): Unit = {
      // TODO IMPLEMENT ME
    }

    override def setGraphSelections(paths: Set[DesignPath]): Unit = {
      graph.setSelected(pathsToGraphNodes(paths))
    }

    override def setGraphHighlights(paths: Option[Set[DesignPath]]): Unit = {
      paths match {
        case Some(paths) => graph.setHighlighted(Some(pathsToGraphNodes(paths)))
        case None => graph.setHighlighted(None)
      }
    }

    override def setFocus(path: DesignPath): Unit = {
      setContext(path)
    }

    override def setSelection(path: DesignPath): Unit = {
      BlockVisualizerPanel.this.selectPath(path)
    }

    override def setStatus(statusText: String): Unit = {
      status.setText(statusText)
    }

    override def getFocus: DesignPath = focusPath
    override def getProject: Project = project
    override def getLibrary: Library = EdgCompilerService(project).pyLib
    override def getDesign: Design = design

    override def startNewTool(tool: BaseTool): Unit = {
      activeTool = tool
      activeTool.init()
    }
    override def endTool(): Unit = {
      activeTool = defaultTool
      activeTool.init()
      setStatus("Ready")
    }
  }

  private val defaultTool: DefaultTool = new DefaultTool(toolInterface)
  private var activeTool: BaseTool = defaultTool

  // Internal development features / tools
  //
  // DnD to allow loading and visualizing a .edg file (Design protobuf)
  // this isn't (yet?) meant to be a proper user-facing feature so this isn't really discoverable
  this.setTransferHandler(new TransferHandler() {
    override def canImport(info: TransferHandler.TransferSupport): Boolean = {
      info.isDataFlavorSupported(DataFlavor.javaFileListFlavor)
    }

    override def importData(info: TransferHandler.TransferSupport): Boolean = {
      if (!info.isDrop || !info.isDataFlavorSupported(DataFlavor.javaFileListFlavor)) {
        return false
      }
      val data = info.getTransferable.getTransferData(DataFlavor.javaFileListFlavor)
          .asInstanceOf[java.util.List[File]].asScala.toSeq
      val file = data match {
        case Seq(file) => file
        case _ =>
          PopupUtils.createErrorPopupAtMouse("can open only one file", BlockVisualizerPanel.this)
          return false
      }
      val design = Using(new FileInputStream(file)) { fileInputStream =>
        schema.Design.parseFrom(fileInputStream)
      }.recover { exc =>
        PopupUtils.createErrorPopupAtMouse(s"failed opening file: $exc", BlockVisualizerPanel.this)
        return false
      }.get
      if (design.contents.isEmpty) {
        PopupUtils.createErrorPopupAtMouse(s"file does not contain a design", BlockVisualizerPanel.this)
        return false
      }

      // TODO show solved values and refinements
      val dummyCompiler = new Compiler(schema.Design(), EdgCompilerService(project).pyLib)
      setDesignTop(design, dummyCompiler, edgrpc.Refinements(), Seq(), Some(f"${file.getName}: "))

      true
    }
  })

  // GUI Components
  //
  private val mainSplitter = new JBSplitter(true, 0.5f, 0.1f, 0.9f)

  // GUI: Top half (status and block visualization)
  //
  private val visualizationPanel = new JPanel(new GridBagLayout())
  mainSplitter.setFirstComponent(visualizationPanel)

  private val status = new JLabel(s"Ready " +
      s"(version ${BuildInfo.version} built at ${BuildInfo.builtAtString}, " +
      s"scala ${BuildInfo.scalaVersion}, sbt ${BuildInfo.sbtVersion})"
  )
  visualizationPanel.add(status, Gbc(0, 0, GridBagConstraints.HORIZONTAL))

  // TODO max value based on depth of tree?
  private val depthSpinner = new JBIntSpinner(1, 1, 8)
  depthSpinner.addChangeListener(new ChangeListener {
    override def stateChanged(e: ChangeEvent): Unit = {
      updateDisplay()
    }
  })
  // TODO update visualization on change?
  visualizationPanel.add(depthSpinner, Gbc(2, 0))

  // TODO remove library requirement
  private val emptyHGraph = HierarchyGraphElk.HGraphNodeToElk(
    EdgirGraph.blockToNode(DesignPath(), elem.HierarchyBlock()), "empty")

  private val graph = new JBlockDiagramVisualizer(emptyHGraph)
  graph.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      graph.getElementForLocation(e.getX, e.getY) match {
        case Some(clicked) => clicked.getProperty(ElkEdgirGraphUtils.DesignPathMapper.property) match {
          case path: DesignPath => activeTool.onPathMouse(e, path)
          case null =>  // TODO should this error out?
        }
        case None =>  // ignored
      }
    }
  })

  private val centeringGraph = new JPanel(new GridBagLayout)
  centeringGraph.add(graph, new GridBagConstraints())

  private val graphScrollPane = new JBScrollPane(centeringGraph) with ZoomDragScrollPanel {
    val zoomable = graph
  }
  graph.addMouseListener(graphScrollPane.makeMouseAdapter)
  graph.addMouseMotionListener(graphScrollPane.makeMouseAdapter)
  visualizationPanel.add(graphScrollPane, Gbc(0, 1, GridBagConstraints.BOTH, xsize=3))

  // GUI: Bottom half (design tree and task tabs)
  //
  private val dseSplitter = new JBSplitter(true, 0.66f, 0.1f, 0.9f)
  private val bottomSplitter = new JBSplitter(false, 0.33f, 0.1f, 0.9f)
  mainSplitter.setSecondComponent(bottomSplitter)

  // Regularly check the selected run config and show the DSE panel if a DSE config is selected
  private var dsePanelShown = false
  AppExecutorUtil.getAppScheduledExecutorService.scheduleWithFixedDelay(() => {
    val dseConfigSelected = DseService(project).getRunConfiguration.isDefined
    if (dsePanelShown != dseConfigSelected) {
      dsePanelShown = dseConfigSelected  // set it now, so we don't get multiple invocations of the update
      ApplicationManager.getApplication.invokeLater(() => {
        if (dseConfigSelected) {
          mainSplitter.setSecondComponent(dseSplitter)
          dseSplitter.setFirstComponent(bottomSplitter)
        } else {
          dseSplitter.setFirstComponent(null)
          mainSplitter.setSecondComponent(bottomSplitter)
        }
      })
    }
  }, 333, 333, TimeUnit.MILLISECONDS) // seems flakey without initial delay

  private var designTreeModel = new BlockTreeTableModel(project, edgir.elem.elem.HierarchyBlock())
  private val designTree = new TreeTable(designTreeModel) with ProvenTreeTableMixin
  new TreeTableSpeedSearch(designTree)
  private val designTreeListener = new TreeSelectionListener {  // an object so it can be re-used since a model change wipes everything out
    override def valueChanged(e: TreeSelectionEvent): Unit = {
      import edg_ide.swing.HierarchyBlockNode
      e.getPath.getLastPathComponent match {
        case selectedNode: HierarchyBlockNode => selectPath(selectedNode.path)
        case _ =>  // any other type ignored, not that there should be any other types
      }
    }
  }
  designTree.getTree.addTreeSelectionListener(designTreeListener)
  designTree.addMouseListener(new MouseAdapter {  // right click context menu
    override def mousePressed(e: MouseEvent): Unit = {
      val selectedTreePath = TreeTableUtils.getPathForRowLocation(designTree, e.getX, e.getY).getOrElse(return)
      selectedTreePath.getLastPathComponent match {
        case clickedNode: HierarchyBlockNode => activeTool.onPathMouse(e, clickedNode.path)
        case _ =>  // any other type ignored
      }
    }
  })
  designTree.setShowColumns(true)

  private val designTreeTreeRenderer = designTree.getTree.getCellRenderer
  private val designTreeTableRenderer = designTree.getDefaultRenderer(classOf[Object])

  private val designTreeScrollPane = new JBScrollPane(designTree)
  bottomSplitter.setFirstComponent(designTreeScrollPane)

  // GUI: Task Tabs
  //
  private val tabbedPane = new JBTabbedPane()
  bottomSplitter.setSecondComponent(tabbedPane)

  private val libraryPanel = new LibraryPanel(project)
  tabbedPane.addTab("Library", libraryPanel)
  private val TAB_INDEX_LIBRARY = 0

  private val detailPanel = new DetailPanel(DesignPath(), compiler, project)
  tabbedPane.addTab("Detail", detailPanel)
  private val TAB_INDEX_DETAIL = 1

  private val errorPanel = new ErrorPanel(compiler)
  tabbedPane.addTab("Errors", errorPanel)
  private val TAB_INDEX_ERRORS = 2

  // add a tab for kicad visualization
  private val kicadVizPanel = new KicadVizPanel(project)
  tabbedPane.addTab("Kicad", kicadVizPanel)
  private val TAB_KICAD_VIZ = 3

  // GUI: Design Space Exploration (bottom tab)
  //
  private val dsePanel = new DsePanel(project)
  dseSplitter.setSecondComponent(dsePanel)

  setLayout(new BorderLayout())
  add(mainSplitter)

  // Actions
  //
  def getDsePanel: DsePanel = dsePanel

  def getContextBlock: Option[(DesignPath, elem.HierarchyBlock)] = {
    EdgirUtils.resolveExactBlock(focusPath, design).map((focusPath, _))
  }

  def getDesign: schema.Design = design

  def getSelectedPath: Option[DesignPath] = {
    Some(selectionPath)
  }

  def setContext(path: DesignPath): Unit = {
    if (activeTool == defaultTool) {
      focusPath = path
      updateDisplay()
    }
  }

  def selectPath(path: DesignPath): Unit = {
    if (ignoreSelect) {  // setting the tree selection triggers a select event, this prevents an infinite loop
      return
    }
    ignoreSelect = true
    selectionPath = path

    val (containingPath, containingBlock) = EdgirUtils.resolveDeepestBlock(path, design)

    designTree.clearSelection()
    val (targetDesignPrefix, targetDesignNode) = BlockTreeTableModel.follow(path, designTreeModel)
    val treePath = targetDesignPrefix.tail.foldLeft(new TreePath(targetDesignPrefix.head)) {
      _.pathByAddingChild(_)
    }
    designTree.addSelectedPath(treePath)

    graph.setSelected(pathsToGraphNodes(Set(path)))

    BlockVisualizerPanel.this.setDetailView(containingPath)

    ignoreSelect = false
  }

  def setDetailView(path: DesignPath): Unit = {
    tabbedPane.setTitleAt(TAB_INDEX_DETAIL, s"Detail (${path.lastString})")
    detailPanel.setLoaded(path, design, refinements, compiler)
    kicadVizPanel.setBlock(path, design, compiler)
  }

  /** Sets the design and updates displays accordingly.
    */
  def setDesignTop(design: schema.Design, compiler: Compiler, refinements: edgrpc.Refinements,
                   errors: Seq[CompilerError], namePrefix: Option[String] = None): Unit = {
    this.refinements = refinements  // must be updated before updateDisplay called in setDesign
    setDesign(design, compiler)
    tabbedPane.setTitleAt(TAB_INDEX_ERRORS, s"Errors (${errors.length})")
    errorPanel.setErrors(errors, compiler)

    ApplicationManager.getApplication.invokeLater(() => {
      toolWindow.setTitle(namePrefix.getOrElse("") + design.getContents.getSelfClass.toSimpleString)
    })

    if (activeTool != defaultTool) { // revert to the default tool
      toolInterface.endTool() // TODO should we also preserve state like selected?
    }
  }

  /** Updates the design tree only, where the overall "top design" does not change.
    * Mainly used for speculative updates on graphical edit actions.
    */
  def setDesign(design: schema.Design, compiler: Compiler): Unit = {
    // Update state
    this.design = design
    this.compiler = compiler

    // Update the design tree first, in case graph layout fails
    designTreeModel = new BlockTreeTableModel(project, design.contents.getOrElse(elem.HierarchyBlock()))
    TreeTableUtils.updateModel(designTree, designTreeModel)
    designTree.getTree.addTreeSelectionListener(designTreeListener)  // this seems to get overridden when the model is updated
    designTree.setTreeCellRenderer(designTreeTreeRenderer)
    designTree.setDefaultRenderer(classOf[Object], designTreeTableRenderer)

    // Also update the active detail panel
    selectPath(selectionPath)
    detailPanel.setStale(false)

    updateDisplay()
  }

  /** Sets the entire design as stale, eg if a recompile is running. Cleared with any variation of setDesign.
    */
  def setDesignStale(): Unit = {
    designTree.setTreeCellRenderer(new StaleTreeRenderer)
    designTree.setDefaultRenderer(classOf[Object], new StaleTableRenderer)
    errorPanel.setStale()
    detailPanel.setStale(true)
  }

  /** Updates the visualizations / trees / other displays, without recompiling or changing (explicit) state.
    * Does not update visualizations that are unaffected by operations that don't change the design.
    */
  def updateDisplay(): Unit = {
    val currentFocusPath = focusPath
    val currentDesign = design
    val currentCompiler = compiler

    ReadAction.nonBlocking((() => { // analyses happen in the background to avoid slow ops in UI thread
      val (blockPath, block) = EdgirUtils.resolveDeepestBlock(currentFocusPath, currentDesign)
      val layoutGraphRoot = HierarchyGraphElk.HBlockToElkNode(
        block, blockPath, depthSpinner.getNumber,
        // note, adding port side constraints with hierarchy seems to break ELK
        Seq(new ElkEdgirGraphUtils.TitleMapper(currentCompiler),
          ElkEdgirGraphUtils.DesignPathMapper)
      )
      val tooltipTextMap = new DesignToolTipTextMap(currentCompiler)
      tooltipTextMap.map(currentDesign)

      (layoutGraphRoot, tooltipTextMap.getTextMap)
    }): Callable[(ElkNode, Map[DesignPath, String])])
        .finishOnUiThread(ModalityState.defaultModalityState(), { case (layoutGraphRoot, tooltipTextMap) =>
      graph.setGraph(layoutGraphRoot)

      tooltipTextMap.foreach { case (path, tooltipText) =>
        pathsToGraphNodes(Set(path)).foreach { node =>
          graph.setElementToolTip(node, SwingHtmlUtil.wrapInHtml(tooltipText, this.getFont))
        }
      }

      updateStale()
      updateDisconnected()
    }).submit(AppExecutorUtil.getAppExecutorService)
  }

  protected def updateDisconnected(): Unit = {
    val disconnectedPaths = DesignFindDisconnected.map(design)._1
    val disconnectedNodes = pathsToGraphNodes(disconnectedPaths.toSet)
    graph.setError(disconnectedNodes)
  }

  protected def updateStale(): Unit = {
    val staleTypesCopy = staleTypes.synchronized {
      staleTypes.toSet
    }
    val stalePathsCopy = stalePaths.synchronized {
      stalePaths.toSeq
    }

    val allStaleTypeBlocks = new DesignFindBlockOfTypes(staleTypesCopy).map(design).map {
      case (path, block) => path
    }

    val nodes = pathsToGraphNodes((allStaleTypeBlocks ++ stalePathsCopy).toSet)
    graph.setStale(nodes)
  }

  def addStaleBlocks(paths: Seq[DesignPath]): Unit = {
    stalePaths.synchronized {
      stalePaths.addAll(paths)
    }
    updateStale()
  }

  def addStaleTypes(types: Seq[ref.LibraryPath]): Unit = {
    staleTypes.synchronized {
      staleTypes.addAll(types)
    }
    updateStale()
  }

  def updateLibrary(library: PythonInterfaceLibrary): Unit = {
    libraryPanel.setLibrary(library)
    staleTypes.synchronized {  // assumed that upon recompiling everything is again up to date
      staleTypes.clear()
    }
    updateStale()
  }

  // In place design tree modifications
  //
  def currentDesignModifyBlock(blockPath: DesignPath)
                              (blockTransformFn: elem.HierarchyBlock => elem.HierarchyBlock): Unit = {
    val newDesign = ElemModifier.modifyBlock(blockPath, design)(blockTransformFn)
    setDesign(newDesign, compiler)
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
    state.depthSpinner = depthSpinner.getNumber
    state.panelMainSplitterPos = mainSplitter.getProportion
    state.panelBottomSplitterPos = bottomSplitter.getProportion
    state.panelTabIndex = tabbedPane.getSelectedIndex
    state.dseSplitterPos = dseSplitter.getProportion
    libraryPanel.saveState(state)
    detailPanel.saveState(state)
    errorPanel.saveState(state)
    kicadVizPanel.saveState(state)
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
    depthSpinner.setNumber(state.depthSpinner)
    mainSplitter.setProportion(state.panelMainSplitterPos)
    bottomSplitter.setProportion(state.panelBottomSplitterPos)
    tabbedPane.setSelectedIndex(state.panelTabIndex)
    dseSplitter.setProportion(state.dseSplitterPos)
    libraryPanel.loadState(state)
    detailPanel.loadState(state)
    errorPanel.loadState(state)
    kicadVizPanel.loadState(state)
  }
}


class DesignToolTipTextMap(compiler: Compiler) extends DesignMap[Unit, Unit, Unit] {
  // TODO this really doesn't belong in the IDE
  // Instead there should be a way to specify short descriptions in the HDL
  // Perhaps also short names

  private val textMap = mutable.Map[DesignPath, String]()

  def getTextMap: Map[DesignPath, String] = textMap.toMap

  override def mapPort(path: DesignPath, port: elem.Port): Unit = {
    val classString = port.getSelfClass.toSimpleString
    textMap.put(path, s"<b>$classString</b> at $path")
  }
  override def mapPortArray(path: DesignPath, port: elem.PortArray,
                   ports: SeqMap[String, Unit]): Unit = {
    val classString = s"Array[${port.getSelfClass.toSimpleString}]"
    textMap.put(path, s"<b>$classString</b> at $path")
  }
  override def mapBundle(path: DesignPath, port: elem.Bundle,
                ports: SeqMap[String, Unit]): Unit = {
    val classString = port.getSelfClass.toSimpleString
    textMap.put(path, s"<b>$classString</b> at $path")
  }
  override def mapPortLibrary(path: DesignPath, port: ref.LibraryPath): Unit = {
    val classString = s"Unelaborated ${port.toSimpleString}"
    textMap.put(path, s"<b>$classString</b> at $path")
  }

  private def makeDescriptionString(path: DesignPath, description: Seq[elem.StringDescriptionElement]) = {
    description.map {
      _.elementType match {
        case elem.StringDescriptionElement.ElementType.Param(value) =>
          compiler.getParamValue(path.asIndirect ++ value.path.get)
              .map(ParamToUnitsStringUtil.paramToUnitsString(_, value.unit))
              .getOrElse("unknown")
        case elem.StringDescriptionElement.ElementType.Text(value) =>
          value
        case elem.StringDescriptionElement.ElementType.Empty =>
          "ERROR"
      }
    }.mkString("")
  }

  override def mapBlock(path: DesignPath, block: elem.HierarchyBlock,
               ports: SeqMap[String, Unit], blocks: SeqMap[String, Unit],
               links: SeqMap[String, Unit]): Unit = {
    val classString = block.getSelfClass.toSimpleString
    val additionalDesc = makeDescriptionString(path, block.description)
    textMap.put(path, s"<b>$classString</b> at $path\n$additionalDesc")
  }
  override def mapBlockLibrary(path: DesignPath, block: ref.LibraryPath): Unit = {
    // does nothing
  }

  override def mapLink(path: DesignPath, link: elem.Link,
              ports: SeqMap[String, Unit], links: SeqMap[String, Unit]): Unit = {
    val classString = link.getSelfClass.toSimpleString
    val additionalDesc = makeDescriptionString(path, link.description)
    textMap.put(path, s"<b>$classString</b> at $path\n$additionalDesc")
  }
  override def mapLinkArray(path: DesignPath, link: elem.LinkArray,
                            ports: SeqMap[String, Unit], links: SeqMap[String, Unit]): Unit = {
    val classString = link.getSelfClass.toSimpleString
    textMap.put(path, s"<b>$classString[]</b> at $path")
  }
  override def mapLinkLibrary(path: DesignPath, link: ref.LibraryPath): Unit = {
    val classString = s"Unelaborated ${link.toSimpleString}"
    textMap.put(path, s"<b>$classString</b> at $path")
  }
}

class ErrorPanel(compiler: Compiler) extends JPanel {
  private val tree = new TreeTable(new CompilerErrorTreeTableModel(Seq(), compiler))
  private val customTableHeader = new CustomTooltipTableHeader(tree.getColumnModel())
  tree.setTableHeader(customTableHeader)
  tree.setShowColumns(true)
  tree.setRootVisible(false)
  private val treeScrollPane = new JBScrollPane(tree)
  private val treeTreeRenderer = tree.getTree.getCellRenderer
  private val treeTableRenderer = tree.getDefaultRenderer(classOf[Object])

  setLayout(new BorderLayout())
  add(treeScrollPane)

  // Actions
  //
  def setErrors(errs: Seq[CompilerError], compiler: Compiler): Unit = {
    TreeTableUtils.updateModel(tree, new CompilerErrorTreeTableModel(errs, compiler))
    tree.setTreeCellRenderer(treeTreeRenderer)
    tree.setDefaultRenderer(classOf[Object], treeTableRenderer)
  }

  def setStale(): Unit = {
    tree.setTreeCellRenderer(new StaleTreeRenderer)
    tree.setDefaultRenderer(classOf[Object], new StaleTableRenderer)
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
  }
}
