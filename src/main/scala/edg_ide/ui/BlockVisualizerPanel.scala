package edg_ide.ui

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.progress.{ProgressIndicator, ProgressManager, Task}
import com.intellij.openapi.project.Project
import com.intellij.ui.{JBIntSpinner, JBSplitter, TreeTableSpeedSearch}
import com.intellij.ui.components.{JBScrollPane, JBTabbedPane}
import com.intellij.ui.treeStructure.treetable.TreeTable
import edg.compiler.{Compiler, CompilerError, DesignStructuralValidate, PythonInterfaceLibrary, hdl => edgrpc}
import edg.elem.elem
import edg.schema.schema
import edg.ElemBuilder
import edg.schema.schema.Design
import edg_ide.edgir_graph.{CollapseBridgeTransform, CollapseLinkTransform, EdgirGraph, ElkEdgirGraphUtils, HierarchyGraphElk, InferEdgeDirectionTransform, PruneDepthTransform, SimplifyPortTransform}
import edg_ide.swing.{BlockTreeTableModel, CompilerErrorTreeTableModel, HierarchyBlockNode, JElkGraph, RefinementsTreeTableModel, ZoomingScrollPane}
import edg.wir.DesignPath
import edg_ide.EdgirUtils
import edg_ide.build.BuildInfo
import org.eclipse.elk.graph.ElkGraphElement

import java.awt.event.{ActionEvent, ActionListener, MouseAdapter, MouseEvent}
import java.awt.{BorderLayout, GridBagConstraints, GridBagLayout}
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.event.{ChangeEvent, ChangeListener, TreeSelectionEvent, TreeSelectionListener}
import javax.swing.tree.TreePath
import javax.swing.{JButton, JLabel, JPanel}


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


class BlockVisualizerPanel(val project: Project) extends JPanel {
  // Internal State
  //
  private var design = schema.Design()
  private var compiler = new Compiler(design, EdgCompilerService(project).pyLib)

  private var blockModule = ""
  private var blockName = ""

  // GUI-facing state
  //
  private var focusPath: DesignPath = DesignPath()  // visualize from root by default

  private val compilerRunning = new AtomicBoolean(false)

  // Tool
  //
  private val toolInterface = new ToolInterface {
    private def pathsToNodes(paths: Set[DesignPath]): Set[ElkGraphElement] = {
      paths.flatMap { path =>
        val (targetElkPrefix, targetElkNode) = ElkEdgirGraphUtils.follow(path, graph.getGraph)
        targetElkNode
      }
    }

    override def setDesignTreeSelection(path: Option[DesignPath]): Unit = {
      designTree.clearSelection()
      path match {
        case Some(path) =>
          val (targetDesignPrefix, targetDesignNode) = BlockTreeTableModel.follow(path, designTreeModel)
          val treePath = targetDesignPrefix.tail.foldLeft(new TreePath(targetDesignPrefix.head)) { _.pathByAddingChild(_) }
          designTree.addSelectedPath(treePath)
        case None =>  // do nothing
      }
    }

    override def setGraphSelections(paths: Set[DesignPath]): Unit = {
      graph.setSelected(pathsToNodes(paths))
    }

    override def setGraphHighlights(paths: Option[Set[DesignPath]]): Unit = {
      paths match {
        case Some(paths) => graph.setHighlighted(Some(pathsToNodes(paths)))
        case None => graph.setHighlighted(None)
      }
    }

    override def setFocus(path: DesignPath): Unit = {
      setContext(path)
    }

    override def setDetailView(path: DesignPath): Unit = {
      tabbedPane.setTitleAt(TAB_INDEX_DETAIL, s"Detail (${path.lastString})")
      detailPanel.setLoaded(path, design, compiler)
    }

    override def setStatus(statusText: String): Unit = {
      status.setText(statusText)
    }

    override def getFocus: DesignPath = focusPath
    override def getProject: Project = project
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

  private val defaultTool: BaseTool = new DefaultTool(toolInterface)
  private var activeTool: BaseTool = defaultTool


  // GUI Components
  //
  private val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.ui.BlockVisualizerPanel")

  private val mainSplitter = new JBSplitter(true, 0.5f, 0.1f, 0.9f)

  // GUI: Top half (status and block visualization)
  //
  private val visualizationPanel = new JPanel(new GridBagLayout())
  mainSplitter.setFirstComponent(visualizationPanel)

  private val blockNameLabel = new JLabel("")
  visualizationPanel.add(blockNameLabel, Gbc(0, 0, GridBagConstraints.HORIZONTAL))

  private val button = new JButton("Update")
  visualizationPanel.add(button, Gbc(1, 0))
  button.addActionListener(new ActionListener() {
    override def actionPerformed(e: ActionEvent) {
      recompile()
    }
  })

  // TODO max value based on depth of tree?
  private val depthSpinner = new JBIntSpinner(1, 1, 8)
  depthSpinner.addChangeListener(new ChangeListener {
    override def stateChanged(e: ChangeEvent): Unit = {
      updateDisplay()
    }
  })
  // TODO update visualization on change?
  visualizationPanel.add(depthSpinner, Gbc(2, 0))

  private val status = new JLabel(s"Ready " +
      s"(version ${BuildInfo.version} built at ${BuildInfo.builtAtString}, " +
      s"scala ${BuildInfo.scalaVersion}, sbt ${BuildInfo.sbtVersion})"
  )
  visualizationPanel.add(status, Gbc(0, 1, GridBagConstraints.HORIZONTAL, xsize=3))

  // TODO remove library requirement
  private val emptyHGraph = HierarchyGraphElk.HGraphNodeToElk(
    EdgirGraph.blockToNode(DesignPath(), elem.HierarchyBlock()))

  private val graph = new JElkGraph(emptyHGraph)
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

  private val graphScrollPane = new JBScrollPane(graph) with ZoomingScrollPane
  visualizationPanel.add(graphScrollPane, Gbc(0, 2, GridBagConstraints.BOTH, xsize=3))

  // GUI: Bottom half (design tree and task tabs)
  //
  private val bottomSplitter = new JBSplitter(false, 0.33f, 0.1f, 0.9f)
  mainSplitter.setSecondComponent(bottomSplitter)

  private var designTreeModel = new BlockTreeTableModel(edg.elem.elem.HierarchyBlock())
  private val designTree = new TreeTable(designTreeModel)
  new TreeTableSpeedSearch(designTree)
  private val designTreeListener = new TreeSelectionListener {  // an object so it can be re-used since a model change wipes everything out
    override def valueChanged(e: TreeSelectionEvent): Unit = {
      import edg_ide.swing.HierarchyBlockNode
      e.getPath.getLastPathComponent match {
        case selectedNode: HierarchyBlockNode =>
          activeTool.onSelect(selectedNode.path)
        case _ =>  // any other type ignored, not that there should be any other types
          // TODO should this error out?
      }
    }
  }
  designTree.getTree.addTreeSelectionListener(designTreeListener)
  designTree.addMouseListener(new MouseAdapter {  // right click context menu
    override def mousePressed(e: MouseEvent): Unit = {
      designTree.getTree.getPathForLocation(e.getX, e.getY) match {
        case null =>  // ignored
        case treePath: TreePath => treePath.getLastPathComponent match {
          case clickedNode: HierarchyBlockNode => activeTool.onPathMouse(e, clickedNode.path)
          case _ =>  // any other type ignored
        }
      }
    }
  })
  designTree.setShowColumns(true)
  private val designTreeScrollPane = new JBScrollPane(designTree)
  bottomSplitter.setFirstComponent(designTreeScrollPane)

  // GUI: Task Tabs
  //
  private val tabbedPane = new JBTabbedPane()
  bottomSplitter.setSecondComponent(tabbedPane)

  private val libraryPanel = new LibraryPanel(project)
  tabbedPane.addTab("Library", libraryPanel)
  val TAB_INDEX_LIBRARY = 0

  private val refinementsPanel = new RefinementsPanel()
  tabbedPane.addTab("Refinements", refinementsPanel)
  val TAB_INDEX_REFINEMENTS = 1

  private val detailPanel = new DetailPanel()
  tabbedPane.addTab("Detail", detailPanel)
  val TAB_INDEX_DETAIL = 2

  private val errorPanel = new ErrorPanel()
  tabbedPane.addTab("Errors", errorPanel)
  val TAB_INDEX_ERRORS = 3

  setLayout(new BorderLayout())
  add(mainSplitter)

  // Actions
  //
  def getModule: String = blockModule

  def getContextBlock: Option[(DesignPath, elem.HierarchyBlock)] = {
    EdgirUtils.resolveExactBlock(focusPath, design).map((focusPath, _))
  }

  def setContext(path: DesignPath): Unit = {
    focusPath = path
    updateDisplay()
  }

  def setFileBlock(module: String, block: String): Unit = {
    blockModule = module
    blockName = block
    blockNameLabel.setText(s"$module.$blockName")
  }

  /** Recompiles the current blockModule / blockName, and updates the display
    */
  def recompile(): Unit = {
    if (!compilerRunning.compareAndSet(false, true)) {
      notificationGroup.createNotification(
        s"Compiler already running", NotificationType.WARNING)
          .notify(project)
      return
    }

    ProgressManager.getInstance().run(new Task.Backgroundable(project, "EDG compiling") {
      override def run(indicator: ProgressIndicator): Unit = {
        status.setText(s"Compiling")
        indicator.setIndeterminate(true)

        try {
          indicator.setText("EDG compiling ... reloading")
          EdgCompilerService(project).pyLib.reloadModule(blockModule)

          indicator.setText("EDG compiling ... design top")
          val fullName = blockModule + "." + blockName
          val designPathTop = ElemBuilder.LibraryPath(fullName)
          val (block, refinements) = EdgCompilerService(project).pyLib.getDesignTop(designPathTop).get  // TODO propagate Errorable
          val design = schema.Design(contents = Some(block.copy(superclasses = Seq(designPathTop))))  // TODO dedup w/ superclass resolution in BlockLink.Block

          indicator.setText("EDG compiling ...")
          val (compiled, compiler, time) = EdgCompilerService(project).compile(design, refinements, Some(indicator))

          indicator.setText("EDG compiling ... validating")
          val checker = new DesignStructuralValidate()
          val errors = compiler.getErrors() ++ checker.map(compiled)
          if (errors.isEmpty) {
            status.setText(s"Compiled ($time ms)")
          } else {
            status.setText(s"Compiled, with ${errors.length} errors ($time ms)")
          }
          tabbedPane.setTitleAt(TAB_INDEX_ERRORS, s"Errors (${errors.length})")
          indicator.setText("EDG compiling ... done")

          updateLibrary(EdgCompilerService(project).pyLib)
          refinementsPanel.setRefinements(refinements)
          errorPanel.setErrors(errors)

          setDesign(compiled, compiler)
        } catch {
          case e: Throwable =>
            import java.io.PrintWriter
            import java.io.StringWriter
            val sw = new StringWriter
            e.printStackTrace(new PrintWriter(sw))
            status.setText(s"Compiler error: ${e.toString}")
            notificationGroup.createNotification(
              s"Compiler error", s"${e.toString}",
              s"${sw.toString}",
              NotificationType.WARNING)
                .notify(project)
          // TODO staleness indicator for graph / design tree
        }

        compilerRunning.set(false)
      }
    })

  }

  /** Sets the design and updates displays accordingly.
    */
  def setDesign(design: schema.Design, compiler: Compiler): Unit = {
    // Update state
    this.design = design
    this.compiler = compiler

    val block = design.contents.getOrElse(elem.HierarchyBlock())

    // Update the design tree first, in case graph layout fails
    designTreeModel = new BlockTreeTableModel(block)
    designTree.setModel(designTreeModel)
    designTree.getTree.addTreeSelectionListener(designTreeListener)  // this seems to get overridden when the model is updated

    updateDisplay()
  }

  /** Updates the visualizations / trees / other displays, without recompiling or changing (explicit) state.
    * Does not update visualizations that are unaffected by operations that don't change the design.
    */
  def updateDisplay(): Unit = {
    val (blockPath, block) = EdgirUtils.resolveDeepestBlock(focusPath, design)
    focusPath = blockPath

    // For now, this only updates the graph visualization, which can change with focus.
    // In the future, maybe this will also update or filter the design tree.
    val edgirGraph = EdgirGraph.blockToNode(focusPath, block)
    val transformedGraph = CollapseBridgeTransform(CollapseLinkTransform(
      InferEdgeDirectionTransform(SimplifyPortTransform(
        PruneDepthTransform(edgirGraph, depthSpinner.getNumber)))))  // TODO configurable depth
    val layoutGraphRoot = HierarchyGraphElk.HGraphNodeToElk(transformedGraph,
      Some(ElkEdgirGraphUtils.DesignPathMapper),
      focusPath != DesignPath())  // need to make a root so root doesn't have ports

    graph.setGraph(layoutGraphRoot)
    if (activeTool != defaultTool) {  // revert to the default tool
      toolInterface.endTool()  // TODO should we also preserve state like selected?
    }
  }


  def updateLibrary(library: PythonInterfaceLibrary): Unit = {
    libraryPanel.setLibrary(library)
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
    state.panelBlockModule = blockModule
    state.panelBlockName = blockName
    state.depthSpinner = depthSpinner.getNumber
    state.panelMainSplitterPos = mainSplitter.getProportion
    state.panelBottomSplitterPos = bottomSplitter.getProportion
    state.panelTabIndex = tabbedPane.getSelectedIndex
    libraryPanel.saveState(state)
    refinementsPanel.saveState(state)
    detailPanel.saveState(state)
    errorPanel.saveState(state)
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
    setFileBlock(state.panelBlockModule, state.panelBlockName)
    depthSpinner.setNumber(state.depthSpinner)
    mainSplitter.setProportion(state.panelMainSplitterPos)
    bottomSplitter.setProportion(state.panelBottomSplitterPos)
    tabbedPane.setSelectedIndex(state.panelTabIndex)
    libraryPanel.loadState(state)
    refinementsPanel.loadState(state)
    detailPanel.loadState(state)
    errorPanel.loadState(state)
  }
}


class RefinementsPanel extends JPanel {
  private val tree = new TreeTable(new RefinementsTreeTableModel(edgrpc.Refinements()))
  new TreeTableSpeedSearch(tree)
  tree.setShowColumns(true)
  tree.setRootVisible(false)
  private val treeScrollPane = new JBScrollPane(tree)

  setLayout(new BorderLayout())
  add(treeScrollPane)

  // Actions
  //
  def setRefinements(refinements: edgrpc.Refinements): Unit = {
    tree.setModel(new RefinementsTreeTableModel(refinements))
    tree.setRootVisible(false)
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
  }
}


class DetailPanel extends JPanel {
  import edg_ide.swing.ElementDetailTreeModel

  private val tree = new TreeTable(new BlockTreeTableModel(edg.elem.elem.HierarchyBlock()))
  tree.setShowColumns(true)
  private val treeScrollPane = new JBScrollPane(tree)

  setLayout(new BorderLayout())
  add(treeScrollPane)

  // Actions
  //
  def setLoaded(path: DesignPath, root: schema.Design, compiler: Compiler): Unit = {
    tree.setModel(new ElementDetailTreeModel(path, root, compiler))
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
  }
}


class ErrorPanel extends JPanel {
  private val tree = new TreeTable(new CompilerErrorTreeTableModel(Seq()))
  tree.setShowColumns(true)
  tree.setRootVisible(false)
  private val treeScrollPane = new JBScrollPane(tree)

  setLayout(new BorderLayout())
  add(treeScrollPane)

  // Actions
  //
  def setErrors(errs: Seq[CompilerError]): Unit = {
    tree.setModel(new CompilerErrorTreeTableModel(errs))
    tree.setRootVisible(false)
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
  }
}
