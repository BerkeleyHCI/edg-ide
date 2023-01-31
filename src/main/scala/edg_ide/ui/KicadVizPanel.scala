package edg_ide.ui

import com.intellij.notification.{NotificationGroup, NotificationType}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.ui.JBSplitter
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.jetbrains.python.psi.types.TypeEvalContext
import edg.ElemBuilder
import edg.compiler.{Compiler, ExprToString, TextValue}
import edg.util.Errorable
import edg.wir.DesignPath
import edg_ide.EdgirUtils
import edg_ide.psi_edits.{InsertAction, InsertFootprintAction, InsertPinningAction}
import edg_ide.swing._
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptNotify, ExceptOption, ExceptSeq}
import edg_ide.util.{DesignAnalysisUtils, exceptable, exceptionPopup, requireExcept}
import edgir.common.common
import edgir.elem.elem
import edgir.expr.expr
import edgir.ref.ref
import edgir.schema.schema

import java.awt.event._
import java.awt.{BorderLayout, GridBagConstraints, GridBagLayout}
import java.io.File
import javax.swing._
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.tree.TreePath


class KicadVizPanel(project: Project) extends JPanel with MouseWheelListener {
  val notificationGroup: NotificationGroup = NotificationGroup.balloonGroup("edg_ide.ui.KicadVizPanel")

  // State
  //
  var currentBlockPathTypePin: Option[(DesignPath, ref.LibraryPath, elem.HierarchyBlock, Map[String, ref.LocalPath])] = None  // should be a Block with a footprint and pinning field
  var footprintSynced: Boolean = false  // whether the footprint is assigned to the block (as opposed to peview mode)

  object FootprintBrowser extends JPanel {
    // TODO flatten out into parent? Or make this its own class with meaningful interfaces / abstractions?
    // TODO use GridBagLayout?

    var libraryDirectory: Option[File] = None  // TODO should be private / protected, but is in an object :s

    // use something invalid so it doesn't try to index a real directory
    val invalidModel = new FilteredTreeTableModel(new FootprintBrowserTreeTableModel(new File("doesnt_exist")))
    private var model = invalidModel
    private val tree = new TreeTable(model)
    tree.setShowColumns(true)
    tree.setRootVisible(false)
    private val treeScrollPane = new JScrollPane(tree)

    // initialize the contents on startup
    setLibraryDirectories(EdgSettingsState.getInstance().kicadDirectories)

    def setLibraryDirectories(directories: Seq[String]): Unit = {
      // TODO use File instead of String
      val directoryFile = new File(directory)
      if (directoryFile.exists()) {
        libraryDirectory = Some(directoryFile)
        model = new FilteredTreeTableModel(new FootprintBrowserTreeTableModel(directoryFile))
      } else {
        libraryDirectory = None
        model = invalidModel
        notificationGroup.createNotification(
          s"Invalid KiCad Directory",
          s"$directory is not a directory",
          NotificationType.ERROR
        ).notify(project)
      }
      TreeTableUtils.updateModel(tree, model)
    }

    def pathToFootprintName(file: File): Option[String] = {
      Option(file.getParentFile).flatMap { parentFile =>
        (parentFile.getName, file.getName) match {
          case (s"$parentFileName.pretty", s"$fileName.kicad_mod") => Some(s"$parentFileName:$fileName")
          case _ => None
        }
      }
    }

    def footprintToFile(footprint: String): Option[File] = {
      footprint match {
        case s"$libraryName:$footprintName" =>
          val footprintFile = libraryDirectory.map(new File(_, libraryName + ".pretty"))
              .map(new File(_, footprintName + ".kicad_mod"))
          footprintFile match {
            case Some(footprintFile) if footprintFile.exists() =>
              Some(footprintFile)
            case _ => None
          }
        case _ => None
      }
    }

    tree.addMouseListener(new MouseListener {
      override def mouseClicked(mouseEvent: MouseEvent): Unit = {
        val node:FootprintBrowserNode = tree.getTree.getSelectionPath.getLastPathComponent.asInstanceOf[FootprintBrowserNode]
        if (mouseEvent.getClickCount == 1) {
          // single click opens the footprint for preview
          footprintSynced = false
          visualizer.pinmap = Map()

          // TODO also pre-check parse legality here?
          pathToFootprintName(node.file) match {
            case Some(footprintName) =>
              visualizer.kicadFootprint = KicadParser.parseKicadFile(node.file)
              visualizer.repaint()
              val footprintStr = footprintName.replace(":", ": ")  // TODO this allows a line break but is hacky
              status.setText(SwingHtmlUtil.wrapInHtml(s"Footprint preview: ${footprintStr}",
                KicadVizPanel.this.getFont))
            case _ =>
              status.setText(SwingHtmlUtil.wrapInHtml(s"Invalid file: ${node.file.getName}",
                KicadVizPanel.this.getFont))
          }
        } else if (mouseEvent.getClickCount == 2) {
          // double click assigns the footprint to the opened block
          exceptionPopup(mouseEvent) {
            val footprint = pathToFootprintName(node.file).exceptNone(s"invalid file ${node.file.getName}")
            (insertBlockFootprint(footprint).exceptError)()
          }
        }
      }

      override def mousePressed(mouseEvent: MouseEvent): Unit = {}
      override def mouseReleased(mouseEvent: MouseEvent): Unit = {}
      override def mouseEntered(mouseEvent: MouseEvent): Unit = {}
      override def mouseExited(mouseEvent: MouseEvent): Unit = {}
    })

    // Filter menu
    def updateFilter(): Unit = {  // TODO DEDUP w/ LibraryPanel - perhaps we should have a FilteredTreePanel or something?
      // TODO spinny working indicator, incremental adding
      def recursiveExpandPath(path: TreePath): Unit = {
        if (path != null) {
          recursiveExpandPath(path.getParentPath)
          tree.getTree.expandPath(path)
        }
      }

      val searchTerms = filterTextBox.getText.split(' ')
          .filterNot(_.isEmpty)
          .map(_.toLowerCase())
      if (searchTerms.isEmpty) {
        model.setFilter(_ => true)
      } else {
        val filteredPaths = model.setFilter {
          case node: FootprintBrowserNode =>
            searchTerms.forall(searchTerm => node.toString.toLowerCase().contains(searchTerm))
          case other => false
        }
        filteredPaths.foreach { filteredPath =>
          recursiveExpandPath(filteredPath)
        }

      }
    }

    private val filterTextBox = new JTextField()
    private val filterLabel = new JLabel("Filter")
    filterTextBox.getDocument.addDocumentListener(new DocumentListener {
      override def insertUpdate(e: DocumentEvent): Unit = update(e)
      override def removeUpdate(e: DocumentEvent): Unit = update(e)
      override def changedUpdate(e: DocumentEvent): Unit = update(e)

      def update(e: DocumentEvent): Unit = {
        updateFilter()
      }
    })

    setLayout(new GridBagLayout)

    add(filterLabel, Gbc(0, 0, GridBagConstraints.NONE))
    add(filterTextBox, Gbc(1, 0, GridBagConstraints.HORIZONTAL))

    add(treeScrollPane, Gbc(0, 1, GridBagConstraints.BOTH, xsize = 2))
  }

  // GUI Components
  //
  private val splitter = new JBSplitter(false, 0.5f, 0.1f, 0.9f)

  splitter.setSecondComponent(FootprintBrowser)

  private val status = new JEditorPane("text/html",
      SwingHtmlUtil.wrapInHtml("No footprint selected", this.getFont))
  status.setEditable(false)
  status.setBackground(null)
  private val visualizer = new KicadVizDrawPanel()
  visualizer.addMouseListener(new MouseAdapter {
    override def mouseClicked(e: MouseEvent): Unit = {
      if (e.getClickCount == 2) {
        exceptionPopup(e) {
          requireExcept(footprintSynced, "footprint not synced")
          val selectedComp = visualizer.getComponentForLocation(e.getX, e.getY)
              .onlyExcept("must select exactly one pad")
          val selectedPin = selectedComp.instanceOfExcept[Rectangle]("selected not a pad").name
          val (blockPath, blockType, block, pinning) = currentBlockPathTypePin.exceptNone("no FootprintBlock selected")

          def continuation(portPath: ref.LocalPath, added: PsiElement): Unit = {
            InsertAction.navigateToEnd(added)

            val newPinning = pinning.updated(selectedPin, portPath)
            currentBlockPathTypePin = Some((blockPath, blockType, block, newPinning))
            visualizer.pinmap = newPinning.view.mapValues(ExprToString(_)).toMap  // TODO dedup
            visualizer.repaint()

            // TODO actually modify the Design ... once a better API for that exists
          }

          InsertPinningAction.createInsertPinningFlow(block, selectedPin, pinning, e, project, continuation).exceptError
        }
      }
    }
  })

  private val visualizerPanel = new JPanel(new GridBagLayout())
  visualizerPanel.add(status, Gbc(0, 0, GridBagConstraints.HORIZONTAL))
  visualizerPanel.add(visualizer, Gbc(0, 1, GridBagConstraints.BOTH))
  splitter.setFirstComponent(visualizerPanel)


  setLayout(new BorderLayout())
  add(splitter)


  override def mouseWheelMoved(mouseWheelEvent: MouseWheelEvent): Unit = {
  }

  // Actions
  //
  def pinningFromBlock(block: elem.HierarchyBlock): Option[Map[String, ref.LocalPath]] = {
    // TODO better error handling, Option[ref.LocalPath]?
    // TODO move into EdgirUtils or something?
    block.meta.map(_.meta).collect {
      case common.Metadata.Meta.Members(members) => members.node.get("pinning")
    }.flatten.map(_.meta).collect {
      case common.Metadata.Meta.Members(members) =>
        members.node.map { case (pin, pinValue) =>
          pin -> pinValue.meta
        } .collect { case (pin, common.Metadata.Meta.BinLeaf(bin)) =>
          pin -> expr.ValueExpr.parseFrom(bin.toByteArray).expr
        } .collect { case (pin, expr.ValueExpr.Expr.Ref(ref)) =>
          pin -> ref
        }
    }
  }

  def footprintFromBlock(blockPath: DesignPath, block: elem.HierarchyBlock, compiler: Compiler):
      Option[(String, Map[String, ref.LocalPath])] = {
    compiler.getParamValue(blockPath.asIndirect + "fp_footprint").collect {
      case TextValue(value) =>
        (value, pinningFromBlock(block).getOrElse(Map()))
    }
  }

  /** Set the rendered block to a path in the design */
  def setBlock(blockPath: DesignPath, design: schema.Design, compiler: Compiler): Unit = {
    EdgirUtils.resolveExactBlock(blockPath, design).map { block =>
      (block, footprintFromBlock(blockPath, block, compiler))
    } match {
      case Some((block, Some((footprint, pinning)))) =>
        val footprintStr = footprint.replace(":", ": ")  // TODO this allows a line break but is hacky
        currentBlockPathTypePin = Some((blockPath, block.getSelfClass, block, pinning))
        footprintSynced = true
        // TODO the proper way might be to fix the stylesheet to allow linebreaks on these characters?
        FootprintBrowser.footprintToFile(footprint) match {
          case Some(footprintFile) =>
            visualizer.kicadFootprint = KicadParser.parseKicadFile(footprintFile)
            visualizer.pinmap = pinning.view.mapValues(ExprToString(_)).toMap
            visualizer.repaint()
            status.setText(SwingHtmlUtil.wrapInHtml(s"Footprint ${footprintStr} at ${blockPath.lastString}",
              this.getFont))

          case _ =>
            visualizer.pinmap = Map()
            status.setText(SwingHtmlUtil.wrapInHtml(s"Unknown footprint ${footprintStr} at ${blockPath.lastString}",
              this.getFont))
        }
      case Some((block, None)) =>
        currentBlockPathTypePin = Some((blockPath, block.getSelfClass, block, Map()))
        footprintSynced = false
        visualizer.pinmap = Map()
        status.setText(SwingHtmlUtil.wrapInHtml(s"No footprint at ${blockPath.lastString}",
          this.getFont))
      case None =>
        currentBlockPathTypePin = None
        footprintSynced = false
        visualizer.pinmap = Map()
        status.setText(SwingHtmlUtil.wrapInHtml(s"Not a block at ${blockPath.lastString}",
          this.getFont))
    }
  }

  /** Returns an action to insert code to set the current block's footprint to something.
    * FootprintBlock detection is done here, so code edit actions are as consistent as possible. */
  def insertBlockFootprint(footprintName: String): Errorable[() => Unit] = exceptable {
    val (blockPath, blockType, block, pinning) = currentBlockPathTypePin.exceptNone("no FootprintBlock selected")
    val blockPyClass = DesignAnalysisUtils.pyClassOf(blockType, project).exceptError

    val footprintFile = FootprintBrowser.footprintToFile(footprintName)
        .exceptNone(s"unknown footprint $footprintName")
    val footprintBlockType = ElemBuilder.LibraryPath("electronics_model.CircuitBlock.FootprintBlock")  // TODO belongs in shared place?
    val footprintBlockClass = DesignAnalysisUtils.pyClassOf(footprintBlockType, project).get
    requireExcept(blockPyClass.isSubclass(footprintBlockClass, TypeEvalContext.codeAnalysis(project, null)),
      s"${blockPyClass.getName} not a FootprintBlock")

    def continuation(added: PsiElement): Unit = {
      InsertAction.navigateToEnd(added)
      visualizer.kicadFootprint = KicadParser.parseKicadFile(footprintFile)
      visualizer.pinmap = pinning.view.mapValues(ExprToString(_)).toMap  // TODO dedup
      footprintSynced = true
      visualizer.repaint()

      // TODO update in design itself - but this requires changing Compiler params which isn't well supported
    }

    InsertFootprintAction.createInsertFootprintFlow(blockPyClass, footprintName,
      project, continuation).exceptError
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = { }  // none currently

  def loadState(state: BlockVisualizerServiceState): Unit = { }  // none currently
}
