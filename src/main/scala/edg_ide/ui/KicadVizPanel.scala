package edg_ide.ui

import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.ui.JBSplitter
import com.intellij.ui.treeStructure.treetable.TreeTable
import com.jetbrains.python.psi.PyExpression
import com.jetbrains.python.psi.types.TypeEvalContext
import edg.ElemBuilder
import edg.elem.elem
import edg.schema.schema
import edg.ref.ref
import edg.common.common
import edg.expr.expr
import edg.wir.DesignPath
import edg_ide.swing.{FootprintBrowserNode, FootprintBrowserTreeTableModel, SwingHtmlUtil}
import edg.compiler.{Compiler, ExprToString, TextValue}
import edg.util.Errorable
import edg_ide.EdgirUtils
import edg_ide.psi_edits.{InsertAction, InsertFootprintAction}
import edg_ide.util.ExceptionNotifyImplicits.{ExceptErrorable, ExceptOption}
import edg_ide.util.{DesignAnalysisUtils, exceptable, exceptionPopup, requireExcept}

import java.awt.event.{MouseEvent, MouseListener, MouseWheelEvent, MouseWheelListener}
import java.awt.{BorderLayout, GridBagConstraints, GridBagLayout}
import java.io.File
import javax.swing._
import javax.swing.event.{DocumentEvent, DocumentListener}

class KicadVizPanel(project: Project) extends JPanel with MouseWheelListener {
  // State
  //
  var currentBlockPathTypePin: Option[(DesignPath, ref.LibraryPath, Map[String, ref.LocalPath])] = None  // should be a Block with a footprint and pinning field
  var footprintSynced: Boolean = false

  object FootprintBrowser extends JPanel {
    // TODO flatten out into parent? Or make this its own class with meaningful interfaces / abstractions?
    // TODO use GridBagLayout?

    var libraryDirectory: Option[File] = None  // TODO should be private / protected, but is in an object :s

    def setLibraryDirectory(directory: String): Unit = {
      // TODO use File instead of String
      val filterFunc = (x:String) => x.contains(filterTextBox.getText)
      val directoryFile = new File(directory)
      if (directoryFile.exists()) {
        libraryDirectory = Some(directoryFile)
        tree.setModel(new FootprintBrowserTreeTableModel(directoryFile, filterFunc))
      } else {
        libraryDirectory = None
        // TODO clear tree model?
      }
    }

    var model = new FootprintBrowserTreeTableModel(new File("."))
    private val tree = new TreeTable(model)
    tree.setShowColumns(true)
    tree.setRootVisible(false)
    private val treeScrollPane = new JScrollPane(tree)

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
              visualizer.kicadParser.setKicadFile(node.file)
              visualizer.repaint()
              status.setText(SwingHtmlUtil.wrapInHtml(s"Footprint preview: ${footprintName}",
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
    private val filterTextBox = new JTextField()
    private val filterLabel = new JLabel("Filter")
    filterTextBox.getDocument.addDocumentListener(new DocumentListener {
      override def insertUpdate(e: DocumentEvent): Unit = update(e)

      override def removeUpdate(e: DocumentEvent): Unit = update(e)

      override def changedUpdate(e: DocumentEvent): Unit = update(e)

      def update(e: DocumentEvent): Unit = {
        val filterFunc = (x:String) => x.toLowerCase().contains(filterTextBox.getText.toLowerCase())
        val oldFile = tree.getTableModel.asInstanceOf[FootprintBrowserTreeTableModel].getRootNode.file
        tree.setModel(new FootprintBrowserTreeTableModel(oldFile, filterFunc))
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
  visualizer.offset = (this.FootprintBrowser.getWidth * 1.2).asInstanceOf[Int] // @TODO clean this up with offset code

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
    compiler.getParamValue(blockPath.asIndirect + "footprint_name").collect {
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
        currentBlockPathTypePin = Some((blockPath, block.superclasses.head, pinning))
        footprintSynced = true
        // TODO the proper way might be to fix the stylesheet to allow linebreaks on these characters?
        FootprintBrowser.footprintToFile(footprint) match {
          case Some(footprintFile) =>
            visualizer.kicadParser.setKicadFile(footprintFile)
            visualizer.pinmap = pinning.mapValues(ExprToString(_)).toMap
            visualizer.repaint()
            status.setText(SwingHtmlUtil.wrapInHtml(s"Footprint ${footprintStr} at ${blockPath.lastString}",
              this.getFont))

          case _ =>
            visualizer.pinmap = Map()
            status.setText(SwingHtmlUtil.wrapInHtml(s"Unknown footprint ${footprintStr} at ${blockPath.lastString}",
              this.getFont))
        }
      case Some((block, None)) =>
        currentBlockPathTypePin = Some((blockPath, block.superclasses.head, Map()))
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
    val (blockPath, blockType, pinning) = currentBlockPathTypePin.exceptNone("no FootprintBlock selected")
    val blockPyClass = DesignAnalysisUtils.pyClassOf(blockType, project).exceptError

    val footprintFile = FootprintBrowser.footprintToFile(footprintName)
        .exceptNone(s"unknown footprint $footprintName")
    val footprintBlockType = ElemBuilder.LibraryPath("electronics_model.CircuitBlock.FootprintBlock")  // TODO belongs in shared place?
    val footprintBlockClass = DesignAnalysisUtils.pyClassOf(footprintBlockType, project).exceptError
    requireExcept(blockPyClass.isSubclass(footprintBlockClass, TypeEvalContext.codeAnalysis(project, null)),
      s"${blockPyClass.getName} not a FootprintClass")

    def continuation(added: PsiElement): Unit = {
      InsertAction.navigateToEnd(added)
      visualizer.kicadParser.setKicadFile(footprintFile)
      visualizer.pinmap = pinning.mapValues(ExprToString(_)).toMap  // TODO dedup
      footprintSynced = true
      visualizer.repaint()

      // TODO update things
    }

    InsertFootprintAction.createInsertFootprintFlow(blockPyClass, footprintName,
      project, continuation).exceptError
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
    state.kicadLibraryDirectory = FootprintBrowser.libraryDirectory.map(_.getAbsolutePath).getOrElse("")
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
    FootprintBrowser.setLibraryDirectory(state.kicadLibraryDirectory)
  }
}
