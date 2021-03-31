package edg_ide.ui

import com.intellij.ui.JBSplitter
import com.intellij.ui.treeStructure.treetable.TreeTable
import edg.elem.elem
import edg.schema.schema
import edg.ref.ref
import edg.common.common
import edg.expr.expr
import edg.wir.DesignPath
import edg_ide.swing.{FootprintBrowserNode, FootprintBrowserTreeTableModel, SwingHtmlUtil}
import edg.compiler.{Compiler, ExprToString, TextValue}
import edg_ide.EdgirUtils

import java.awt.event.{MouseEvent, MouseListener, MouseWheelEvent, MouseWheelListener}
import java.awt.{BorderLayout, GridBagConstraints, GridBagLayout}
import java.io.File
import javax.swing._
import javax.swing.event.{DocumentEvent, DocumentListener}

class KicadVizPanel() extends JPanel with MouseWheelListener {
  // State
  //
  var currentBlockPath: Option[DesignPath] = None  // should be a Block with a footprint and pinning field


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
        if (mouseEvent.getClickCount == 1) {
          // single click opens the footprint for preview
          val node:FootprintBrowserNode = tree.getTree.getSelectionPath.getLastPathComponent.asInstanceOf[FootprintBrowserNode]
          // TODO also pre-check parse legality here?
          pathToFootprintName(node.file) match {
            case Some(footprintName) =>
              visualizer.kicadParser.setKicadFile(node.file.getCanonicalPath)
              visualizer.repaint()
              status.setText(s"Footprint preview: ${footprintName}")
            case _ =>
              status.setText(s"Invalid file: ${node.file.getName}")
          }
        } else if (mouseEvent.getClickCount == 2) {
          // double click assigns the footprint to the opened block

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

  def setBlock(blockPath: DesignPath, design: schema.Design, compiler: Compiler): Unit = {
    EdgirUtils.resolveExactBlock(blockPath, design).flatMap { block =>
      footprintFromBlock(blockPath, block, compiler)
    } match {
      case Some((footprint, pinning)) =>
        val footprintStr = footprint.replace(":", ": ")  // TODO this allows a line break but is hacky
        currentBlockPath = Some(blockPath)
        // TODO the proper way might be to fix the stylesheet to allow linebreaks on these characters?
        FootprintBrowser.footprintToFile(footprint) match {
          case Some(footprintFile) =>
            println(pinning)
            visualizer.kicadParser.setKicadFile(footprintFile.getCanonicalPath)
            visualizer.pinmap = pinning.mapValues(ExprToString(_)).toMap
            visualizer.repaint()
            status.setText(SwingHtmlUtil.wrapInHtml(s"Footprint ${footprintStr} at ${blockPath.lastString}",
              this.getFont))

          case _ =>
            visualizer.pinmap = Map()
            status.setText(SwingHtmlUtil.wrapInHtml(s"Unknown footprint ${footprintStr} at ${blockPath.lastString}",
              this.getFont))
        }

      case None =>
        currentBlockPath = None
        visualizer.pinmap = Map()
        status.setText(SwingHtmlUtil.wrapInHtml(s"No footprint at ${blockPath.lastString}",
          this.getFont))
    }
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
