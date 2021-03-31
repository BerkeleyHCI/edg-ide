package edg_ide.ui

import com.intellij.ui.JBSplitter
import com.intellij.ui.treeStructure.treetable.TreeTable
import edg_ide.swing.{FootprintBrowserNode, FootprintBrowserTreeTableModel}

import java.awt.event.{MouseEvent, MouseListener, MouseWheelEvent, MouseWheelListener}
import java.awt.{BorderLayout, Color, Dimension}
import java.io.File
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing._

class KicadVizPanel() extends JPanel with MouseWheelListener {
  // State
  //



  object FootprintBrowser extends JPanel {
    // TODO flatten out into parent? Or make this its own class with meaningful interfaces / abstractions?
    // TODO use GridBagLayout?

    var libraryDirectory: String = ""  // TODO should be private / protected, but is in an object :s

    def setLibraryDirectory(directory: String): Unit = {
      // TODO use File instead of String
      val filterFunc = (x:String) => x.contains(filterTextBox.getText)
      libraryDirectory = directory
      tree.setModel(new FootprintBrowserTreeTableModel(new File(directory), filterFunc))
    }

    var model = new FootprintBrowserTreeTableModel(new File("."))
    private val tree = new TreeTable(model)
    tree.setShowColumns(true)
    tree.setRootVisible(false)
    private val treeScrollPane = new JScrollPane(tree)

    tree.addMouseListener(new MouseListener {
      override def mouseClicked(mouseEvent: MouseEvent): Unit = {
        // If a mod or kicad_mod file is double clicked, then visualize it
        if (mouseEvent.getClickCount == 2) {
          val node:FootprintBrowserNode = tree.getTree.getSelectionPath.getLastPathComponent.asInstanceOf[FootprintBrowserNode]
          if (node.file.getName.contains(".mod") || node.file.getName.contains(".kicad_mod")) {
            visualizer.kicadParser.setKicadFile(node.file.getCanonicalPath)
            visualizer.repaint()
          }
        }
      }

      override def mousePressed(mouseEvent: MouseEvent): Unit = {}

      override def mouseReleased(mouseEvent: MouseEvent): Unit = {}

      override def mouseEntered(mouseEvent: MouseEvent): Unit = {}

      override def mouseExited(mouseEvent: MouseEvent): Unit = {}
    })

    // Filter menu
    private val filterMenu = new JPanel
    private val filterTextBox = new JTextArea
    private val filterLabel = new JLabel("Filter")
    filterTextBox.setPreferredSize(new Dimension(400, 25))
    filterTextBox.setBorder(BorderFactory.createLineBorder(Color.WHITE))
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

    filterMenu.setLayout(new BoxLayout(filterMenu, BoxLayout.X_AXIS))
    filterMenu.add(filterTextBox)
    filterMenu.add(filterLabel)
    filterMenu.setMaximumSize(new Dimension(500, 25))

    setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))
    add(filterMenu)
    add(treeScrollPane)
  }

  // GUI Components
  //
  private val splitter = new JBSplitter(false, 0.5f, 0.1f, 0.9f)

  splitter.setSecondComponent(FootprintBrowser)

  private val visualizer = new KicadVizDrawPanel()
  visualizer.offset = (this.FootprintBrowser.getWidth * 1.2).asInstanceOf[Int] // @TODO clean this up with offset code
  splitter.setFirstComponent(visualizer)


  setLayout(new BorderLayout())
  add(splitter)


  override def mouseWheelMoved(mouseWheelEvent: MouseWheelEvent): Unit = {
  }

  // Configuration State
  //
  def saveState(state: BlockVisualizerServiceState): Unit = {
    state.kicadLibraryDirectory = FootprintBrowser.libraryDirectory
  }

  def loadState(state: BlockVisualizerServiceState): Unit = {
    FootprintBrowser.setLibraryDirectory(state.kicadLibraryDirectory)
  }
}
