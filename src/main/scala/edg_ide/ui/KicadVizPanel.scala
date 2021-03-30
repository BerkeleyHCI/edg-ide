package edg_ide.ui

import java.awt.{BorderLayout, Color, Dimension}
import java.awt.event.{ActionEvent, MouseEvent, MouseListener, MouseWheelEvent, MouseWheelListener}
import java.io.File

import com.intellij.ui.JBSplitter
import com.intellij.ui.components.JBScrollPane
import com.intellij.ui.treeStructure.treetable.TreeTable
import edg.schema.schema
import edg.wir
import edg_ide.swing.{EdgirLibraryTreeTableModel, FootprintBrowserNode, FootprintBrowserTreeTableModel}
import javax.swing.event.{DocumentEvent, DocumentListener, TreeModelEvent, TreeModelListener}
import javax.swing.{BorderFactory, BoxLayout, JButton, JLabel, JPanel, JScrollPane, JTextArea, JTree}

class KicadVizPanel() extends JPanel with MouseWheelListener {
  // State
  //


  object FootprintBrowser extends JPanel {

    var model = new FootprintBrowserTreeTableModel(new File("."))
    private val tree = new TreeTable(model)
    tree.setShowColumns(true)
    tree.setRootVisible(false)
    private val treeScrollPane = new JScrollPane(tree)

    // Top menu bar
    private val filepathMenu = new JPanel
    private val filepathTextbox = new JTextArea
    private val filepathButton = new JButton("Set Path")
    filepathTextbox.setPreferredSize(new Dimension(400, 25))
    filepathMenu.setLayout(new BoxLayout(filepathMenu, BoxLayout.X_AXIS))
    filepathMenu.add(filepathTextbox)
    filepathMenu.add(filepathButton)
    filepathMenu.setMaximumSize(new Dimension(500, 25))

    filepathButton.addActionListener((actionEvent: ActionEvent) => {
      // Refresh tree
      val filterFunc = (x:String) => x.contains(filterTextBox.getText)
      tree.setModel(new FootprintBrowserTreeTableModel(new File(filepathTextbox.getText), filterFunc))
    })

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

    setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))
    add(filepathMenu)
    add(treeScrollPane)

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
    add(filepathMenu)
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

}
