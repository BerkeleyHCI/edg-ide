package edg_ide.ui

import java.awt.{BorderLayout, Dimension}
import java.awt.event.{ActionEvent, MouseEvent, MouseListener, MouseWheelEvent, MouseWheelListener}
import java.io.File

import com.intellij.ui.JBSplitter
import com.intellij.ui.components.JBScrollPane
import com.intellij.ui.treeStructure.treetable.TreeTable
import edg.schema.schema
import edg.wir
import edg_ide.swing.{EdgirLibraryTreeTableModel, FootprintBrowserNode, FootprintBrowserTreeTableModel}
import javax.swing.event.{TreeModelEvent, TreeModelListener}
import javax.swing.{BoxLayout, JButton, JPanel, JScrollPane, JTextArea, JTree}

class KicadVizPanel() extends JPanel with MouseWheelListener {
  // State
  //


  object FootprintBrowser extends JPanel {

    var model = new FootprintBrowserTreeTableModel(new File("/Users/nikhiljain/Downloads/Connector_Audio.pretty"))
    private var tree = new TreeTable(model)
    tree.setShowColumns(true)
    tree.setRootVisible(false)
    private var treeScrollPane = new JScrollPane(tree)

    // Top menu bar
    private val menuBar = new JPanel
    private val textBox = new JTextArea
    private val button = new JButton("Set Path")
    textBox.setPreferredSize(new Dimension(400, 25))
    menuBar.setLayout(new BoxLayout(menuBar, BoxLayout.X_AXIS))
    menuBar.add(textBox)
    menuBar.add(button)
    menuBar.setMaximumSize(new Dimension(500, 25))

    button.addActionListener((actionEvent: ActionEvent) => {
      // Refresh tree -- TODO could probably do in a better way, especially with the mouse listener...
      tree = new TreeTable(new FootprintBrowserTreeTableModel(new File(textBox.getText)))
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

      remove(treeScrollPane)
      treeScrollPane = new JScrollPane(tree)
      add(treeScrollPane)
    })

    // TODO dont have this listener declared twice
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
    add(menuBar)
    add(treeScrollPane)

  }


  private var library: wir.Library = new wir.EdgirLibrary(schema.Library())
  private var kicadFile:String = "hello"

  // GUI Components
  //
  private val splitter = new JBSplitter(false, 0.5f, 0.1f, 0.9f)

  splitter.setSecondComponent(FootprintBrowser)

  private val visualizer = new KicadVizDrawPanel()
  visualizer.offset = (this.FootprintBrowser.getWidth * 1.2).asInstanceOf[Int] // @TODO clean this up with offset code
  splitter.setFirstComponent(visualizer)


  setLayout(new BorderLayout())
  add(splitter)


  def setKicadFile(kicadFile: String): Unit = {
    this.kicadFile = kicadFile
  }

  override def mouseWheelMoved(mouseWheelEvent: MouseWheelEvent): Unit = {
    println(mouseWheelEvent.getWheelRotation, "scrolliosis")
  }

}
