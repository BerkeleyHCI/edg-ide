package edg_ide.ui

import java.awt.BorderLayout
import java.awt.event.{MouseWheelEvent, MouseWheelListener}

import com.intellij.ui.JBSplitter
import com.intellij.ui.components.JBScrollPane
import com.intellij.ui.treeStructure.treetable.TreeTable
import edg.schema.schema
import edg.wir
import edg_ide.swing.EdgirLibraryTreeTableModel
import javax.swing.JPanel

class KicadVizPanel() extends JPanel with MouseWheelListener {
  // State
  //

  private var library: wir.Library = new wir.EdgirLibrary(schema.Library())
  private var kicadFile:String = "hello"

  // GUI Components
  //
  private val splitter = new JBSplitter(false, 0.5f, 0.1f, 0.9f)

  // TODO placeholder, add custom tree models here
  private val libraryTree = new TreeTable(new EdgirLibraryTreeTableModel(library))
  libraryTree.setShowColumns(true)
  private val libraryTreeScrollPane = new JBScrollPane(libraryTree)
  splitter.setFirstComponent(libraryTreeScrollPane)

  private val visualizer = new KicadVizDrawPanel()
  visualizer.offset = (this.libraryTreeScrollPane.getWidth * 1.2).asInstanceOf[Int]
  splitter.setSecondComponent(visualizer)

  setLayout(new BorderLayout())
  add(splitter)


  def setKicadFile(kicadFile: String): Unit = {
    this.kicadFile = kicadFile
  }

  override def mouseWheelMoved(mouseWheelEvent: MouseWheelEvent): Unit = {
    println(mouseWheelEvent.getWheelRotation, "scrolliosis")
  }

}
