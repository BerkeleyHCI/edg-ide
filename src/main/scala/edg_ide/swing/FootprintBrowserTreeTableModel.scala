package edg_ide.swing

import java.io.File

import com.sun.glass.ui.mac.MacFileNSURL
import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree.TreePath
import jnr.posix.JavaSecuredFile
import org.codehaus.groovy.runtime.WritableFile
import sun.awt.shell.ShellFolder


class FootprintBrowserNode(fp: String) {
  // set children to be one level lower (only .mod files tho)

  val filepath: String = fp

  def isDirOrModfile(f: File): Boolean = {
    // only show directories, or files with extension .mod or .kicad_mod
    // also filter out null values
    if (f == null) return false
    if (f.getName == null) return false
    f.isDirectory || (f.isFile && (f.getName.contains(".mod") || f.getName.contains(".kicad_mod")))
  }

//  print(new File(filepath).listFiles.map(_.getName))

  val c: Seq[File] = {
    val f = new File(filepath)
    if (f.listFiles == null)  {
      println("nullonomics")
      List()
    }
    else {
      f.listFiles
        .filter(isDirOrModfile)
    }

  }

  println("children", c, c.length)

  lazy val children: Seq[FootprintBrowserNode] = {
    if (c != null && c.nonEmpty)
      c.map(f => new FootprintBrowserNode(f.getName))
    else
      Seq()
  }

//  lazy val children = List(new FootprintBrowserNode("a"), new FootprintBrowserNode("b"))

  override def equals(other: Any): Boolean = other match {
    case other: FootprintBrowserNode => (other.filepath == this.filepath) && (other.children == this.children)
    case _ => false
  }

  override def toString: String = filepath // + children.toString()

  def getColumns(index: Int): String = "Path"

}

class FootprintBrowserTreeTableModel(root: String) extends SeqTreeTableModel[FootprintBrowserNode] {
  val rootNode: FootprintBrowserNode = new FootprintBrowserNode(root)
  val COLUMNS = Seq("Path")

  override def getNodeChildren(node: FootprintBrowserNode): Seq[FootprintBrowserNode] = node.children

  override def getRootNode: FootprintBrowserNode = rootNode

  override def getNodeValueAt(node: FootprintBrowserNode, column: Int): AnyRef = node.filepath
//    = {
//    // Only show directories or .mod/.kicad_mod files
//    if (node.isDirOrModfile(new File(node.filepath)))
//      node.filepath
//    else
//      ""
//  }

  override def getColumnCount: Int = COLUMNS.length

  override def getColumnName(i: Int): String = COLUMNS(i)

  override def getColumnClass(i: Int): Class[_] = i match {
    case 0 => classOf[FootprintBrowserNode]
    case _ => classOf[String]
  }

  // Trees can't be edited
  override def setTree(jTree: JTree): Unit = {}
  override def valueForPathChanged(treePath: TreePath, o: Any): Unit = {}
  override def addTreeModelListener(treeModelListener: TreeModelListener): Unit = {}
  override def removeTreeModelListener(treeModelListener: TreeModelListener): Unit = {}
}

