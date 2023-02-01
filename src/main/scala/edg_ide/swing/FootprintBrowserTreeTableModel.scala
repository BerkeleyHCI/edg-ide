package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.TreeTableModel

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree.TreePath
import java.io.File


sealed trait FootprintBrowserBaseNode {
  def children: Seq[FootprintBrowserBaseNode]
}


class FootprintBrowserRootNode(directories: Seq[File]) extends FootprintBrowserBaseNode {
  override lazy val children: Seq[FootprintBrowserNode] = directories.flatMap { directory =>
    directory.list().flatMap {  // flatten the libraries regardless of their containing directory
      case elt if elt.endsWith(".pretty") => Some(new FootprintBrowserNode(new File(directory, elt)))
      case _ => None
    }
  }
}


class FootprintBrowserNode(val file: File) extends FootprintBrowserBaseNode {
  def isValidFile(filename: String): Boolean = {
    if (filename == "." || filename == "..") return false  // ignore self and up pointers
    val currFile = new File(filename)
    currFile.exists() && (filename.endsWith(".mod") || filename.endsWith(".kicad_mod") || currFile.isDirectory)
  }

  override lazy val children: Seq[FootprintBrowserNode] = {
    Option(file.list()) match {
      case Some(filenames) => filenames.toSeq
        .map(filename => file.getCanonicalPath + "/" + filename)
        .filter(isValidFile)
        .sorted
        .map(f => new FootprintBrowserNode(new File(f)))
      case None => Seq()
    }
  }


  override def equals(obj: Any): Boolean = obj match {
    case obj:FootprintBrowserNode => obj.file.equals(this.file)
    case _ => false
  }

  override def toString: String = file.getName

}

class FootprintBrowserTreeTableModel(directories: Seq[File]) extends SeqTreeTableModel[FootprintBrowserBaseNode] {
  private val rootNode: FootprintBrowserRootNode = new FootprintBrowserRootNode(directories)
  private val COLUMNS = Seq("Path")

  override def getNodeChildren(node: FootprintBrowserBaseNode): Seq[FootprintBrowserBaseNode] = node.children

  override def getRootNode: FootprintBrowserBaseNode = rootNode

  override def getNodeValueAt(node: FootprintBrowserBaseNode, column: Int): FootprintBrowserBaseNode = node

  override def getColumnCount: Int = COLUMNS.length

  override def getColumnName(i: Int): String = COLUMNS(i)

  override def getColumnClass(i: Int): Class[_] = i match {
    case 0 => classOf[TreeTableModel]
    case _ => classOf[String]
  }

  override def setTree(jTree: JTree): Unit = {}

  override def valueForPathChanged(treePath: TreePath, o: Any): Unit = {}

  override def addTreeModelListener(treeModelListener: TreeModelListener): Unit = {}

  override def removeTreeModelListener(treeModelListener: TreeModelListener): Unit = {}
}
