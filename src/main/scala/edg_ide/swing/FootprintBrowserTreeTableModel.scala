package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree.TreePath
import java.io.File
import java.nio.file.{Files, NotDirectoryException, Paths}


class FootprintBrowserNode(fArg: File) {

  val file: File = fArg

  def isValidFile(filename: String): Boolean = {
    if (filename == null || filename == "" || filename == "." || filename == "..") return false
    val currFile = new File(filename)
    if (currFile == null) return false
    val ans = filename.contains(".mod") || filename.contains(".kicad_mod") || currFile.isDirectory
    ans
  }

  lazy val children: Seq[FootprintBrowserNode] = {
    var filenamesMaybe = Option(file.list())
    filenamesMaybe match {
    case Some(value) => filenamesMaybe
      .get
      .map(f => file.getCanonicalPath + "/" + f)
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

class FootprintBrowserTreeTableModel(file: File) extends SeqTreeTableModel[FootprintBrowserNode] {
  val rootNode: FootprintBrowserNode = new FootprintBrowserNode(file)
//  print(file.getName)
  val COLUMNS = Seq("Path")

  override def getNodeChildren(node: FootprintBrowserNode): Seq[FootprintBrowserNode] = node.children

  override def getRootNode: FootprintBrowserNode = rootNode

  override def getNodeValueAt(node: FootprintBrowserNode, column: Int): AnyRef = node.file

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
