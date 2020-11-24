package edg_ide

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg.elem.elem.HierarchyBlock

import javax.swing.tree._
import javax.swing.event.TreeModelListener
import javax.swing.JTree


class HierarchyBlockNode(val block: HierarchyBlock) {

}


class EdgTreeTableModel(val root: HierarchyBlock) extends TreeTableModel {
  val COLUMNS = Seq("Path", "Class")

  // TreeView abstract methods
  //
  override def getRoot: Object = new Object()

  override def isLeaf(node: Object): Boolean = true

  override def getChildCount(node: Object): Int = 0

  override def getChild(parent: Object, index: Int): Object = new Object()

  override def getIndexOfChild(parent: Object, child: Object): Int = 0

  // These aren't relevant for trees that can't be edited
  override def valueForPathChanged(path: TreePath, newValue: Any): Unit = {}
  override def addTreeModelListener(l: TreeModelListener): Unit = {}
  override def removeTreeModelListener(l: TreeModelListener): Unit = {}

  // TreeTableView abstract methods
  //
  override def getColumnCount: Int = COLUMNS.length

  override def getColumnName(column: Int): String = COLUMNS(column)

  override def getColumnClass(column: Int): Class[_] = {
    if (column == 0) {
      return classOf[TreeTableModel]
    } else {
      return classOf[String]
    }
  }

  override def getValueAt(node: Object, column: Int): Object = new Object()

  // These aren't relevant for trees that can't be edited
  override def isCellEditable(node: Object, column: Int): Boolean = false
  override def setValueAt(aValue: Any, node: Object, column: Int): Unit = {}

  def setTree(tree: JTree): Unit = { require(false) }  // idk what this is
}
