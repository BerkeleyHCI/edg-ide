package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg.compiler.{CompilerError, ElaborateRecord}

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree.TreePath


trait CompilerErrorNode {
  def getChildren: Seq[CompilerErrorNode]
  def getColumns(index: Int): String
}


class CompilerErrorTopNode(errs: Seq[CompilerError]) extends CompilerErrorNode {
  def getChildren: Seq[CompilerErrorNode] = errs.map { new CompilerErrorLeafNode(_) }
  def getColumns(index: Int): String = ""

  override def toString: String = "All Errors"
}


class CompilerErrorLeafNode(err: CompilerError) extends CompilerErrorNode {
  def getChildren: Seq[CompilerErrorNode] = Seq()
  def getColumns(index: Int): String = err match {
    case CompilerError.Unelaborated(ElaborateRecord.Block(path), deps) => path.toString
    case CompilerError.Unelaborated(ElaborateRecord.Link(path), deps) => path.toString
    case CompilerError.Unelaborated(ElaborateRecord.Param(path), deps) => path.toString
    case CompilerError.Unelaborated(ElaborateRecord.Generator(path, fnName), deps) => path.toString
    case CompilerError.LibraryElement(path, target) => path.toString
    case CompilerError.Generator(path, targets, fn) => path.toString
    case CompilerError.ConflictingAssign(target, oldAssign, newAssign) => target.toString
    case _ => ""
  }

  override def toString: String = err.toString
}


class CompilerErrorTreeTableModel(errs: Seq[CompilerError]) extends SeqTreeTableModel[CompilerErrorNode] {
  val rootNode: CompilerErrorTopNode = new CompilerErrorTopNode(errs)
  val COLUMNS = Seq("Error", "Path")

  // TreeView abstract methods
  //
  override def getRootNode: CompilerErrorNode = rootNode

  override def getNodeChildren(node: CompilerErrorNode): Seq[CompilerErrorNode] = node.getChildren

  // These aren't relevant for trees that can't be edited
  override def valueForPathChanged(path: TreePath, newValue: Any): Unit = {}
  override def addTreeModelListener(l: TreeModelListener): Unit = {}
  override def removeTreeModelListener(l: TreeModelListener): Unit = {}

  // TreeTableView abstract methods
  //
  override def getColumnCount: Int = COLUMNS.length

  override def getColumnName(column: Int): String = COLUMNS(column)

  override def getColumnClass(column: Int): Class[_] = column match {
    case 0 => classOf[TreeTableModel]
    case _ => classOf[String]
  }

  override def getNodeValueAt(node: CompilerErrorNode, column: Int): Object = node.getColumns(column)

  // These aren't relevant for trees that can't be edited
  override def isNodeCellEditable(node: CompilerErrorNode, column: Int): Boolean = false
  override def setNodeValueAt(aValue: Any, node: CompilerErrorNode, column: Int): Unit = {}

  def setTree(tree: JTree): Unit = { }  // tree updates ignored
}
