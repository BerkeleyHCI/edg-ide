package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg.compiler.{CompilerError, ElaborateRecord, ExprToString}
import edg.wir.DesignPath
import edg_ide.EdgirUtils

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree.TreePath


trait CompilerErrorNodeBase {
  val children: Seq[CompilerErrorNodeBase]
  def getColumns(index: Int): String
}

object CompilerErrorNodeBase {
  class CompilerErrorTopNode(errs: Seq[CompilerError]) extends CompilerErrorNodeBase {
    override lazy val children: Seq[CompilerErrorNodeBase] = errs.map {
      new CompilerErrorNode(_)
    }

    override def getColumns(index: Int): String = ""

    override def toString: String = "All Errors"
  }

  // Freeform node with arbitrary text and path
  class CompilerErrorDetailNode(text: String, path: String) extends CompilerErrorNodeBase {
    override lazy val children = Seq()
    override def getColumns(index: Int): String = path
    override def toString: String = text
  }

  // Error node defining text, path, and children for a compiler error
  class CompilerErrorNode(err: CompilerError) extends CompilerErrorNodeBase {
    def elaborateRecordToDetailNode(rec: ElaborateRecord): CompilerErrorDetailNode = rec match {
      case rec @ (ElaborateRecord.Block(_) | ElaborateRecord.Link(_) |
          ElaborateRecord.Param(_) |  ElaborateRecord.Generator(_, _) |
          ElaborateRecord.Connect(_, _)) =>
        new CompilerErrorDetailNode(s"Unexpected Missing Record $rec", "")
      case ElaborateRecord.ConnectedLink(path) =>
        new CompilerErrorDetailNode("Missing Connected Link at Port", path.toString)
      case ElaborateRecord.ParamValue(path) =>
        new CompilerErrorDetailNode("Missing Param Value", path.toString)
    }

    lazy val all: (String, String, Seq[CompilerErrorNodeBase]) = err match {
      case CompilerError.Unelaborated(ElaborateRecord.Block(path), deps) =>
        ("Unelaborated Block", path.toString, deps.toSeq.map(elaborateRecordToDetailNode))
      case CompilerError.Unelaborated(ElaborateRecord.Link(path), deps) =>
        ("Unelaborated Link", path.toString, deps.toSeq.map(elaborateRecordToDetailNode))
      case CompilerError.Unelaborated(ElaborateRecord.Param(path), deps) =>
        ("Unelaborated Param", path.toString, deps.toSeq.map(elaborateRecordToDetailNode))
      case CompilerError.Unelaborated(ElaborateRecord.Generator(path, fnName), deps) =>
        (s"Unelaborated Generator", s"${path.toString}:$fnName", deps.toSeq.map(elaborateRecordToDetailNode))
      case CompilerError.Unelaborated(ElaborateRecord.Connect(toLinkPortPath, fromLinkPortPath), deps) =>
        (s"Unelaborated Connect", "", Seq(
          new CompilerErrorDetailNode("Connect Towards Link Port", toLinkPortPath.toString),
          new CompilerErrorDetailNode("Connect Away From Link Port", fromLinkPortPath.toString),
        ) ++ deps.toSeq.map(elaborateRecordToDetailNode))
      case CompilerError.LibraryElement(path, target) =>
        (s"Missing library element ${EdgirUtils.SimpleLibraryPath(target)}", path.toString, Seq())
      case CompilerError.Generator(path, targets, fnName) =>
        (s"Generator not ready, ${EdgirUtils.SimpleSuperclass(targets)}:$fnName", path.toString, Seq())
      case CompilerError.ConflictingAssign(target, oldAssign, newAssign) =>
        ("Conflicting assign", target.toString, Seq(
          new CompilerErrorDetailNode(ExprToString(oldAssign._3), s"${oldAssign._1}:${oldAssign._2}"),
          new CompilerErrorDetailNode(ExprToString(newAssign._3), s"${newAssign._1}:${newAssign._2}"),
        ))
    }

    override lazy val children: Seq[CompilerErrorNodeBase] = all._3
    override def getColumns(index: Int): String = all._2
    override def toString: String = all._1
  }
}


class CompilerErrorTreeTableModel(errs: Seq[CompilerError]) extends SeqTreeTableModel[CompilerErrorNodeBase] {
  val rootNode: CompilerErrorNodeBase = new CompilerErrorNodeBase.CompilerErrorTopNode(errs)
  val COLUMNS = Seq("Error", "Path")

  // TreeView abstract methods
  //
  override def getRootNode: CompilerErrorNodeBase = rootNode

  override def getNodeChildren(node: CompilerErrorNodeBase): Seq[CompilerErrorNodeBase] = node.children

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

  override def getNodeValueAt(node: CompilerErrorNodeBase, column: Int): Object = node.getColumns(column)

  // These aren't relevant for trees that can't be edited
  override def isNodeCellEditable(node: CompilerErrorNodeBase, column: Int): Boolean = false
  override def setNodeValueAt(aValue: Any, node: CompilerErrorNodeBase, column: Int): Unit = {}

  def setTree(tree: JTree): Unit = { }  // tree updates ignored
}
