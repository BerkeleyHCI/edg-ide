package edg_ide.swing.dse

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg_ide.dse.{CombinedDseResultSet, DseConfigElement, DseResult}
import edg_ide.swing.SeqTreeTableModel

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree.TreePath


trait DseResultNodeBase {
  val children: Seq[DseResultNodeBase]
  val config: String
  def getColumns(index: Int): String

  override def toString = config
}

class DseResultTreeNode(results: CombinedDseResultSet, objectiveNames: Seq[String], inProgress: Boolean) extends DseResultNodeBase {
  private val informationalHeader = if (inProgress) {
    Seq(new InformationalNode("... search in progress ..."))
  } else {
    Seq()
  }
  private val objectiveNameByColumn = objectiveNames.zipWithIndex.map { case (objective, index) =>
    index + 1 -> objective
  }.toMap


  // Defines the root node
  override lazy val children = informationalHeader ++ results.groupedResults.map { resultsSet =>
    new ResultSetNode(resultsSet)
  }
  override val config = "" // empty, since the root node is hidden
  override def getColumns(index: Int): String = ""


  // Displays a set of equivalent results, useful for deduplicating similar results
  class ResultSetNode(val setMembers: Seq[DseResult]) extends DseResultNodeBase {
    private val exampleResult = setMembers.head
    private val errString = if (exampleResult.errors.nonEmpty) {
      " (with errors)"
    } else {
      ""
    }
    override val config = f"${setMembers.length} points" + errString
    override lazy val children = setMembers.map(result => new ResultNode(result))
    override def getColumns(index: Int): String = objectiveNameByColumn.get(index) match {
      case Some(objectiveName) => exampleResult.objectives.get(objectiveName) match {
        case Some(value) => DseConfigElement.valueToString(value)
        case _ => "(unknown)"
      }
      case _ => "???"
    }
  }

  class ResultNode(val result: DseResult) extends DseResultNodeBase {
    override val config = f"${result.index}: ${DseConfigElement.configMapToString(result.config)}"
    override def getColumns(index: Int): String = ""
    override val children: Seq[DseResultNodeBase] = Seq()
  }

  class InformationalNode(text: String) extends DseResultNodeBase {
    override val config = text
    override def getColumns(index: Int): String = ""
    override val children: Seq[DseResultNodeBase] = Seq()
  }
}


class DseResultTreeTableModel(results: CombinedDseResultSet, objectiveNames: Seq[String], inProgress: Boolean)
    extends SeqTreeTableModel[DseResultNodeBase] {
  val COLUMNS = Seq("Config") ++ objectiveNames

  val rootNode: DseResultTreeNode = new DseResultTreeNode(results, objectiveNames, inProgress)

  // TreeView abstract methods
  //
  override def getRootNode: DseResultNodeBase = rootNode

  override def getNodeChildren(node: DseResultNodeBase): Seq[DseResultNodeBase] = node.children

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

  override def getNodeValueAt(node: DseResultNodeBase, column: Int): Object = node.getColumns(column)

  // These aren't relevant for trees that can't be edited
  override def isNodeCellEditable(node: DseResultNodeBase, column: Int): Boolean = false
  override def setNodeValueAt(aValue: Any, node: DseResultNodeBase, column: Int): Unit = {}

  def setTree(tree: JTree): Unit = { }  // tree updates ignored
}
