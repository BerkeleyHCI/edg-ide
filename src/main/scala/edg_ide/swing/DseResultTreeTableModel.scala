package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg_ide.dse.{DseConfigElement, DseResult}

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree.TreePath


object DseResultTreeNode {
  // Aggregates similar results together, somewhat preserving order of the input
  private def combineSimilarResults(results: Seq[DseResult]): Seq[Seq[DseResult]] = {
    // Stores results, combining by unique combination of design, error, and objective results
    // Specifically avoids the actual refinements, which may produce a false positive match
    // (different configurations that produce the same design in the end)
    results.groupBy(result => (result.compiled, result.errors, result.objectives))
        .values.toSeq
  }

  trait NodeBase {
    val children: Seq[NodeBase]

    val config: String
    val values: String

    override def toString = config
    def getColumns(index: Int): String = index match {
      case 1 => values
      case _ => "???"
    }
  }

  class LeafNode(val config: String, val errors: String, val values: String) extends NodeBase {
    override val children: Seq[NodeBase] = Seq()
  }

  class RootNode(results: Seq[DseResult]) extends NodeBase {
    override lazy val children = combineSimilarResults(results).map { resultsSet =>
      new ResultSetNode(resultsSet)
    }

    override val config = ""  // empty, since the root node is hidden
    override val values = ""
  }

  // Displays a set of equivalent results, useful for deduplicating similar results
  class ResultSetNode(setMembers: Seq[DseResult]) extends NodeBase {
    class InnerResultsNode extends NodeBase {
      override val config = f"Individual Results"
      override val values = ""
      override lazy val children = setMembers.map(result => new ResultNode(result))
    }

    private val exampleResult = setMembers.head
    override val config = DseConfigElement.configMapToString(exampleResult.config)
    val errString = if (exampleResult.errors.nonEmpty) {
      f", ${exampleResult.errors.length} errors"
    } else {
      ""
    }
    override val values = f"${setMembers.length} in set" + errString
    override lazy val children = Seq(
      new InnerResultsNode()
    ) ++ exampleResult.objectives.map { case (objectiveName, objectiveValue) =>
      new LeafNode(objectiveName, "", DseConfigElement.valueToString(objectiveValue))
    }
  }

  class ResultNode(result: DseResult) extends NodeBase {
    override val config = DseConfigElement.configMapToString(result.config)
    override val values = ""
    override val children: Seq[NodeBase] = Seq()
  }
}


class DseResultTreeTableModel(results: Seq[DseResult])
    extends SeqTreeTableModel[DseResultTreeNode.NodeBase] {
  val rootNode: DseResultTreeNode.NodeBase = new DseResultTreeNode.RootNode(results)
  val configNames = results.headOption.map{ result => result.config.keys.map(_.configToString) }.getOrElse(Seq()).toSeq
  val objectiveNames = results.headOption.map(_.objectives.keys).getOrElse(Seq()).toSeq
  val COLUMNS = Seq("Config", "Values") ++ configNames ++ objectiveNames

  // TreeView abstract methods
  //
  override def getRootNode: DseResultTreeNode.NodeBase = rootNode

  override def getNodeChildren(node: DseResultTreeNode.NodeBase): Seq[DseResultTreeNode.NodeBase] = node.children

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

  override def getNodeValueAt(node: DseResultTreeNode.NodeBase, column: Int): Object = node.getColumns(column)

  // These aren't relevant for trees that can't be edited
  override def isNodeCellEditable(node: DseResultTreeNode.NodeBase, column: Int): Boolean = false
  override def setNodeValueAt(aValue: Any, node: DseResultTreeNode.NodeBase, column: Int): Unit = {}

  def setTree(tree: JTree): Unit = { }  // tree updates ignored
}
