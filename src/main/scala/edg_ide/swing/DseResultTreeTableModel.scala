package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.TreeTableModel
import edg_ide.dse.{DseConfigElement, DseResult}

import javax.swing.JTree
import javax.swing.event.TreeModelListener
import javax.swing.tree.TreePath


trait DseResultNodeBase {
  val children: Seq[DseResultNodeBase]
  val config: String
  def getColumns(index: Int): String

  override def toString = config
}

class DseResultTreeNode(columnToObjectiveName: Map[Int, String], results: Seq[DseResult], inProgress: Boolean) extends DseResultNodeBase {
  // Aggregates similar results together, somewhat preserving order of the input
  private def combineSimilarResults(results: Seq[DseResult]): Seq[Seq[DseResult]] = {
    // Stores results, combining by unique combination of design, error, and objective results
    // Specifically avoids the actual refinements, which may produce a false positive match
    // (different configurations that produce the same design in the end)
    results.groupBy(result => (result.compiled, result.errors, result.objectives))
        .values.toSeq
  }

  private val informationalHeader = if (inProgress) {
    Seq(new InformationalNode("... search in progress ..."))
  } else {
    Seq()
  }

  // Defines the root node
  override lazy val children = informationalHeader ++ combineSimilarResults(results).map { resultsSet =>
    new ResultSetNode(resultsSet)
  }
  override val config = "" // empty, since the root node is hidden
  override def getColumns(index: Int): String = ""


  // Displays a set of equivalent results, useful for deduplicating similar results
  class ResultSetNode(setMembers: Seq[DseResult]) extends DseResultNodeBase {
    private val exampleResult = setMembers.head
    private val errString = if (exampleResult.errors.nonEmpty) {
      f", ${exampleResult.errors.length} errors"
    } else {
      ""
    }
    override val config = f"${setMembers.length} points" + errString
    override lazy val children = setMembers.map(result => new ResultNode(result))
    override def getColumns(index: Int): String = columnToObjectiveName.get(index) match {
      case Some(objectiveName) => exampleResult.objectives.get(objectiveName) match {
        case Some(value) => DseConfigElement.valueToString(value)
        case _ => "???"
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


class DseResultTreeTableModel(results: Seq[DseResult], inProgress: Boolean)
    extends SeqTreeTableModel[DseResultNodeBase] {
  val objectiveNames = results.headOption.map(_.objectives.keys).getOrElse(Seq()).toSeq
  val COLUMNS = Seq("Config") ++ objectiveNames

  val columnToObjectiveNames = objectiveNames.zipWithIndex.map(elt => elt._2 + 1 -> elt._1).toMap
  val rootNode: DseResultNodeBase = new DseResultTreeNode(columnToObjectiveNames, results, inProgress)

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
