package edg_ide.swing
import javax.swing.JTree
import javax.swing.event.{TreeModelEvent, TreeModelListener}
import javax.swing.tree.TreePath
import collection.mutable
import scala.reflect.ClassTag

/** A filtering layer around a TreeModel, that takes a filter function on node types, and displays only the filtered
  * nodes, all their children (recursively), and all their parents.
  *
  * Filtering happens on filter application, and traverses the entire tree, requiring N time.
  */
class FilteredTreeTableModel[NodeType <: Object](model: SeqTreeTableModel[NodeType])(implicit tag: ClassTag[NodeType])
    extends SeqTreeTableModel[NodeType] {

  /** Applies the filter on nodes. Computation happens immediately, traversing all nodes to rebuild the tree.
    */
  def setFilter(filter: NodeType => Boolean): Seq[TreePath] = {
    val (newFilteredChildren, filterMatchPaths) = computeFilteredChildren(filter)
    filteredChildren = newFilteredChildren
    listeners.foreach { listener =>
      // TODO maybe be more accurate about tree ops to preserve tree state
      listener.treeStructureChanged(
        new TreeModelEvent(getRootNode.asInstanceOf[Object], Array(getRootNode.asInstanceOf[Object]))
      )
    }
    filterMatchPaths
  }

  // TODO also listen to model events to update tree?
  // Computes filtered children, and returns paths to all children that match the filter
  private def computeFilteredChildren(
      filter: NodeType => Boolean
  ): (Map[NodeType, Seq[NodeType]], Seq[TreePath]) = {
    val treeBuilder = mutable.Map[NodeType, Seq[NodeType]]() // TODO use identity map and eq
    val filterMatchPaths = mutable.ListBuffer[TreePath]()

    // traverses a node (recursively), returning whether it (or its children) have passed the filter
    // and should be included in the filtered set
    def traverse(node: NodeType, nodePath: TreePath, parentPassedFilter: Boolean): Boolean = {
      val originalChildren = model.getNodeChildren(node)

      val filterMatches = filter(node)
      if (parentPassedFilter || filterMatches) { // include the entire subtree
        if (filterMatches) {
          filterMatchPaths += nodePath
        }
        originalChildren.foreach { child =>
          traverse(child, nodePath.pathByAddingChild(child), true)
        }

        if (treeBuilder.contains(node)) {
          require(treeBuilder(node) == originalChildren, s"inconsistent reinsertion at $node")
        }
        treeBuilder.put(node, originalChildren)

        true
      } else {
        val filteredChildren = originalChildren
          .map { child =>
            (child, traverse(child, nodePath.pathByAddingChild(child), false))
          }
          .collect { case (child, true) => child }

        if (treeBuilder.contains(node)) {
          require(treeBuilder(node) == originalChildren, s"inconsistent reinsertion at $node")
        }
        treeBuilder.put(node, filteredChildren)

        filteredChildren.nonEmpty
      }
    }
    traverse(model.getRootNode, new TreePath(model.getRootNode), false)
    (treeBuilder.toMap, filterMatchPaths.toSeq)
  }

  private var filteredChildren = computeFilteredChildren(_ => true)._1

  override def getRootNode: NodeType = model.getRootNode
  override def getNodeChildren(node: NodeType): Seq[NodeType] = filteredChildren.getOrElse(node, Seq())

  // TreeView abstract methods are directly delegated
  override def getColumnCount: Int = model.getColumnCount
  override def getColumnName(column: Int): String = model.getColumnName(column)
  override def getColumnClass(column: Int): Class[_] = model.getColumnClass(column)
  override def getNodeValueAt(node: NodeType, column: Int): AnyRef = model.getNodeValueAt(node, column)

  override def valueForPathChanged(path: TreePath, newValue: Any): Unit =
    model.valueForPathChanged(path, newValue)
  private var listeners = mutable.ListBuffer[TreeModelListener]()
  override def addTreeModelListener(l: TreeModelListener): Unit = {
    model.addTreeModelListener(l)
    listeners += l
  }
  override def removeTreeModelListener(l: TreeModelListener): Unit = {
    model.removeTreeModelListener(l)
    listeners -= l
  }

  override def setTree(tree: JTree): Unit = model.setTree(tree)
}
