package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.{TreeTable, TreeTableModel}

import javax.swing.JTree
import javax.swing.tree.TreePath
import scala.collection.mutable

// Utility methods to preserve user-visible state when the tree model is updated.
// The clean solution would be to use TreeModelListener events, but these don't seem to behave
// when the underlying node structure is modified, even if it's mostly isomorphic.
object TreeTableUtils {
  def updateModel(treeTable: TreeTable, newModel: TreeTableModel): Unit = {
    val savedNodes = getExpandedNodes(treeTable.getTree)
    val isRootVisible = treeTable.getTree.isRootVisible
    val savedRenderer = treeTable.getTree.getCellRenderer
    treeTable.setModel(
      newModel
    ) // note that setModel resets TreeTable.getTree, so we need to get a fresh tree handle
    restoreExpandedNodes(treeTable.getTree, savedNodes)
    treeTable.setRootVisible(isRootVisible)
    treeTable.getTree.setCellRenderer(savedRenderer)
  }

  // Returns the tree path of all expanded nodes (as sequence of node objects).
  protected def getExpandedNodes(tree: JTree): Seq[TreePath] = {
    val expandedNodes = mutable.ListBuffer[TreePath]()
    val model = tree.getModel

    def traverse(path: TreePath): Unit = {
      val thisNode = path.getLastPathComponent
      if (tree.isExpanded(path)) { // only recurse if expanded
        expandedNodes.append(path)
        (0 until model.getChildCount(thisNode)).map(model.getChild(thisNode, _)).foreach { childNode =>
          val childPath = path.pathByAddingChild(childNode)
          traverse(childPath)
        }
      }
    }
    traverse(new TreePath(model.getRoot))

    expandedNodes.toSeq
  }

  // Restores expanded node state, by walking the tree and comparing node approximate-equality.
  protected def restoreExpandedNodes(tree: JTree, expanded: Seq[TreePath]): Unit = {
    // This is pretty computationally inefficient. Storing the expanded nodes in a tree structure can be
    // much more efficient, but this is the lazy standard-classes based solution.
    val model = tree.getModel

    // Checks whether to expand the node at treePath, then recursively calls with filtered expandedPaths for each child
    def traverse(treePath: TreePath, expandedPaths: Seq[TreePath]): Unit = {
      val treeNode = treePath.getLastPathComponent
      val expandedByThisNode = expandedPaths.groupBy(_.getPathComponent(treePath.getPathCount - 1))

      expandedByThisNode.foreach { case (expandedNode, expandedPaths) =>
        // while it's theoretically possible to get multiple expandedNode matches this is probably unlikely
        if (treeNode.toString == expandedNode.toString) {
          tree.expandPath(treePath)
          val treeNodeChildren = (0 until model.getChildCount(treeNode)).map(model.getChild(treeNode, _))
          treeNodeChildren.foreach { childNode =>
            val childExpandedPaths = expandedPaths.filter(_.getPathCount > treePath.getPathCount)
            traverse(treePath.pathByAddingChild(childNode), childExpandedPaths)
          }
        }
      }
    }
    traverse(new TreePath(model.getRoot), expanded)
  }

  // Returns the TreePath for a location where the entire row is valid, instead of the getPathForLocation behavior
  // which must hit on the text.
  def getPathForRowLocation(treeTable: TreeTable, x: Int, y: Int): Option[TreePath] = {
    val nearestRow = treeTable.getTree.getClosestRowForLocation(x, y)
    if (nearestRow < 0) {
      return None
    }
    val nearestRowBounds = treeTable.getTree.getRowBounds(nearestRow)
    Option(treeTable.getTree.getPathForLocation(nearestRowBounds.x, y))
  }
}
