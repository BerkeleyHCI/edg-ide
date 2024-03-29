package edg_ide.swing

import com.intellij.icons.AllIcons
import com.intellij.ui.JBColor
import com.intellij.ui.treeStructure.treetable.{TreeTable, TreeTableCellRenderer, TreeTableTree}
import edg_ide.proven.ProvenStatus
import icons.PlatformDebuggerImplIcons

import java.awt.Component
import javax.swing.table.{DefaultTableCellRenderer, TableCellRenderer}
import javax.swing.{JTable, JTree}
import javax.swing.tree.DefaultTreeCellRenderer

class EdgirLibraryTreeRenderer extends DefaultTreeCellRenderer {
  override def getTreeCellRendererComponent(
      tree: JTree,
      value: Any,
      sel: Boolean,
      expanded: Boolean,
      leaf: Boolean,
      row: Int,
      hasFocus: Boolean
  ): Component = {
    val component = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus)
    value match {
      case node: EdgirLibraryNodeBase =>
        if (node.traits.contains(EdgirLibraryNodeTraits.Category)) {
          setIcon(AllIcons.Nodes.Folder)
        } else if (node.traits.contains(EdgirLibraryNodeTraits.Abstract)) {
          setIcon(AllIcons.Hierarchy.Subtypes)
        } else {
          setIcon(null)
        }
      case _ => // ignored
    }
    component
  }
}
