package edg_ide.swing

import com.intellij.icons.AllIcons
import com.intellij.ui.JBColor
import com.intellij.ui.treeStructure.treetable.{TreeTable, TreeTableCellRenderer, TreeTableTree}
import icons.PlatformDebuggerImplIcons

import java.awt.Component
import javax.swing.table.{DefaultTableCellRenderer, TableCellRenderer}
import javax.swing.{JTable, JTree}
import javax.swing.tree.DefaultTreeCellRenderer

class EdgirLibraryTreeRenderer extends DefaultTreeCellRenderer {
  override def getTreeCellRendererComponent(tree: JTree, value: Any, sel: Boolean, expanded: Boolean, leaf: Boolean,
                                            row: Int, hasFocus: Boolean): Component = {
    val component = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus)
    value match {
      case node: EdgirLibraryNodeBase =>
        if (node.traits.contains(EdgirLibraryNodeTraits.Category)) {
          setIcon(AllIcons.Nodes.Folder)
        } else if (node.traits.contains(EdgirLibraryNodeTraits.Abstract)) {
          setIcon(AllIcons.Hierarchy.Subtypes)
        } else if (node.traits.contains(EdgirLibraryNodeTraits.Footprint)) {
          setIcon(PlatformDebuggerImplIcons.MemoryView.Active)
        } else {
          setIcon(null)
        }
      case _ =>  // ignored
    }
    component
  }
}


class EdgirLibraryTableRenderer extends DefaultTableCellRenderer {
  override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean,
                                             row: Int, column: Int): Component = {
    val component = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
    println(value)
    component.setForeground(JBColor.GREEN)
    component
  }
}
