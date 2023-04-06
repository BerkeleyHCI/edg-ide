package edg_ide.ui

import com.intellij.ui.JBColor

import java.awt.Component
import javax.swing.table.DefaultTableCellRenderer
import javax.swing.tree.DefaultTreeCellRenderer
import javax.swing.{JTable, JTree}


class StaleTreeRenderer extends DefaultTreeCellRenderer {
  override def getTreeCellRendererComponent(tree: JTree, value: Any, sel: Boolean, expanded: Boolean, leaf: Boolean,
                                            row: Int, hasFocus: Boolean): Component = {
    val component = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus)
    component.setForeground(JBColor.GRAY)
    setIcon(null)
    component
  }
}


class StaleTableRenderer extends DefaultTableCellRenderer {
  override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean,
                                             row: Int, column: Int): Component = {
    val component = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
    component.setForeground(JBColor.GRAY)
    component
  }
}
