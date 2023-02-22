package edg_ide.swing

import edg_ide.proven.ProvenStatus

import java.awt.Component
import javax.swing.JTable
import javax.swing.table.DefaultTableCellRenderer

class ProvenTableRenderer extends DefaultTableCellRenderer {
  override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean,
                                             row: Int, column: Int): Component = {
    val component = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
    value match {
      case cell: BlockProven =>
        component.setForeground(ProvenStatus.colorOf(cell.records.getLatestStatus))
      case _ =>
    }
    component
  }
}
