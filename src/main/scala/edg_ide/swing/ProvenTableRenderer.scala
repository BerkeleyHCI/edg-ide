package edg_ide.swing

import com.intellij.ui.treeStructure.treetable.{TreeTable, TreeTableModel}
import edg_ide.proven.ProvenStatus

import java.awt.Component
import java.awt.event.MouseEvent
import javax.swing.JTable
import javax.swing.table.DefaultTableCellRenderer
import scala.jdk.CollectionConverters.EnumerationHasAsScala

// Mixin that adds hover tooltip and cell coloring for TreeTable cells with a Proven column
trait ProvenTreeTableMixin extends TreeTable {
  val tableRenderer = new ProvenTableRenderer
  setDefaultRenderer(classOf[ProvenNodeBase], tableRenderer)

  override def getToolTipText(e: MouseEvent): String = {
    getValueAt(rowAtPoint(e.getPoint), columnAtPoint(e.getPoint)) match {
      case cell: BlockProven => SwingHtmlUtil.wrapInHtml(cell.htmlDescription, getFont)
      case _ => super.getToolTipText(e)
    }
  }

  override def setModel(treeTableModel: TreeTableModel): Unit = {
    super.setModel(treeTableModel)
    getColumnModel.getColumns.asScala.foreach { column =>  // must support the case where the column isn't shown
      if (column.getIdentifier == "Proven") {
        column.setPreferredWidth(32)
      }
    }
  }
}

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
