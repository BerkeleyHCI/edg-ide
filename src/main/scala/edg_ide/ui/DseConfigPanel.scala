package edg_ide.ui

import com.intellij.ui.components.JBScrollPane
import com.intellij.ui.treeStructure.treetable.TreeTable
import edg_ide.swing.CompilerErrorTreeTableModel

import java.awt.BorderLayout
import javax.swing.JPanel


class DseConfigPanel extends JPanel {
  private val tree = new TreeTable(new CompilerErrorTreeTableModel(Seq()))
  tree.setShowColumns(true)
  tree.setRootVisible(false)
  private val treeScrollPane = new JBScrollPane(tree)

  setLayout(new BorderLayout())
  add(treeScrollPane)
}
