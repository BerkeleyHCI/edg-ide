package edg_ide.ui

import java.awt.BorderLayout

import com.intellij.ui.components.JBScrollPane
import com.intellij.ui.treeStructure.treetable.TreeTable
import edg_ide.swing.{CompilerErrorTreeTableModel, FootprintBrowserTreeTableModel}
import javax.swing.{JPanel, JScrollPane, JTextArea}

class FootprintBrowser extends JPanel {

  // TODO initialize to different value / have a textbox for user to select root note
  private val tree = new TreeTable(new FootprintBrowserTreeTableModel("."))
  tree.setShowColumns(true)

  private val treeScrollPane = new JBScrollPane(tree)
  setLayout(new BorderLayout)

  add(treeScrollPane)


}
