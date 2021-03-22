package edg_ide.swing

import com.intellij.icons.AllIcons
import icons.PlatformDebuggerImplIcons

import java.awt.Component
import javax.swing.JTree
import javax.swing.tree.DefaultTreeCellRenderer

class EdgirLibraryTreeRenderer extends DefaultTreeCellRenderer {
  override def getTreeCellRendererComponent(tree: JTree, value: Any, sel: Boolean, expanded: Boolean, leaf: Boolean,
                                            row: Int, hasFocus: Boolean): Component = {
    val component = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus)
    value match {
      case node: EdgirLibraryTreeNode =>
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
