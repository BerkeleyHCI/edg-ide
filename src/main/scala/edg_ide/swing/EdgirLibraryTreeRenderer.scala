package edg_ide.swing

import com.intellij.icons.AllIcons
import com.intellij.ui.render.LabelBasedRenderer

import java.awt.Component
import javax.swing.JTree


class EdgirLibraryTreeRenderer extends LabelBasedRenderer.Tree {
  override def getTreeCellRendererComponent(tree: JTree, value: Any, sel: Boolean, expanded: Boolean, leaf: Boolean,
                                            row: Int, hasFocus: Boolean): Component = {
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
      case _ =>  // ignored
    }
    component
  }
}
