package edg_ide.ui

import edg.util.Errorable

import java.awt.event.ActionEvent
import javax.swing.{JMenu, JMenuItem}


object ContextMenuUtils {
  def MenuItem(action: () => Unit, label: String): JMenuItem = {
    val item = new JMenuItem(label)
    item.addActionListener((e: ActionEvent) => {
      action()
    })
    item
  }

  def MenuItemFromErrorable(action: Errorable[() => Unit], label: String): JMenuItem = {
    val item = action match {
      case Errorable.Success(action) =>
        val item = new JMenuItem(label)
        item.addActionListener((e: ActionEvent) => {
          action()
        })
        item
      case Errorable.Error(msg) =>
        val item = new JMenuItem(s"$label ($msg)")
        item.setEnabled(false)
        item
    }
    item
  }

  def MenuItemsFromErrorableSeq[T](actions: Errorable[Seq[(String, () => Unit)]],
                                   errorLabel: String): Seq[JMenuItem] = {
    actions match {
      case Errorable.Success(actions) =>
        actions.map { case (label, action) =>
          val item = new JMenuItem(label)
          item.addActionListener((e: ActionEvent) => {
            action()
          })
          item
        }
      case Errorable.Error(msg) =>
        val item = new JMenuItem(s"$errorLabel ($msg)")
        item.setEnabled(false)
        Seq(item)
    }
  }
}
