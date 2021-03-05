package edg_ide.ui

import edg.util.Errorable

import java.awt.event.ActionEvent
import javax.swing.{JMenu, JMenuItem}


object PopupMenuUtils {
  def MenuItemFromErrorable[T](obj: Errorable[T], label: String)(actionFn: T => Unit): JMenuItem = {
    val item = obj match {
      case Errorable.Success(obj) =>
        val item = new JMenuItem(label)
        item.addActionListener((e: ActionEvent) => {
          actionFn(obj)
        })
        item
      case Errorable.Error(msg) =>
        val item = new JMenuItem(s"$label ($msg)")
        item.setEnabled(false)
        item
    }
    item
  }

  def MenuItemsFromErrorableSeq[T](objs: Errorable[Seq[T]], labelFn: T => String, errorLabel: String)
                                  (actionFn: T => Unit): Seq[JMenuItem] = {
    objs match {
      case Errorable.Success(objs) =>
        objs.map { obj =>
          val item = new JMenuItem(labelFn(obj))
          item.addActionListener((e: ActionEvent) => {
            actionFn(obj)
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
