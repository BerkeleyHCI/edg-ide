package edg_ide.ui

import edg.util.Errorable

import java.awt.event.ActionEvent
import javax.swing.{JMenu, JMenuItem}


object PopupMenuUtils {
  def MenuItemFromErrorable[T](obj: Errorable[T], label: String)(actionFn: T => Unit): JMenuItem = {
    val item = new JMenuItem(label)
    obj match {
      case Errorable.Success(obj) =>
        item.addActionListener((e: ActionEvent) => {
          actionFn(obj)
        })
      case Errorable.Error(msg) =>
        item.setEnabled(false)
    }
    item
  }

  def MenuItemsFromErrorableSeq[T](objs: Errorable[Seq[T]], errorLabelFn: String => String, labelFn: T => String)
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
        val item = new JMenuItem(errorLabelFn(msg))
        item.setEnabled(false)
        Seq(item)
    }
  }
}
