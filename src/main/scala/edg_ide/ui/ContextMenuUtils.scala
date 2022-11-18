package edg_ide.ui

import edg.util.Errorable

import java.awt.MouseInfo
import java.awt.event.ActionEvent
import javax.swing.JMenuItem


object ContextMenuUtils {
  // Creates a menu item where the action is an Errorable and may fail.
  // On a failure, the error message is displayed as a popup at the mouse location clicking the menu item.
  def ErrorableMenuItem(action: () => Errorable[Unit], label: String): JMenuItem = {
    val item = new JMenuItem(label)
    item.addActionListener((e: ActionEvent) => {
      // store the location before the action, as close to the click location as possible
      val clickLocation = MouseInfo.getPointerInfo.getLocation
      action() match {
        case Errorable.Error(msg) =>
          val (popup, height) = PopupUtils.createErrorPopup(msg)
          popup.showInScreenCoordinates(item, clickLocation)
        case Errorable.Success(_) => // ignored
      }
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
