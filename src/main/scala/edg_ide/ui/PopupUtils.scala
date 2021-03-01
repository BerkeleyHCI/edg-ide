package edg_ide.ui

import com.intellij.ide.ui.newItemPopup.NewItemPopupUtil
import com.intellij.ide.ui.newItemPopup.NewItemSimplePopupPanel
import com.intellij.openapi.project.Project
import edg.util.Errorable


object PopupUtils {
  def createStringEntryPopup(title: String, project: Project)(accept: String => Errorable[Unit]): Unit = {
    val contentPanel = new NewItemSimplePopupPanel
    val nameField = contentPanel.getTextField
    val popup = NewItemPopupUtil.createNewItemPopup(title, contentPanel, nameField)
    contentPanel.setApplyAction { event =>
      val input = nameField.getText()
      accept(input) match {
        case Errorable.Success(_) => popup.closeOk(event)
        case Errorable.Error(msg) => contentPanel.setError(msg)
      }
    }
    popup.showCenteredInCurrentWindow(project)
  }
}
