package edg_ide.ui

import com.intellij.ide.ui.newItemPopup.NewItemPopupUtil
import com.intellij.ide.ui.newItemPopup.NewItemSimplePopupPanel
import com.intellij.openapi.project.Project


object PopupUtils {
  def createStringEntryPopup(title: String, project: Project)(accept: String => Unit): Unit = {
    val contentPanel = new NewItemSimplePopupPanel
    val nameField = contentPanel.getTextField
    val popup = NewItemPopupUtil.createNewItemPopup(title, contentPanel, nameField)
    contentPanel.setApplyAction { event =>
      val input = nameField.getText()
      if (input.isEmpty) {  // TODO custom validation function
        contentPanel.setError("empty")
      } else {
        popup.closeOk(event)
        accept(input)
      }
    }
    popup.showCenteredInCurrentWindow(project)
  }
}
