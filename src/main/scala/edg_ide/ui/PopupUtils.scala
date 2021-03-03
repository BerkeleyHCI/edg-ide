package edg_ide.ui

import com.intellij.ide.ui.newItemPopup.NewItemPopupUtil
import com.intellij.ide.ui.newItemPopup.NewItemSimplePopupPanel
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.{ComponentValidator, ValidationInfo}
import com.intellij.ui.scale.JBUIScale
import edg.util.Errorable

import java.awt.Point
import java.awt.event.MouseEvent
import javax.swing.{JComponent, JEditorPane}


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

  def createErrorPopup(message: String, e: MouseEvent): Unit = {
    var hintHeight: Int = 0
    val popupBuilder = ComponentValidator.createPopupBuilder(
      new ValidationInfo(message, e.getComponent.asInstanceOf[JComponent]),
      (editorPane: JEditorPane) => {
        hintHeight = editorPane.getPreferredSize.height
      }
    )   .setCancelOnWindowDeactivation(false)
        .setCancelOnClickOutside(true)
        .addUserData("SIMPLE_WINDOW")

    val myErrorPopup = popupBuilder.createPopup
    myErrorPopup.showInScreenCoordinates(e.getComponent,
      new Point(e.getXOnScreen, e.getYOnScreen - JBUIScale.scale(6) - hintHeight))
  }
}
