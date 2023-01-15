package edg_ide.ui

import com.intellij.ide.ui.newItemPopup.{NewItemPopupUtil, NewItemSimplePopupPanel}
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.popup.{JBPopup, JBPopupFactory}
import com.intellij.openapi.ui.{ComponentValidator, ValidationInfo}
import com.intellij.ui.scale.JBUIScale
import edg.util.Errorable

import java.awt.Point
import java.awt.event.MouseEvent
import javax.swing.JEditorPane
import scala.jdk.CollectionConverters.SeqHasAsJava


object PopupUtils {
  def createMenuPopup[T](title: String, elts: Seq[T], editor: Editor)(accept: T => Unit): Unit = {
    val popup = JBPopupFactory.getInstance().createPopupChooserBuilder(elts.asJava)
        .setTitle(title)
        .setItemChosenCallback((t: T) => {
          accept(t)
        })
        .createPopup()
    popup.showInBestPositionFor(editor)
  }

  // TODO unify & dedup!
  def createMenuPopup[T](title: String, elts: Seq[T], project: Project)(accept: T => Unit): Unit = {
    val popup = JBPopupFactory.getInstance().createPopupChooserBuilder(elts.asJava)
        .setTitle(title)
        .setItemChosenCallback((t: T) => {
          accept(t)
        })
        .createPopup()
    popup.showCenteredInCurrentWindow(project)
  }

  def createMenuPopup[T](title: String, elts: Seq[T], e: MouseEvent)(accept: T => Unit): Unit = {
    val popup = JBPopupFactory.getInstance().createPopupChooserBuilder(elts.asJava)
        .setTitle(title)
        .setItemChosenCallback((t: T) => {
          accept(t)
        })
        .createPopup()
    popup.showInScreenCoordinates(e.getComponent, new Point(e.getXOnScreen, e.getYOnScreen))
  }

  def createStringEntryPopup(title: String, project: Project, initialValue: String = "")
                            (accept: String => Errorable[Unit]): Unit = {
    val contentPanel = new NewItemSimplePopupPanel
    val nameField = contentPanel.getTextField
    nameField.setText(initialValue)
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

  // creates an error popup without showing it
  private def createErrorPopupRaw(message: String): (JBPopup, Int) = {
    var hintHeight: Int = 0
    val popupBuilder = ComponentValidator.createPopupBuilder(
      new ValidationInfo(message, null),  // TODO support component?
      (editorPane: JEditorPane) => {
        hintHeight = editorPane.getPreferredSize.height
      }
    )   .setCancelOnWindowDeactivation(false)
        .setCancelOnClickOutside(true)
        .addUserData("SIMPLE_WINDOW")

    (popupBuilder.createPopup, hintHeight)
  }

  // creates and shows an error popup at some point in screen coordinates
  // point will be the top left of the popup
  def createErrorPopup(message: String, owner: java.awt.Component, point: Point): Unit = {
    val (popup, height) = createErrorPopupRaw(message)
    val newPoint = new Point(point)
    newPoint.translate(0, height)
    popup.showInScreenCoordinates(owner, newPoint)
  }

  def createErrorPopup(message: String, e: MouseEvent): Unit = {
    val (popup, height) = createErrorPopupRaw(message)
    popup.showInScreenCoordinates(e.getComponent,
      new Point(e.getXOnScreen, e.getYOnScreen - JBUIScale.scale(6) - height))
  }

  def createErrorPopup(message: String, editor: Editor): Unit = {
    val (popup, height) = createErrorPopupRaw(message)
    popup.showInBestPositionFor(editor)
  }
}
