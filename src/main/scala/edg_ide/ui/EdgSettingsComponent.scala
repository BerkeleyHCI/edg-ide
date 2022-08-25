package edg_ide.ui

import com.intellij.openapi.options.Configurable
import com.intellij.ui.components.{JBLabel, JBTextField}
import com.intellij.util.ui.FormBuilder

import javax.swing.JPanel


class EdgSettingsComponent {
  val kicadDirectoryText = new JBTextField()

  val mainPanel = FormBuilder.createFormBuilder()
      .addLabeledComponent(new JBLabel("KiCad Footprint Directory"), kicadDirectoryText, false)
      .addComponentFillVertically(new JPanel(), 0)
      .getPanel

}


// "controller" for settings
class EdgSettingsConfigurable extends Configurable {
  private val component = new EdgSettingsComponent()

  override def getDisplayName: String = "EDG IDE"

  override def createComponent(): JPanel = component.mainPanel

  override def isModified: Boolean = {
    val settings = EdgSettingsState.getInstance()
    settings.kicadDirectory == component.kicadDirectoryText.getText
  }

  override def apply(): Unit = {
    val settings = EdgSettingsState.getInstance()
    settings.kicadDirectory = component.kicadDirectoryText.getText
  }

  override def reset(): Unit = {
    val settings = EdgSettingsState.getInstance()
    component.kicadDirectoryText.setText(settings.kicadDirectory)
  }
}
