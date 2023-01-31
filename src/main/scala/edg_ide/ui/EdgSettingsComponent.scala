package edg_ide.ui

import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory
import com.intellij.openapi.options.Configurable
import com.intellij.openapi.ui.TextFieldWithBrowseButton
import com.intellij.ui.components.JBLabel
import com.intellij.util.ui.FormBuilder

import javax.swing.{JCheckBox, JPanel}


class EdgSettingsComponent {
  val kicadDirectoryText = new TextFieldWithBrowseButton()
  kicadDirectoryText.addBrowseFolderListener(
    "Choose KiCad Footprint Directory", "", null,
    FileChooserDescriptorFactory.createSingleFolderDescriptor()
  )
  val kicadDirectoryHelp = new JBLabel("IDE restart may be required to take effect.")
  kicadDirectoryHelp.setEnabled(false)
  val persistBlockCache = new JCheckBox()
  val persistBlockCacheHelp = new JBLabel("Not recommended. "
    + "Persists compiled blocks across IDE restarts for a faster first compile. "
    + "May not detect HDL changes when the IDE is not running.")
  persistBlockCacheHelp.setEnabled(false)

  val mainPanel = FormBuilder.createFormBuilder()
      .addLabeledComponent(new JBLabel("KiCad Footprint Directory"), kicadDirectoryText, false)
      .addComponent(kicadDirectoryHelp)
      .addLabeledComponent(new JBLabel("Persist Block Cache"), persistBlockCache, false)
      .addComponent(persistBlockCacheHelp)
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
    settings.kicadDirectories.sameElements(component.kicadDirectoryText.getText.split(";"))
    settings.persistBlockCache != component.persistBlockCache.isSelected
  }

  override def apply(): Unit = {
    val settings = EdgSettingsState.getInstance()
    settings.kicadDirectories = component.kicadDirectoryText.getText.split(";")
    settings.persistBlockCache = component.persistBlockCache.isSelected
  }

  override def reset(): Unit = {
    val settings = EdgSettingsState.getInstance()
    component.kicadDirectoryText.setText(settings.kicadDirectories.mkString(";"))
    component.persistBlockCache.setSelected(settings.persistBlockCache)
  }
}
