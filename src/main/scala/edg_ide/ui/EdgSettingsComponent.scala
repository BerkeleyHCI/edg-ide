package edg_ide.ui

import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory
import com.intellij.openapi.options.Configurable
import com.intellij.openapi.ui.TextFieldWithBrowseButton
import com.intellij.ui.components.JBLabel
import com.intellij.util.ui.FormBuilder
import edg_ide.build.BuildInfo

import javax.swing.{JCheckBox, JPanel}

class EdgSettingsComponent {
  val kicadDirectoryText = new TextFieldWithBrowseButton()
  kicadDirectoryText.addBrowseFolderListener(
    null,
    FileChooserDescriptorFactory.createSingleFolderDescriptor()
      .withTitle("Choose KiCad Footprint Directory")
  )
  val kicadDirectoryHelp = new JBLabel(
    "Multiple footprint directories can be separated by semicolons (;). " +
      "IDE restart may be required to take effect."
  )
  kicadDirectoryHelp.setEnabled(false)

  val persistBlockCache = new JCheckBox()
  val persistBlockCacheHelp = new JBLabel(
    "Not recommended. " +
      "Persists compiled blocks across IDE restarts for a faster first compile. " +
      "May not detect HDL changes when the IDE is not running."
  )
  persistBlockCacheHelp.setEnabled(false)

  val showProvenStatus = new JCheckBox()
  val showProvenStatusHelp = new JBLabel(
    "Experimental feature. " +
      "In the design tree and library browser, shows which blocks have been made and tested in actual hardware " +
      "by the project maintainers, from an internal database included locally with the IDE. " +
      "IDE restart may be required to take effect."
  )
  showProvenStatusHelp.setEnabled(false)

  val showInternalBlocks = new JCheckBox()
  val showInternalBlocksHelp = new JBLabel(
    "Show internal blocks like bridges and adapters in the design tree. " +
      "These are implementation details in most cases but may be useful for model debugging. " +
      "IDE restart may be required to take effect."
  )
  showInternalBlocksHelp.setEnabled(false)

  val showIdeErrors = new JCheckBox()
  val showIdeErrorsHelp = new JBLabel(
    "Show detailed IDE internal errors in the run console. " +
      "This is generally only useful for developing the IDE itself and may add excessive clutter for HDL work."
  )
  showIdeErrorsHelp.setEnabled(false)

  val versionLabel = new JBLabel(
    s"Version ${BuildInfo.version}, built at ${BuildInfo.builtAtString}, " +
      s"scala ${BuildInfo.scalaVersion}, sbt ${BuildInfo.sbtVersion}"
  )
  versionLabel.setEnabled(false)

  val mainPanel = FormBuilder
    .createFormBuilder()
    .addLabeledComponent(new JBLabel("KiCad Footprint Directory"), kicadDirectoryText, false)
    .addComponent(kicadDirectoryHelp)
    .addLabeledComponent(new JBLabel("Persist Block Cache"), persistBlockCache, false)
    .addComponent(persistBlockCacheHelp)
    .addLabeledComponent(new JBLabel("Show Proven Status"), showProvenStatus, false)
    .addComponent(showProvenStatusHelp)
    .addLabeledComponent(new JBLabel("Show Internal Blocks"), showInternalBlocks, false)
    .addComponent(showInternalBlocksHelp)
    .addLabeledComponent(new JBLabel("Show IDE Errors"), showIdeErrors, false)
    .addComponent(showIdeErrorsHelp)
    .addComponentFillVertically(new JPanel(), 0)
    .addComponent(versionLabel)
    .getPanel
}

// "controller" for settings
class EdgSettingsConfigurable extends Configurable {
  private val component = new EdgSettingsComponent()

  override def getDisplayName: String = "EDG IDE"

  override def createComponent(): JPanel = component.mainPanel

  override def isModified: Boolean = {
    val settings = EdgSettingsState.getInstance()
    !settings.kicadDirectories.sameElements(component.kicadDirectoryText.getText.split(";")) ||
    settings.persistBlockCache != component.persistBlockCache.isSelected ||
    settings.showProvenStatus != component.showProvenStatus.isSelected ||
    settings.showInternalBlocks != component.showInternalBlocks.isSelected ||
    settings.showIdeErrors != component.showIdeErrors.isSelected
  }

  override def apply(): Unit = {
    val settings = EdgSettingsState.getInstance()
    settings.kicadDirectories = component.kicadDirectoryText.getText.split(";")
    settings.persistBlockCache = component.persistBlockCache.isSelected
    settings.showProvenStatus = component.showProvenStatus.isSelected
    settings.showInternalBlocks = component.showInternalBlocks.isSelected
    settings.showIdeErrors = component.showIdeErrors.isSelected
  }

  override def reset(): Unit = {
    val settings = EdgSettingsState.getInstance()
    component.kicadDirectoryText.setText(settings.kicadDirectories.mkString(";"))
    component.persistBlockCache.setSelected(settings.persistBlockCache)
    component.showProvenStatus.setSelected(settings.showProvenStatus)
    component.showInternalBlocks.setSelected(settings.showInternalBlocks)
    component.showIdeErrors.setSelected(settings.showIdeErrors)
  }
}
