package edg_ide.lang

import com.intellij.openapi.fileTypes.LanguageFileType
import javax.swing._

import edg_ide.EdgBundle
import edg_ide.EdgIcons


object EdgFileType extends LanguageFileType(EdgLanguage) {
  final val INSTANCE = this

  def getName(): String = "EDG"
  def getDescription(): String = "EDG"  // should this refer to the bundle?
  def getDefaultExtension(): String = "py"
  def getIcon(): Icon = EdgIcons.EdgFileIcon
}
