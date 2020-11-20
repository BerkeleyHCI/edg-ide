package edg_ide

import com.intellij.codeHighlighting.BackgroundEditorHighlighter
import com.intellij.ide.structureView.StructureViewBuilder
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.editor.Document
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.editor.colors.EditorFontType
import com.intellij.openapi.editor.impl.EditorImpl
import com.intellij.openapi.fileEditor._
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Disposer
import com.intellij.openapi.util.Key
import com.intellij.openapi.util.Pair
import com.intellij.openapi.util.UserDataHolderBase
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.pom.Navigatable
import com.intellij.ui.JBColor
import com.intellij.ui.JBSplitter
import com.intellij.util.ui.ImageUtil
import org.jetbrains.annotations.NotNull
import org.jetbrains.annotations.Nullable

import javax.imageio.ImageIO
import javax.swing._
import java.awt._
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import java.beans.PropertyChangeEvent
import java.beans.PropertyChangeListener
import java.io._
import java.util._
import java.util


class SplitFileEditor(private val textEditor: FileEditor, private val file: VirtualFile)
    extends UserDataHolderBase with TextEditor {

  override def getName() = "PyCharm with EDG Live Visualization"

}
