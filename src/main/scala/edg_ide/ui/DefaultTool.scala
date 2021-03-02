package edg_ide.ui

import com.intellij.openapi.project.Project
import edg.wir.DesignPath

import java.awt.event.MouseEvent


class DefaultTool(val project: Project, val interface: ToolInterface) extends BaseTool {
  // Mouse event that is generated on any mouse event in either the design tree or graph layout
  override def onBlockMouse(e: MouseEvent, path: DesignPath): Unit = {

  }

  // Event that is generated when the tree selection changes.
  override def onSelect(path: DesignPath): Unit = {

  }
}
