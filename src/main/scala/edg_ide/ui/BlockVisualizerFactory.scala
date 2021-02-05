package edg_ide.ui

import com.intellij.openapi.project.Project
import com.intellij.openapi.wm.{ToolWindow, ToolWindowFactory}
import com.intellij.ui.content.ContentFactory


class BlockVisualizerFactory extends ToolWindowFactory {
  override def createToolWindowContent(project: Project, toolWindow: ToolWindow): Unit = {
    val contentFactory = ContentFactory.SERVICE.getInstance()
    val content = contentFactory.createContent(new BlockVisualizerPanel(project), "Block Visualizer", false)
    toolWindow.getContentManager.addContent(content)
  }
}
