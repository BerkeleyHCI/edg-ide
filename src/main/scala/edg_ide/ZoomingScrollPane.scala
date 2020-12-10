package edg_ide

import com.intellij.ui.components.JBScrollPane

import java.awt.event.MouseWheelEvent
import javax.swing.JComponent


trait Zoomable extends JComponent {
  /**
    * Sets the zoom level of this component. 1 is default.
    * Origin should stay constant across zoom levels.
    */
  def setZoom(zoom: Float): Unit
  def getZoom: Float
}


trait ZoomingScrollPane extends JBScrollPane {
  override protected def processMouseWheelEvent(e: MouseWheelEvent) {
    val zoomable = getViewport.getView.asInstanceOf[Zoomable]  // crash otherwise

    val newZoom = zoomable.getZoom * Math.pow(1.1, -1 * e.getPreciseWheelRotation)
    zoomable.setZoom(newZoom.toFloat)
    zoomable.revalidate()
    zoomable.repaint()
  }
}