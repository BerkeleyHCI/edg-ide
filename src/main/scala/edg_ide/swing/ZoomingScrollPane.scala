package edg_ide.swing

import com.intellij.ui.components.JBScrollPane

import java.awt.Point
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

    val zoomFactor = Math.pow(1.1, -1 * e.getPreciseWheelRotation)
    zoomable.setZoom((zoomable.getZoom * zoomFactor).toFloat)
    zoomable.revalidate()
    zoomable.repaint()

    // https://stackoverflow.com/questions/13155382/jscrollpane-zoom-relative-to-mouse-position
    // to keep the mouse over the same position in pre-scaled space
    val viewPos = getViewport.getViewPosition
    val newViewPos = new Point(
      (e.getPoint.x * (zoomFactor - 1) + zoomFactor * viewPos.x).toInt,
      (e.getPoint.y * (zoomFactor - 1) + zoomFactor * viewPos.y).toInt
    )
    getViewport.setViewPosition(newViewPos)
  }
}
