package edg_ide.swing

import java.awt.Point
import java.awt.event.{MouseAdapter, MouseEvent, MouseWheelEvent}
import javax.swing.{JComponent, JScrollPane, SwingUtilities}

trait Zoomable extends JComponent {

  /** Sets the zoom level of this component. 1 is default. Origin should stay constant across zoom levels.
    */
  def setZoom(zoom: Float): Unit
  def getZoom: Float
}

trait ZoomDragScrollPanel extends JScrollPane {
  val zoomable: Zoomable
  var dragOrigin: Option[Point] = None // mouse origin, viewport origin

  override protected def processMouseWheelEvent(e: MouseWheelEvent): Unit = {
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

  def makeMouseAdapter: MouseAdapter = new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = {
      if (SwingUtilities.isLeftMouseButton(e)) {
        dragOrigin = Some(e.getPoint)
      }
    }

    override def mouseReleased(e: MouseEvent): Unit = {
      dragOrigin = None
    }

    override def mouseDragged(e: MouseEvent): Unit = {
      if (SwingUtilities.isLeftMouseButton(e)) {
        dragOrigin.foreach { dragOrigin =>
          val viewPos = getViewport.getViewPosition
          val newViewPos = new Point(
            (viewPos.getX + (dragOrigin.getX - e.getX)).toInt,
            (viewPos.getY + (dragOrigin.getY - e.getY)).toInt
          )
          getViewport.setViewPosition(newViewPos)
          zoomable.revalidate()
        }
      }
    }
  }
}
