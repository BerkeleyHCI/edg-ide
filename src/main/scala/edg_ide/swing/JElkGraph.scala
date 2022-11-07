package edg_ide.swing

import edg_ide.runner.PDFGeneratorUtil
import org.eclipse.elk.graph._
import org.eclipse.elk.core.options._

import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.geom.AffineTransform
import java.awt._
import javax.swing.{JComponent, Scrollable}
import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}
import collection.mutable


class JElkGraph(var rootNode: ElkNode, var showTop: Boolean = false)
  extends JComponent with Scrollable with Zoomable {
  private val elementToolTips = mutable.Map[ElkGraphElement, String]()

  private var zoomLevel: Float = 1.0f
  private val margin: Int = 32 // margin in pixels, regardless of zoom level, so tunnel labels aren't cut off

  override def setZoom(zoom: Float): Unit = {
    zoomLevel = zoom
  }

  override def getZoom = zoomLevel



  // Selection operations
  //
  protected var selected: Set[ElkGraphElement] = Set()

  def setSelected(elts: Set[ElkGraphElement]): Unit = {
    selected = elts
    validate()
    repaint()
  }

  protected var highlighted: Option[Set[ElkGraphElement]] = None

  def setHighlighted(elts: Option[Set[ElkGraphElement]]): Unit = {
    highlighted = elts
    validate()
    repaint()
  }

  setGraph(rootNode)

  def setGraph(newGraph: ElkNode): Unit = {
    elementToolTips.clear()
    highlighted = None
    selected = Set()
    rootNode = newGraph
    revalidate()
    repaint()
  }

  def getGraph: ElkNode = rootNode

  // Given a ElkLabel and placement (anchoring) constraints, return the x and y coordinates for where the
  // text should be drawn.
  def transformLabelCoords(g: Graphics2D, label: ElkLabel, placement: Set[NodeLabelPlacement]): (Double, Double) = {
    val fontMetrics = g.getFontMetrics(g.getFont)

    val textWidth = fontMetrics.stringWidth(label.getText)
    val textHeight = fontMetrics.getMaxAscent

    if (Set(NodeLabelPlacement.H_CENTER, NodeLabelPlacement.V_TOP).subsetOf(placement)) {
      (label.getX + label.getWidth / 2 - textWidth / 2, // shift X to centerline
        label.getY + textHeight)
    } else if (Set(NodeLabelPlacement.H_LEFT, NodeLabelPlacement.V_TOP,
      NodeLabelPlacement.OUTSIDE).subsetOf(placement)) { // inside means bottom-anchored
      (label.getX,
        label.getY + label.getHeight)
    } else if (Set(NodeLabelPlacement.H_CENTER, NodeLabelPlacement.V_BOTTOM).subsetOf(placement)) {
      (label.getX + label.getWidth / 2 - textWidth / 2,
        label.getY + label.getHeight)
    } else if (Set(NodeLabelPlacement.H_LEFT, NodeLabelPlacement.V_TOP).subsetOf(placement)) {
      (label.getX,
        label.getY + textHeight)
    } else if (Set(NodeLabelPlacement.H_LEFT, NodeLabelPlacement.V_CENTER).subsetOf(placement)) {
      (label.getX,
        label.getY + label.getHeight / 2 + textHeight / 2)
    } else if (Set(NodeLabelPlacement.H_RIGHT, NodeLabelPlacement.V_CENTER).subsetOf(placement)) {
      (label.getX + label.getWidth - textWidth,
        label.getY + label.getHeight / 2 + textHeight / 2)
    } else { // fallback: center anchored
      (label.getX + label.getWidth / 2 - textWidth / 2,
        label.getY + label.getHeight / 2 + textHeight / 2)
    }
  }

  override def paintComponent(paintGraphics: Graphics): Unit = {
    GraphicsPaintingUtil.paintComponent(paintGraphics)
  }

  // support for mouse drag: https://docs.oracle.com/javase/tutorial/uiswing/components/scrollpane.html
  setAutoscrolls(true)
  // TODO proper drag support

  val EDGE_CLICK_WIDTH = 5.0f // how thick edges are for click detection purposes

  addMouseListener(new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = {
      requestFocusInWindow()
    }
  })

  // Tooltip operations
  //
  def setElementToolTip(element: ElkGraphElement, text: String): Unit = {
    elementToolTips.put(element, text)
  }

  override def getToolTipText(e: MouseEvent): String = {
    getElementForLocation(e.getX, e.getY) match {
      case None => null
      case Some(element) => elementToolTips.get(element) match {
        case None => null
        case Some(text) => text
      }
    }
  }

  // Scrollable APIs
  //
  override def getPreferredSize: Dimension =
    new Dimension((rootNode.getWidth * zoomLevel + 2 * margin).toInt,
      (rootNode.getHeight * zoomLevel + 2 * margin).toInt)

  override def getPreferredScrollableViewportSize: Dimension = getPreferredSize

  override def getScrollableBlockIncrement(rectangle: Rectangle, i: Int, i1: Int): Int = 1

  override def getScrollableUnitIncrement(rectangle: Rectangle, i: Int, i1: Int): Int = 1

  override def getScrollableTracksViewportWidth: Boolean = false

  override def getScrollableTracksViewportHeight: Boolean = false
}
