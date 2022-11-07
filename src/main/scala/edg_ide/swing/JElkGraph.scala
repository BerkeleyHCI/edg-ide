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

  def blendColor(baseColor: Color, topColor: Color, factor: Double): Color = {
    GraphicsPaintingUtil.blendColor(baseColor, topColor, factor)
  }

  protected def fillGraphics(base: Graphics2D, background: Color, element: ElkGraphElement): Graphics2D = {
    GraphicsPaintingUtil.fillGraphics(base, background, element)
  }

  protected def paintEdge(parentG: Graphics2D, blockG: Graphics2D, nodeBackground: Color, edge: ElkEdge): Unit = {
    GraphicsPaintingUtil.paintEdge(parentG, blockG, nodeBackground, edge)
  }

  protected def textGraphics(base: Graphics2D, background: Color, element: ElkGraphElement): Graphics2D = {
    GraphicsPaintingUtil.textGraphics(base, background, element)
  }

  def getElementForLocation(x: Int, y: Int): Option[ElkGraphElement] = {
    GraphicsPaintingUtil.getElementForLocation(x, y)
  }

  override def paintComponent(paintGraphics: Graphics): Unit = {
    println(rootNode.getWidth + " , " + rootNode.getHeight)
    GraphicsPaintingUtil.setRootNode(rootNode)
    GraphicsPaintingUtil.setShowTop(showTop)
    GraphicsPaintingUtil.setSelected(selected)
    GraphicsPaintingUtil.setHighlighted(highlighted)
    GraphicsPaintingUtil.paintComponent(paintGraphics, this.getBackground)
  }

  // support for mouse drag: https://docs.oracle.com/javase/tutorial/uiswing/components/scrollpane.html
  setAutoscrolls(true)
  // TODO proper drag support



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
    GraphicsPaintingUtil.getElementForLocation(e.getX, e.getY) match {
      case None => null
      case Some(element) => elementToolTips.get(element) match {
        case None => null
        case Some(text) => text
      }
    }
//    getElementForLocation(e.getX, e.getY) match {
//      case None => null
//      case Some(element) => elementToolTips.get(element) match {
//        case None => null
//        case Some(text) => text
//      }
//    }
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
