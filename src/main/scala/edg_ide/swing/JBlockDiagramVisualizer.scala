package edg_ide.swing

import org.eclipse.elk.graph.{ElkGraphElement, ElkNode}

import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.{Dimension, Graphics, Rectangle}
import javax.swing.{JComponent, Scrollable}
import scala.collection.mutable


/** Block diagram visualizer that customizes the rendering with options specific to
 * design block diagrams:
 * - tunnel link names by heuristic matching (degenerate self-to-self links)
 * - additional setError(elts) to render elts as filled in red
 * - additional setStable(elts) to render elts as stale (???)
 */
class JBlockDiagramVisualizer(var rootNode: ElkNode, var showTop: Boolean = false)
  extends JComponent with Scrollable with Zoomable {
  private val elementToolTips = mutable.Map[ElkGraphElement, String]()
  private val margin: Int = 32 // margin in pixels, regardless of zoom level, so tunnel labels aren't cut off
  protected var highlighted: Option[Set[ElkGraphElement]] = None
  protected var errorElts: Set[ElkGraphElement] = Set()
  protected var staleElts: Set[ElkGraphElement] = Set()
  private var zoomLevel: Float = 1.0f
  private var painter = new ModifiedElkNodePainter(rootNode, showTop)

  override def getZoom = zoomLevel

  override def setZoom(zoom: Float): Unit = {
    zoomLevel = zoom
  }

  def setSelected(elts: Set[ElkGraphElement]): Unit = {
    painter.setSelected(elts)
    validate()
    repaint()
  }

  def setHighlighted(elts: Option[Set[ElkGraphElement]]): Unit = {
    highlighted = elts
    painter.setHighlighted(highlighted)
    validate()
    repaint()
  }

  setGraph(rootNode)

  def getGraph: ElkNode = rootNode

  def setGraph(newGraph: ElkNode): Unit = {
    errorElts = Set()
    staleElts = Set()
    painter = new ModifiedElkNodePainter(newGraph, showTop, zoomLevel)
    elementToolTips.clear()
    highlighted = None
    painter.setSelected(Set())
    rootNode = newGraph
    revalidate()
    repaint()
  }

  def setError(elts: Set[ElkGraphElement]): Unit = {
    errorElts = elts
    validate()
    repaint()
  }

  def setStale(elts: Set[ElkGraphElement]): Unit = {
    staleElts = elts
    validate()
    repaint()
  }

  def getElementForLocation(x: Int, y: Int): Option[ElkGraphElement] = {
    painter.getElementForLocation(x, y)
  }

  override def paintComponent(paintGraphics: Graphics): Unit = {
    painter = new ModifiedElkNodePainter(rootNode, showTop, zoomLevel, errorElts, staleElts)
    painter.paintComponent(paintGraphics, this.getBackground)
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
    painter.getElementForLocation(e.getX, e.getY) match {
      case None => null
      case Some(element) => elementToolTips.get(element) match {
        case None => null
        case Some(text) => text
      }
    }
  }

  override def getPreferredScrollableViewportSize: Dimension = getPreferredSize

  // Scrollable APIs
  //
  override def getPreferredSize: Dimension =
    new Dimension((rootNode.getWidth * zoomLevel + 2 * margin).toInt,
      (rootNode.getHeight * zoomLevel + 2 * margin).toInt)

  override def getScrollableBlockIncrement(rectangle: Rectangle, i: Int, i1: Int): Int = 1

  override def getScrollableUnitIncrement(rectangle: Rectangle, i: Int, i1: Int): Int = 1

  override def getScrollableTracksViewportWidth: Boolean = false

  override def getScrollableTracksViewportHeight: Boolean = false
}
