package edg_ide.ui

import edg_ide.swing.DrawAnchored

import java.awt.{Color, Graphics, Graphics2D}
import java.awt.event.MouseWheelEvent
import javax.swing.JPanel

class KicadVizDrawPanel extends JPanel {
  val kicadParser = new KicadParser(".")
  // TODO offset and offset_mul_factor are messy, should be in one var
  var offset = 0
  var offset_mul_factor = 0.1
  var mul_factor: Int = 10

  addMouseWheelListener((mouseWheelEvent: MouseWheelEvent) => {
    mul_factor += mouseWheelEvent.getWheelRotation
    mul_factor = math.max(mul_factor, 1)
    this.repaint()
  })

  var pinmap: Map[String, String] = Map()  // TODO shouldn't be public, should be set alongside the file

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)

    // TODO don't reparse kicad file on gui update?
    val components = kicadParser.parseKicadFile()

    if (components.isEmpty)
      return

    val min_x = components.map(c => (c match {
      case Rectangle(x, y, width, height, name) => x
      case Line(x0, y0, x1, y1) => math.min(x0, x1)
    })).min.abs

    val min_y = components.map(c => (c match {
      case Rectangle(x, y, width, height, name) => y
      case Line(x0, y0, x1, y1) => math.min(y0, y1)
    })).min.abs

    for (c <- components) {
      c match {
        // Notes
        // 1. Lose precision converting int to float
        // 2. Scale
        // 3. It's negative
        case Line(x0, y0, x1, y1) =>
          g.drawLine(
            (offset + this.getWidth * offset_mul_factor + (min_x + x0) * mul_factor).asInstanceOf[Int],
            ((min_y + y0) * mul_factor).asInstanceOf[Int],
            (offset + this.getWidth * offset_mul_factor + (min_x + x1) * mul_factor).asInstanceOf[Int],
            ((min_y + y1) * mul_factor).asInstanceOf[Int]
          )
        case Rectangle(x, y, width, height, name) =>
          val scaledWidth = (width * mul_factor).asInstanceOf[Int]
          val scaledHeight = (height * mul_factor).asInstanceOf[Int]
          val scaledX = ((offset + this.getWidth * offset_mul_factor + (min_x + x) * mul_factor).asInstanceOf[Int]) - (scaledWidth / 2)
          val scaledY = (((min_y + y) * mul_factor).asInstanceOf[Int]) - (scaledHeight / 2)

          g.drawRect(
            scaledX,
            scaledY,
            scaledWidth,
            scaledHeight)

          val fillG = g.create().asInstanceOf[Graphics2D]  // TODO dedup w/ JElkGraph / JBlockDiagramVisualizer?
          fillG.setColor(new Color(
            fillG.getColor.getRed,
            fillG.getColor.getGreen,
            fillG.getColor.getBlue,
            63
          ))
          fillG.fillRect(
            scaledX,
            scaledY,
            scaledWidth,
            scaledHeight)

          DrawAnchored.drawLabel(g, name,
            (scaledX + scaledWidth / 2, scaledY + scaledHeight / 2),
            DrawAnchored.Bottom)
          DrawAnchored.drawLabel(g, pinmap.getOrElse(name, ""),
            (scaledX + scaledWidth / 2, scaledY + scaledHeight / 2),
            DrawAnchored.Top)
        case _ =>
      }
    }
  }

  def getComponentForLocation(locX: Int, locY: Int): Seq[KicadComponent] = {
    // TODO 'widen' lines so that can be selected w/o pinpoint accuracy - see algo in JElkGraph
    // TODO dedup this code w/ draw, this is so much copypaste =(
    val components = kicadParser.parseKicadFile().toSeq

    if (components.isEmpty)
      return Seq()

    val min_x = components.map(c => (c match {
      case Rectangle(x, y, width, height, name) => x
      case Line(x0, y0, x1, y1) => math.min(x0, x1)
    })).min.abs

    val min_y = components.map(c => (c match {
      case Rectangle(x, y, width, height, name) => y
      case Line(x0, y0, x1, y1) => math.min(y0, y1)
    })).min.abs

    components.flatMap {
      case comp @ Rectangle(x, y, width, height, name) =>
        val scaledWidth = (width * mul_factor).asInstanceOf[Int]
        val scaledHeight = (height * mul_factor).asInstanceOf[Int]
        val scaledX = ((offset + this.getWidth * offset_mul_factor + (min_x + x) * mul_factor).asInstanceOf[Int]) - (scaledWidth / 2)
        val scaledY = (((min_y + y) * mul_factor).asInstanceOf[Int]) - (scaledHeight / 2)
        Option.when(locX >= scaledX && locX <= scaledX+scaledWidth &&
            locY >= scaledY && locY <= scaledY+scaledHeight) {
          comp
        }
      case _ => None
    }
  }
}
