package edg_ide.ui

import edg_ide.swing.DrawAnchored

import java.awt.{Color, Graphics, Graphics2D}
import java.awt.event.MouseWheelEvent
import javax.swing.JPanel

class KicadVizDrawPanel extends JPanel {
  var kicadFootprint = KicadFootprint(Seq())

  var mul_factor: Int = 10

  addMouseWheelListener((mouseWheelEvent: MouseWheelEvent) => {
    mul_factor -= mouseWheelEvent.getWheelRotation
    mul_factor = math.max(mul_factor, 1)
    this.repaint()
  })

  var pinmap: Map[String, String] = Map()  // TODO shouldn't be public, should be set alongside the file

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)

    val ((min_x, min_y), _) = kicadFootprint.bounds
    for (c <- kicadFootprint.elts) {
      c match {
        // Notes
        // 1. Lose precision converting int to float
        // 2. Scale
        // 3. It's negative
        case Line(x0, y0, x1, y1, layers) if layers.contains("F.SilkS") || layers.contains("F.CrtYd") =>
          g.drawLine(
            ((x0 - min_x) * mul_factor).asInstanceOf[Int],
            ((y0 - min_y) * mul_factor).asInstanceOf[Int],
            ((x1 - min_x) * mul_factor).asInstanceOf[Int],
            ((y1 - min_y) * mul_factor).asInstanceOf[Int]
          )

        case Rectangle(x, y, width, height, name) =>
          val scaledWidth = (width * mul_factor).asInstanceOf[Int]
          val scaledHeight = (height * mul_factor).asInstanceOf[Int]
          val scaledX = ((x - min_x) * mul_factor).asInstanceOf[Int] - (scaledWidth / 2)
          val scaledY = ((y - min_y) * mul_factor).asInstanceOf[Int] - (scaledHeight / 2)

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

        case Oval(x, y, width, height, name) =>  // TODO dedup w/ Rectangle case?
          val scaledWidth = (width * mul_factor).asInstanceOf[Int]
          val scaledHeight = (height * mul_factor).asInstanceOf[Int]
          val scaledX = ((x - min_x) * mul_factor).asInstanceOf[Int] - (scaledWidth / 2)
          val scaledY = ((y - min_y) * mul_factor).asInstanceOf[Int] - (scaledHeight / 2)

          g.drawOval(
            scaledX,
            scaledY,
            scaledWidth,
            scaledHeight)

          val fillG = g.create().asInstanceOf[Graphics2D] // TODO dedup w/ JElkGraph / JBlockDiagramVisualizer?
          fillG.setColor(new Color(
            fillG.getColor.getRed,
            fillG.getColor.getGreen,
            fillG.getColor.getBlue,
            63
          ))
          fillG.fillOval(
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
    val ((min_x, min_y), _) = kicadFootprint.bounds
    kicadFootprint.elts.flatMap {
      case comp @ Rectangle(x, y, width, height, name) =>
        val scaledWidth = (width * mul_factor).asInstanceOf[Int]
        val scaledHeight = (height * mul_factor).asInstanceOf[Int]
        val scaledX = ((x - min_x) * mul_factor).asInstanceOf[Int] - (scaledWidth / 2)
        val scaledY = ((y - min_y) * mul_factor).asInstanceOf[Int] - (scaledHeight / 2)
        Option.when(locX >= scaledX && locX <= scaledX+scaledWidth &&
            locY >= scaledY && locY <= scaledY+scaledHeight) {
          comp
        }
      case comp @ Oval(x, y, width, height, name) =>
        val scaledWidth = (width * mul_factor).asInstanceOf[Int]
        val scaledHeight = (height * mul_factor).asInstanceOf[Int]
        val scaledX = ((x - min_x) * mul_factor).asInstanceOf[Int] - (scaledWidth / 2)
        val scaledY = ((y - min_y) * mul_factor).asInstanceOf[Int] - (scaledHeight / 2)
        Option.when(locX >= scaledX && locX <= scaledX + scaledWidth &&
            locY >= scaledY && locY <= scaledY + scaledHeight) {
          comp
        }
      case _ => None
    }
  }
}
