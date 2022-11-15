package edg_ide.swing

import org.eclipse.elk.graph.ElkEdgeSection
import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}

object ElkNodeUtil {
  // Utility functions
  def edgeSectionPairs[T](section: ElkEdgeSection): Seq[((Double, Double), (Double, Double))] = {
    val start = (section.getStartX, section.getStartY)
    val end = (section.getEndX, section.getEndY)
    val bends = section.getBendPoints.asScala.map { elkBendPoint =>
      (elkBendPoint.getX, elkBendPoint.getY)
    }
    val allPoints = Seq(start) ++ bends ++ Seq(end)

    allPoints.sliding(2).map { case point1 :: point2 :: Nil =>
      (point1, point2)
    }.toSeq
  }
}