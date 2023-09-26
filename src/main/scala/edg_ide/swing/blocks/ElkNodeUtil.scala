package edg_ide.swing.blocks

import org.eclipse.elk.graph.ElkEdgeSection

import scala.jdk.CollectionConverters.ListHasAsScala

object ElkNodeUtil {
  // Utility functions
  def allPoints(section: ElkEdgeSection): Seq[(Double, Double)] = {
    val bendPoints = section.getBendPoints.asScala.map { bend => (bend.getX, bend.getY) }
    val start = (section.getStartX, section.getStartY)
    val end = (section.getEndX, section.getEndY)
    Seq(start) ++ bendPoints ++ Seq(end)
  }
}
