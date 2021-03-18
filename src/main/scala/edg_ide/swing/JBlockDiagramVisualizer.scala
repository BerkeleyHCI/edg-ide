package edg_ide.swing

import edg_ide.edgir_graph.ElkEdgirGraphUtils
import org.eclipse.elk.graph.{ElkEdge, ElkNode}

import java.awt.Graphics2D
import scala.jdk.CollectionConverters.CollectionHasAsScala


/** Block diagram visualizer that customizes the rendering in JElkGraph with options specific to
  * design block diagrams:
  * - tunnel link names by heuristic matching (degenerate self-to-self links)
  * - additional setError(elts) to render elts as filled in red
  * - additional setStable(elts) to render elts as stale (???)
  */
class JBlockDiagramVisualizer(rootNode: ElkNode, showTop: Boolean = false) extends
    JElkGraph(rootNode, showTop) {

  override def paintEdge(g: Graphics2D, edge: ElkEdge): Unit = {
    super.paintEdge(g, edge)
    if (edge.getSources == edge.getTargets) {  // degenerate, "tunnel" (by heuristic / transform) edge
      println(edge.getSources.asScala.toSeq)
      println(edge.getContainingNode)
      println(edge.getProperty(ElkEdgirGraphUtils.DesignPathMapper.property))
    }
  }
}
