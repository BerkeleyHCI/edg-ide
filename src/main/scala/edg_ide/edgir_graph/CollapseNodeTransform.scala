package edg_ide.edgir_graph

import edg_ide.edgir_graph.EdgirGraph.EdgirEdge

trait CollapseNodeTransform {

  /** In a containing node, collapses the target node, by removing edges pointing to that node and replacing them with
    * edges from sources directly to sinks.
    *
    * Takes in a function that determines how to merge the edges being collapsed.
    *
    * Algorithm overview:
    *   - Collect all edges involving the target block
    *   - Get a list of all sources and sinks on the other end
    *   - 'Delete' the original edges
    *   - ... and add in new edges as all-to-all edges from sources to sinks TODO: can this be made order preserving in
    *     some way?
    *   - 'Delete' the original node
    *
    * TODO: can this be made generic on HGraphNode? Issue seems to be with instantiating an abstract type
    */
  def collapse(
      node: EdgirGraph.EdgirNode,
      collapseTarget: Seq[String],
      edgeFn: Seq[EdgeWrapper] => EdgeWrapper
  ): EdgirGraph.EdgirNode = {
    val collapsedBlockSources = node.edges.collect {
      case EdgirEdge(data, Some(source), Some(target)) if target.startsWith(collapseTarget) => // block is source
        (source, data)
    }
    val collapsedBlockTargets = node.edges.collect {
      case EdgirEdge(data, Some(source), Some(target)) if source.startsWith(collapseTarget) => // block is target
        (target, data)
    }

    val collapsedEdgesData = (collapsedBlockSources ++ collapsedBlockTargets)
      .map { case (_, data) => data }

    // If there are no sources or sinks, arbitrarily designate the first as the source
    val (fixedSources, fixedTargets) = if (collapsedBlockSources.isEmpty && collapsedBlockTargets.isEmpty) {
      (Seq(), Seq())
    } else if (collapsedBlockSources.isEmpty) {
      (Seq(collapsedBlockTargets.head), collapsedBlockTargets.tail)
    } else if (collapsedBlockTargets.isEmpty) {
      (Seq(collapsedBlockSources.head), collapsedBlockSources.tail)
    } else {
      (collapsedBlockSources, collapsedBlockTargets)
    }

    val crossSourceTarget =
      fixedSources.flatMap(source =>
        fixedTargets.collect {
          case target if source != target =>
            (source, target)
        }
      )
    val newEdges = crossSourceTarget.map { case ((sourcePath, _), (targetPath, _)) =>
      EdgirGraph.EdgirEdge(edgeFn(collapsedEdgesData), source = Some(sourcePath), target = Some(targetPath))
    }

    val filteredEdges = node.edges.filter { // remove edges pointing to collapsed node
      case EdgirEdge(_, Some(source), _) if source.startsWith(collapseTarget) => false
      case EdgirEdge(_, _, Some(target)) if target.startsWith(collapseTarget) => false
      case _ => true
    }
    val combinedEdges = filteredEdges ++ newEdges

    val filteredMembers = node.members.filter { case (name, _) => name != collapseTarget }
    EdgirGraph.EdgirNode(node.data, filteredMembers, combinedEdges)
  }
}
