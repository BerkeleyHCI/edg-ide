package edg_ide.edgir_graph

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
      case edge if edge.target.startsWith(collapseTarget) => // block is source
        (edge.source, edge.data)
    }
    val collapsedBlockTargets = node.edges.collect {
      case edge if edge.source.startsWith(collapseTarget) => // block is target
        (edge.target, edge.data)
    }

    val collapsedEdgesData = (collapsedBlockSources ++ collapsedBlockTargets)
      .map { case (_, data) => data }

    // If there are no sources or sinks, arbitrarily designate the first as the source
    val (fixedSources, fixedTargets) = if (collapsedBlockSources.isEmpty && collapsedBlockSources.isEmpty) {
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
      EdgirGraph.EdgirEdge(edgeFn(collapsedEdgesData), source = sourcePath, target = targetPath)
    }

    val filteredEdges = node.edges.filter { edge => // remove edges pointing to collapsed node
      !edge.source.startsWith(collapseTarget) && !edge.target.startsWith(collapseTarget)
    }
    val combinedEdges = filteredEdges ++ newEdges

    val filteredMembers = node.members.filter { case (name, _) => name != collapseTarget }
    EdgirGraph.EdgirNode(node.data, filteredMembers, combinedEdges)
  }
}
