package edg_ide.edgir_graph

import scala.collection.SeqMap


/**
  * Removes all nodes below a certain depth
  *
  * TODO: can this be made generic on HGraphNode?
  */
object PruneDepthTransform extends CollapseNodeTransform {
  def apply(node: EdgirGraph.EdgirNode, depth: Int): EdgirGraph.EdgirNode = {
    if (depth == 0) {
      val filteredMembers = node.members.collect {
        case (name, member: EdgirGraph.EdgirPort) => name -> member
          // discard anything else, namely nodes
      }
      EdgirGraph.EdgirNode(node.data, filteredMembers, Seq())  // no internal components, discard edges
    } else {
      val mappedMembers = node.members.to(SeqMap).view.mapValues {
        case member: EdgirGraph.EdgirPort => member
        case member: EdgirGraph.EdgirNode => apply(member, depth - 1)
      }.to(SeqMap)
      EdgirGraph.EdgirNode(node.data, mappedMembers, node.edges)
    }

  }
}
