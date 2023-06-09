package edg_ide.util
import edg.wir.DesignPath
import edg.wir.ProtoUtil._
import edgir.elem.elem

object EdgirAnalysisUtils {

  /** If this block is a chain of single inner blocks (excluding bridges and adapters), returns the deepest inner block.
    * If this is a block with no children, returns itself.
    *
    * This is useful for getting the innermost block, past any wrapping adapters.
    */
  def getInnermostSubblock(
      path: DesignPath,
      block: elem.HierarchyBlock
  ): Option[(DesignPath, elem.HierarchyBlock)] = {
    val nonDummyBlocks = block.blocks.asPairs.collect {
      case (name, subblock)
        if !(name.startsWith("(bridge)") || name.startsWith("(adapter)") ||
          name.startsWith("(not_connected)")) =>
        (name, subblock.`type`)
    }
    nonDummyBlocks match {
      case Nil => Some((path, block))
      case (name, elem.BlockLike.Type.Hierarchy(subblock)) :: Nil =>
        getInnermostSubblock(path + name, subblock)
      case _ => None // multiple inner blocks
    }
  }
}
