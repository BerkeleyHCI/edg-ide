package edg_ide.util

import edg.compiler.DesignBlockMap
import edg.elem.elem
import edg.ref.ref
import edg.wir.DesignPath

import scala.collection.SeqMap


/** For a design, returns all elaborated blocks of a certain type
  */
class DesignFindBlockOfTypes(targetTypes: Set[ref.LibraryPath])
    extends DesignBlockMap[Seq[(DesignPath, elem.HierarchyBlock)]] {
  override def mapBlock(path: DesignPath, block: elem.HierarchyBlock,
                        blocks: SeqMap[String, Seq[(DesignPath, elem.HierarchyBlock)]]):
      Seq[(DesignPath, elem.HierarchyBlock)] = {
    if (targetTypes.contains(block.getSelfClass)) {
      blocks.values.flatten.toSeq :+ ((path, block))
    } else {
      blocks.values.flatten.toSeq
    }
  }
  override def mapBlockLibrary(path: DesignPath, block: ref.LibraryPath): Seq[(DesignPath, elem.HierarchyBlock)] = {
    Seq()
  }
}
