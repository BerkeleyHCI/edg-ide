package edg_ide.util

import edg.compiler.DesignMap
import edg.elem.elem
import edg.ref.ref
import edg.wir.DesignPath

import scala.collection.SeqMap


/** For a design, returns all elaborated blocks of a certain type
  */
class DesignFindBlockOfTypes(targetTypes: Set[ref.LibraryPath])
    extends DesignMap[Unit, Seq[(DesignPath, elem.HierarchyBlock)], Unit] {
  override def mapPort(path: DesignPath, port: elem.Port): Unit = {
  }
  override def mapPortArray(path: DesignPath, port: elem.PortArray,
                            ports: SeqMap[String, Unit]): Unit = {
  }
  override def mapBundle(path: DesignPath, port: elem.Bundle,
                         ports: SeqMap[String, Unit]): Unit = {
  }
  override def mapPortLibrary(path: DesignPath, port: ref.LibraryPath): Unit = {
  }

  override def mapBlock(path: DesignPath, block: elem.HierarchyBlock,
                        ports: SeqMap[String, Unit], blocks: SeqMap[String, Seq[(DesignPath, elem.HierarchyBlock)]],
                        links: SeqMap[String, Unit]): Seq[(DesignPath, elem.HierarchyBlock)] = {
    if (block.superclasses.toSet.subsetOf(targetTypes)) {
      blocks.values.flatten.toSeq :+ ((path, block))
    } else {
      blocks.values.flatten.toSeq
    }
  }
  override def mapBlockLibrary(path: DesignPath, block: ref.LibraryPath): Seq[(DesignPath, elem.HierarchyBlock)] = {
    Seq()
  }

  override def mapLink(path: DesignPath, block: elem.Link,
                       ports: SeqMap[String, Unit], links: SeqMap[String, Unit]): Unit = {
  }
  override def mapLinkLibrary(path: DesignPath, link: ref.LibraryPath): Unit = {
  }
}
