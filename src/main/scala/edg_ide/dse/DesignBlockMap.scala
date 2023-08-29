package edg_ide.dse

import edg.compiler.DesignMap
import edg.wir.DesignPath
import edgir.elem.elem
import edgir.ref.ref

import scala.collection.SeqMap

/** A simplified version of DesignMap that needs an implementation for blocks
  */
trait DesignBlockMap[BlockType] extends DesignMap[Unit, BlockType, Unit] {
  override def mapPort(path: DesignPath, port: elem.Port): Unit = {}
  override def mapPortArray(path: DesignPath, port: elem.PortArray, ports: SeqMap[String, Unit]): Unit = {}
  override def mapBundle(path: DesignPath, port: elem.Bundle, ports: SeqMap[String, Unit]): Unit = {}
  override def mapPortLibrary(path: DesignPath, port: ref.LibraryPath): Unit = {}

  override def mapBlock(
      path: DesignPath,
      block: elem.HierarchyBlock,
      ports: SeqMap[String, Unit],
      blocks: SeqMap[String, BlockType],
      links: SeqMap[String, Unit]
  ): BlockType = {
    mapBlock(path, block, blocks)
  }

  // this is the simpler version of mapBlock that drops the port and link args, implement me
  def mapBlock(path: DesignPath, block: elem.HierarchyBlock, blocks: SeqMap[String, BlockType]): BlockType

  override def mapLink(
      path: DesignPath,
      link: elem.Link,
      ports: SeqMap[String, Unit],
      links: SeqMap[String, Unit]
  ): Unit = {}
  override def mapLinkArray(
      path: DesignPath,
      link: elem.LinkArray,
      ports: SeqMap[String, Unit],
      links: SeqMap[String, Unit]
  ): Unit = {}
  override def mapLinkLibrary(path: DesignPath, link: ref.LibraryPath): Unit = {}
}
