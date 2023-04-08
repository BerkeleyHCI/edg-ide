package edg_ide.runner

import com.lowagie.text.{Document, Element, HeaderFooter, Rectangle}
import com.lowagie.text.pdf.PdfWriter
import edg.EdgirUtils.SimpleLibraryPath
import edg.wir.ProtoUtil.BlockProtoToSeqMap
import edgir.elem.elem.{BlockLike, HierarchyBlock}
import org.eclipse.elk.graph.ElkNode
import edg.wir.DesignPath
import edg_ide.edgir_graph.HierarchyGraphElk.PropertyMapper
import edg_ide.edgir_graph.{EdgeWrapper, HierarchyGraphElk, NodeDataWrapper, PortWrapper}
import edg_ide.swing.blocks.ElkNodePainter
import edgir.ref.ref
import edgir.ref.ref.LibraryPath

import java.awt.Color
import java.io.{FileNotFoundException, FileOutputStream, IOException}
import scala.collection.mutable

object PDFGeneratorUtil{

  private def generatePageSize(node: ElkNode): (Float, Float) = {
    /*
    Note for future development:
      If infinite loop occurs when printing texts, check that the page size is large enough such
      the printed text will be within the margins. This is due to a bug in the API
    */
    val width = node.getWidth.toFloat + 2.5f * ElkNodePainter.margin.toFloat
    val height = node.getHeight.toFloat + 2.5f * ElkNodePainter.margin.toFloat
    (width, height)
  }

  def getDuplicationList(block: HierarchyBlock, path: DesignPath = DesignPath(),
                         dupList: mutable.Map[LibraryPath, Set[DesignPath]]): Unit ={

    val className = block.getSelfClass
//    println(className.toSimpleString)
    dupList.getOrElseUpdate(className, Set(path)) match {
      case existingSet: Set[DesignPath] => dupList(className) = existingSet + path
    }

    block.blocks.asPairs.map {
      case (name, subblock) => (name, subblock.`type`)
    }.collect {
      case (name, BlockLike.Type.Hierarchy(subblock)) =>  (path + name, subblock)
    }.foreach {
      case (path, subblock) => getDuplicationList(subblock, path, dupList)
    }
  }

  def generate(content: HierarchyBlock,
               mappers: Seq[PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper]] = Seq(),
               fileName: String): Unit = {
    val dupList = mutable.Map[LibraryPath, Set[DesignPath]]()
    getDuplicationList(content, dupList = dupList)
//    println("Duplication List:")
//    dupList.foreach { case (key, value) =>
//      println(s"$key: $value")
//    }

    try {
      val document = new Document()
      val writer = PdfWriter.getInstance(document, new FileOutputStream(fileName))
      val footer = new HeaderFooter(true)
      footer.setBorder(Rectangle.NO_BORDER)
      footer.setAlignment(Element.ALIGN_RIGHT)
      document.setFooter(footer)

      def printNode(node: ElkNode): Unit = {
        val (width, height) = generatePageSize(node)
        document.setPageSize(new Rectangle(width, height))

        /*
        Metadata for the Footer does not align the page number correctly if
        document.open() is called before document.setPageSize() was executed
         */
        if (!document.isOpen) {
          document.open()
        } else {
          document.newPage
        }

        val cb = writer.getDirectContent
        val graphics = cb.createGraphics(width, height)
        val painter = new ElkNodePainter(node)
        painter.paintComponent(graphics, Color.white)
        graphics.dispose()
      }

      def printNextHierarchyLevel(block: HierarchyBlock, path: DesignPath = DesignPath()): Unit = {
        val node = HierarchyGraphElk.HBlockToElkNode(block, path, mappers = mappers)
        printNode(node)

        block.blocks.asPairs.map {
          case (name, subblock) => (name, subblock.`type`)
        }.collect {
          case (name, BlockLike.Type.Hierarchy(subblock)) if subblock.blocks.nonEmpty => (path + name, subblock, subblock.getSelfClass)
        }.foreach {
          case (path, subblock, className) => {
//            println(s"Printing ${className}")
//            println(s"Printing ${dupList.getOrElse(className, Set.empty).head}")
            if (path.toString == dupList.getOrElse(className, Set.empty).head.toString) {
              printNextHierarchyLevel(subblock, path)
            }
          }
        }
      }

      printNextHierarchyLevel(content)
      document.close()
    } catch {
      case e: FileNotFoundException => println(s"Couldn't find ${fileName}.")
      case e: SecurityException => println(s"Access to ${fileName} denied.")
      case e: IOException => println(s"Had an IOException trying to read ${fileName}")
    }
  }
}
