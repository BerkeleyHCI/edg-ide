package edg_ide.runner

import com.lowagie.text.{Anchor, Document, Element, HeaderFooter, Paragraph, Phrase, Rectangle}
import com.lowagie.text.pdf.{ColumnText, PdfPCell, PdfPTable, PdfWriter}
import edg.EdgirUtils.SimpleLibraryPath
import edg.wir.ProtoUtil.BlockProtoToSeqMap
import edgir.elem.elem.{BlockLike, HierarchyBlock}
import org.eclipse.elk.graph.ElkNode
import edg.wir.DesignPath
import edg_ide.edgir_graph.HierarchyGraphElk.PropertyMapper
import edg_ide.edgir_graph.{EdgeWrapper, HierarchyGraphElk, NodeDataWrapper, PortWrapper}
import edg_ide.swing.blocks.ElkNodePainter
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

    val TABLE_ROW_HEIGHT = 20f
    val dupList = mutable.Map[LibraryPath, Set[DesignPath]]()
    getDuplicationList(content, dupList = dupList)
    println("Duplication List:")
    dupList.foreach { case (key, value) =>
      println(s"$key: $value")
    }

    try {
      val document = new Document()
      val writer = PdfWriter.getInstance(document, new FileOutputStream(fileName))
//      val footer = new HeaderFooter(true)
//      footer.setBorder(Rectangle.NO_BORDER)
//      footer.setAlignment(Element.ALIGN_RIGHT)
//      document.setFooter(footer)

      def printNode(node: ElkNode, className: LibraryPath, path: DesignPath): Unit = {
        val dupSet = dupList.getOrElse(className, Set.empty)
        val (width, height) = generatePageSize(node)
        val adjustedHeight = if(dupSet.size == 1) {
          height
        } else {
          TABLE_ROW_HEIGHT * dupSet.size + height + ElkNodePainter.margin
        }
        document.setPageSize(new Rectangle(width, adjustedHeight))

        val targetAnchor = new Anchor(path.toString)
        targetAnchor.setName(path.toString)
        val footerPhrase = new Phrase("Design Path: ")
        footerPhrase.add(targetAnchor)
        val pageFooter = new HeaderFooter(footerPhrase,true)
        pageFooter.setBorder(Rectangle.NO_BORDER)
        pageFooter.setAlignment(Element.ALIGN_RIGHT)
        document.setFooter(pageFooter)
        /*
        Metadata for the Footer does not align the page number correctly if
        document.open() is called before document.setPageSize() was executed
         */
        if (!document.isOpen) {
          document.open()
        } else {
          document.newPage
        }

        // Prints out name of the component
//        val componentName = new Paragraph(path.toString)
//        document.add(componentName)

        val cb = writer.getDirectContent
        val graphics = cb.createGraphics(width, adjustedHeight)
        val painter = new ElkNodePainter(node)
        painter.paintComponent(graphics, Color.white)
        graphics.dispose()

        // Prints out table if there are multiple usages of the same component
        if(dupSet.size > 1) {
          val table = new PdfPTable(1)

          val message = new Paragraph(s"${className.toSimpleString} is also used at the following:")
          val title = new PdfPCell(message)
          title.setBorder(Rectangle.NO_BORDER)
//          title.setColspan(2)
          table.addCell(title)

          dupSet.foreach { path =>
            val cellContent = new Paragraph
            val parentAnchor = new Anchor(path.toString)
            val parentPath = path.toString
            if (parentPath.contains('.')) {
              parentAnchor.setReference(s"#${parentPath.substring(0, parentPath.lastIndexOf('.'))}")
            } else {
              // need to see if this else case is needed???
              // TODO: consider the case when the parentPath is empty (i.e. the root block)
              parentAnchor.setReference(s"#${DesignPath().toString}")
            }
            cellContent.add(parentAnchor)
            val cell = new PdfPCell(cellContent)
            table.addCell(cell)
//            val cell2 = new PdfPCell(new Paragraph(s"Link to parent ${path.toString}"))
//            table.addCell(cell2)
          }
          val tableY = adjustedHeight - height + ElkNodePainter.margin
          table.setTotalWidth(width - 2 * ElkNodePainter.margin)
          table.writeSelectedRows(0, -1, ElkNodePainter.margin.toFloat, tableY, writer.getDirectContent())
        }
      }

      def printNextHierarchyLevel(block: HierarchyBlock, path: DesignPath = DesignPath()): Unit = {
        val node = HierarchyGraphElk.HBlockToElkNode(block, path, mappers = mappers)
        printNode(node, block.getSelfClass, path)

        block.blocks.asPairs.map {
          case (name, subblock) => (name, subblock.`type`)
        }.collect {
          case (name, BlockLike.Type.Hierarchy(subblock)) if subblock.blocks.nonEmpty => (path + name, subblock, subblock.getSelfClass)
        }.foreach {
          case (path, subblock, className) => {
//            println(s"Printing ${className}")
            println(s"Printing ${dupList.getOrElse(className, Set.empty).head}")
            if (path == dupList.getOrElse(className, Set.empty).head) {
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
