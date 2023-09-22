package edg_ide.runner

import com.lowagie.text.pdf.{PdfPCell, PdfPTable, PdfWriter}
import com.lowagie.text.{Anchor, Document, Element, Font, HeaderFooter, Paragraph, Phrase, Rectangle}
import edg.EdgirUtils.SimpleLibraryPath
import edg.wir.DesignPath
import edg.wir.ProtoUtil.BlockProtoToSeqMap
import edg_ide.edgir_graph.HierarchyGraphElk.PropertyMapper
import edg_ide.edgir_graph.{EdgeWrapper, HierarchyGraphElk, NodeDataWrapper, PortWrapper}
import edg_ide.swing.blocks.{ElkNodePainter, EdgElkNodePainter}
import edgir.elem.elem.{BlockLike, HierarchyBlock}
import edgir.ref.ref.LibraryPath
import org.eclipse.elk.graph.ElkNode

import java.awt.Color
import java.io.FileOutputStream
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object PDFGeneratorUtil {
  /*
    Note for future development:
      If infinite loop (and eventually StackOverflow) occurs when adding header/footer, check that the
      page size is large enough/page margin is small enough so that header/footer content do not exceed
      the available space in the document.
   */
  private final val kPageMargin = 16f
  private final val kTableRowHeight = 23f
  final val kLinkFont = new Font(Font.HELVETICA, 12f, Font.UNDERLINE, Color.BLUE)
  final val kBoldFont = new Font(Font.HELVETICA, 12f, Font.BOLD, Color.BLACK)

  private def generatePageSize(node: ElkNode, tableRows: Int): (Float, Float) = {
    val width = node.getWidth.toFloat + 2f * ElkNodePainter.margin.toFloat + 2f * kPageMargin
    val height = node.getHeight.toFloat + 2f * ElkNodePainter.margin.toFloat + 2f * kPageMargin
    val adjustedHeight = if (tableRows == 1) {
      height
    } else {
      kTableRowHeight * tableRows + height + ElkNodePainter.margin
    }
    (width, adjustedHeight)
  }

  /*
    Generates a map of component class name to a set of DesignPath that uses the component.
    This is used to generate a table of components that are used multiple times in the design.
   */
  def getDuplicationList(block: HierarchyBlock): Map[LibraryPath, List[DesignPath]] = {
    val dupList = mutable.Map[LibraryPath, ListBuffer[DesignPath]]()

    def traverseBlock(block: HierarchyBlock, path: DesignPath): Unit = {
      val className = block.getSelfClass
      dupList.getOrElseUpdate(className, ListBuffer()) += path

      block.blocks.asPairs
        .map { case (name, subblock) =>
          (name, subblock.`type`)
        }
        .collect { case (name, BlockLike.Type.Hierarchy(subblock)) =>
          (path + name, subblock)
        }
        .foreach { case (path, subblock) =>
          traverseBlock(subblock, path)
        }
    }

    traverseBlock(block, DesignPath())
    dupList.view.mapValues(_.toList).toMap
  }

  def generate(
      content: HierarchyBlock,
      mappers: Seq[PropertyMapper[NodeDataWrapper, PortWrapper, EdgeWrapper]] = Seq(),
      fileName: String
  ): Unit = {

    val dupList = getDuplicationList(content)

    val document = new Document()
    val writer = PdfWriter.getInstance(document, new FileOutputStream(fileName))

    def printNode(node: ElkNode, className: LibraryPath, path: DesignPath): Unit = {
      val dupSet = dupList.getOrElse(className, Set.empty)
      val (width, height) = generatePageSize(node, tableRows = dupSet.size)
      document.setPageSize(new Rectangle(width, height))
      document.setMargins(2 * kPageMargin, 2 * kPageMargin, kPageMargin, kPageMargin)

      // Sets the header and footer of each page. Target of the component target link is set as the header.
      val targetAnchor = new Anchor(path.toString, kBoldFont)
      targetAnchor.setName(path.toString)
      val headerPhrase = new Phrase
      headerPhrase.add(targetAnchor)
      val pageHeader = new HeaderFooter(headerPhrase, false)
      pageHeader.setAlignment(Element.ALIGN_LEFT)
      document.setHeader(pageHeader)

      val pageFooter = new HeaderFooter(new Phrase("Page "), true)
      pageFooter.setAlignment(Element.ALIGN_RIGHT)
      pageFooter.setBorder(Rectangle.NO_BORDER)
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

      val cb = writer.getDirectContent
      val graphics = cb.createGraphics(width, height)
      graphics.setBackground(Color.white)
      val painter = new EdgElkNodePainter(node)
      painter.paintComponent(graphics)
      graphics.dispose()

      // Prints out table if there are multiple usages of the same component
      if (dupSet.size > 1) {
        val table = new PdfPTable(1)

        val message = new Paragraph(s"${className.toSimpleString} is also used at the following:")
        val title = new PdfPCell(message)
        title.setBorder(Rectangle.NO_BORDER)
        table.addCell(title)

        dupSet.foreach { dupPath =>
          val cellContent = new Paragraph
          val lastElement = dupPath.lastString

          def generateHyperlinkPathName(hyperlinkPath: DesignPath): Unit = {
            val (parent, currentElement) = hyperlinkPath.split

            if (parent != DesignPath()) {
              generateHyperlinkPathName(parent)
            }
            val anchor = new Anchor
            anchor.setReference(s"#${parent.toString}")
            anchor.add(new Phrase(parent.lastString, kLinkFont))
            anchor.add(new Phrase("."))

            if (currentElement == lastElement) {
              anchor.add(new Phrase(currentElement))
            }

            cellContent.add(anchor)
          }

          generateHyperlinkPathName(dupPath)

          val cell = new PdfPCell(cellContent)
          cell.setFixedHeight(kTableRowHeight)
          table.addCell(cell)
        }

        val tableY = kTableRowHeight * dupSet.size + 2 * ElkNodePainter.margin
        table.setTotalWidth(width - 2 * ElkNodePainter.margin)
        table.writeSelectedRows(0, -1, ElkNodePainter.margin.toFloat, tableY, writer.getDirectContent())
      }
    }

    def printNextHierarchyLevel(block: HierarchyBlock, path: DesignPath = DesignPath()): Unit = {
      val node = HierarchyGraphElk.HBlockToElkNode(block, path, mappers = mappers)
      printNode(node, block.getSelfClass, path)

      block.blocks.asPairs
        .map { case (name, subblock) =>
          (name, subblock.`type`)
        }
        .collect {
          case (name, BlockLike.Type.Hierarchy(subblock)) if subblock.blocks.nonEmpty =>
            (path + name, subblock, subblock.getSelfClass)
        }
        .foreach {
          case (path, subblock, className) => {
            if (dupList.getOrElse(className, Set.empty).headOption.contains(path)) {
              printNextHierarchyLevel(subblock, path)
            }
          }
        }
    }

    printNextHierarchyLevel(content)
    document.close()
  }
}
