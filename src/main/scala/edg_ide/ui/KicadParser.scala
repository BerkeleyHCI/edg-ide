package edg_ide.ui
import java.io.{File, FileNotFoundException}
import java.nio.charset.MalformedInputException

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

// Kicad IR
sealed trait KicadComponent
// TODO: add layers to this data structure?
// TODO distinguish pad names from other geometry
case class Rectangle(x:Float, y:Float, width:Float, height:Float, name: String) extends KicadComponent
case class Line(x0:Float, y0:Float, x1:Float, y1:Float) extends KicadComponent


class KicadParser(kicadFilePath:String) {

  private var kicadFile: File = new File("")

  def setKicadFile(kicadFile: File): Unit = {
    this.kicadFile = kicadFile
  }


  def containsAtom(slist:SList, atom: String): Boolean = {
    val filtered = slist.values.filter {
      case Atom(symbol) if symbol == atom => true
      case _ => false
    }

    filtered.nonEmpty
  }

  // Given an SList, scan the SList for the only sublist tagged by `name`
  // i.e. in the list ((a b c) ((d) e f) (d 1 2) d h i), if we search for
  // sublists tagged by 'd', we want to return (d 1 2)
  def getOnlySublistByName(list: SList, name: String): SList = {
    val subLists = list.values
      .filter {
        case _:Atom => false
        case SList(values) =>
          values.head match {
            case atom: Atom =>
              atom.symbol.equals(name)
            case _: SList =>false
          }
      }
      .map {
        // all Atoms should have been filtered out at this point
        e => e.asInstanceOf[SList]
      }

    if (subLists.length == 1)
      return subLists(0)

    // enforcing the "Only" in getOnlySublistByName
    if (subLists.length > 1)
      throw new IllegalArgumentException("Multiple subLists with name, only expected one: " + name)

    throw new IllegalArgumentException("No subList with name, expected one: " + name)

  }

  // Given a position-identifying list of the form (name:String, a:Float, b:Float),
  // return (a, b) or throw an Exception
  def extractPosition(list:SList): (Float, Float) = {

    list.values match {
      case (_:Atom) :: (b:Atom) :: (c:Atom) :: Nil =>
        (b, c) match {
          case (Atom(xPos), Atom(yPos)) =>
            (xPos.toFloatOption, yPos.toFloatOption) match {
              case (Some(xPos), Some(yPos)) => (xPos, yPos)
              case _ => throw new IllegalArgumentException("Expected (float, float), but got non-numerical value: " + xPos + yPos)
            }
          case badVal => throw new IllegalArgumentException("Expected (float, float), but got: " + badVal)
        }

      case badVal => throw new IllegalArgumentException("Expected (float, float), but got: " + badVal)
    }

  }

  def parseKicadFile(): List[KicadComponent] = {
    try {
      val fileReader = Source.fromFile(kicadFile)
      val lines = fileReader.getLines()
      var s = ""
      for (l <- lines.toList) {
        s += l
      }

      val parsed = ExpressionParser.parse(s)

      val kicadComponents = parsed.values.flatMap {
        case list: SList if list.values.nonEmpty && list.values.head.isInstanceOf[Atom] =>
            list.values.head.asInstanceOf[Atom].symbol match {

              // Match on the different components we want to handle
              case "pad" if containsAtom(list, "rect") || containsAtom(list, "roundrect") =>
                val (x, y) = extractPosition(getOnlySublistByName(list, "at"))
                val (w, h) = extractPosition(getOnlySublistByName(list, "size"))

                val name = list.values(1).asInstanceOf[Atom].symbol
                Some(Rectangle(x, y, w, h, name))

              case "fp_line" =>
                val layerList = getOnlySublistByName(list, "layer")
                if (layerList.values.length == 2 && layerList.values.tail.head == Atom("F.SilkS")) {
                  val (startX, startY) = extractPosition(getOnlySublistByName(list, "start"))
                  val (endX, endY) = extractPosition(getOnlySublistByName(list, "end"))
                  Some(Line(startX, startY, endX, endY))
                }
                else
                  None

              case _ => None
          }

        case _ => None
      }

      fileReader.close()
      kicadComponents
    }
    catch {
      // Fail noisily but don't crash the plugin -- just don't draw anything
      case e:FileNotFoundException => {}
        println("Couldn't open kicad file for parsing: ", kicadFilePath)
        e.printStackTrace()
        List()
      case t:Throwable =>
        println("Error while parsing kicad file")
        t.printStackTrace()
        List()
    }
  }
}


