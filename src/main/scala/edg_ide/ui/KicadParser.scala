package edg_ide.ui
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

// Kicad IR
sealed trait KicadComponent
case class Rectangle(x:Float, y:Float, width:Float, height:Float) extends KicadComponent
case class Line(x0:Float, y0:Float, x1:Float, y1:Float) extends KicadComponent


class KicadParser {

  // TODO have user select with FootprintBrowserPanel
  private var kicadFile:String = "/Users/nikhiljain/college/edg-research/edg-ide/src/main/scala/edg_ide/ui/tmp.mod"

  def getKicadFile: String = {
    this.kicadFile
  }

  def setKicadFile(kicadFile: String): Unit = {
    this.kicadFile = kicadFile
  }

  def parseKicadFile(): ArrayBuffer[KicadComponent] = {
    val fileReader = Source.fromFile(this.getKicadFile)
    val lines = fileReader.getLines()
    var s = ""
    for (l <- lines.toList) {
      s += l
    }

    val parsed = ExpressionParser.parse(s)
    var kicadComponents = new ArrayBuffer[KicadComponent]

    for (p <- parsed.values) {
      // Iterate over the parsed values, seeing if they are a rect / line, and if so, parse into appropriate data structure

      // TODO this code sucks
      p match {
        case SList(values) =>
          if (values.length > 3) {
            values.head match {
              case atom: Atom =>
                if (atom.symbol.equalsIgnoreCase("fp_line")) {
                  // match on "FSilk" for lines
                  val line = values.filter {
                    case a:Atom => false
                    case s: SList =>
                      if (s.values.length == 2) {
                        val first = s.values.head
                        val second = s.values.tail.head
                        if (first.isInstanceOf[Atom] && second.isInstanceOf[Atom]) {
                          second.asInstanceOf[Atom].symbol.contains("Silk")
//                          true
                        }
                        else {
                          false
                        }
                      }
                      else {
                        false
                      }
                  }
                  if (line.length == 1) {
                    // We have a valid line, need to get x0, y0, x1, y1
                    // Assume file is well formed -- i.e. contains "start", "end"
                    val startPos = values.filter {
                      case s:SList =>
                        if (s.values.length == 3) {
                          val first = s.values.head
                          first match {
                            case atom1: Atom =>
                              atom1.symbol.equalsIgnoreCase("start")
                            case _ =>
                              false
                          }
                        }
                        else {
                          false
                        }
                      case _ => false
                    }.head

                    val endPos = values.filter {
                      case s:SList =>
                        if (s.values.length == 3) {
                          val first = s.values.head
                          first match {
                            case atom1: Atom =>
                              atom1.symbol.equalsIgnoreCase("end")
                            case _ =>
                              false
                          }
                        }
                        else {
                          false
                        }
                      case _ => false
                    }.head

                    // TODO is there a better way?
                    val x0 = startPos.asInstanceOf[SList].values.tail.head.asInstanceOf[Atom].symbol.toFloat
                    val y0 = startPos.asInstanceOf[SList].values.tail.tail.head.asInstanceOf[Atom].symbol.toFloat

                    val x1 = endPos.asInstanceOf[SList].values.tail.head.asInstanceOf[Atom].symbol.toFloat
                    val y1 = endPos.asInstanceOf[SList].values.tail.tail.head.asInstanceOf[Atom].symbol.toFloat

                    kicadComponents.addOne(new Line(x0, y0, x1, y1))
                  }
                }
                else if (atom.symbol.equalsIgnoreCase("pad")) {
                  // match on "rect" for rectangles
                  val rect = values.filter {
                    case a:Atom => a.symbol.contains("rect")
                    case _ => false
                  }
                  // TODO parse all pads as rect?
                  if (rect.length == 1) {
                    // We have a valid rect, parse params
                    val startPos = values.filter {
                      case s:SList =>
                        if (s.values.length == 3) {
                          val first = s.values.head
                          first match {
                            case atom1: Atom =>
                              atom1.symbol.equalsIgnoreCase("at")
                            case _ =>
                              false
                          }
                        }
                        else {
                          false
                        }
                      case _ => false
                    }.head

                    val size = values.filter {
                      case s:SList =>
                        if (s.values.length == 3) {
                          val first = s.values.head
                          first match {
                            case atom1: Atom =>
                              atom1.symbol.equalsIgnoreCase("size")
                            case _ =>
                              false
                          }
                        }
                        else {
                          false
                        }
                      case _ => false
                    }.head

                    // TODO is there a better way?
                    val x0 = startPos.asInstanceOf[SList].values.tail.head.asInstanceOf[Atom].symbol.toFloat
                    val y0 = startPos.asInstanceOf[SList].values.tail.tail.head.asInstanceOf[Atom].symbol.toFloat

                    val width = size.asInstanceOf[SList].values.tail.head.asInstanceOf[Atom].symbol.toFloat
                    val height = size.asInstanceOf[SList].values.tail.tail.head.asInstanceOf[Atom].symbol.toFloat

                    kicadComponents.addOne(new Rectangle(x0, y0, width, height))

                  }

                }

              case _ =>

            }
          }

        case _ =>

      }

    }

    fileReader.close()
    kicadComponents
  }




}


