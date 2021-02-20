package edg_ide.ui
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

class KicadParser {

  private var kicadFile:String = "/Users/nikhiljain/Downloads/Connector_Audio.pretty/Jack_3.5mm_CUI_SJ-3523-SMT_Horizontal.kicad_mod"

  def getKicadFile(): String = {
    this.kicadFile
  }

  def setKicadFile(kicadFile: String): Unit = {
    this.kicadFile = kicadFile
  }

  def parseKicadFile(): Unit = {
    val fileReader = Source.fromFile(this.getKicadFile())
    val lines = fileReader.getLines()
    println("printing file contents")
    var s = ""
    for (l <- lines.toList) {
      println(l)
      s += l
    }
    println("DONE file contents")


    println("parse")

    val parsed = ExpressionParser.parse(s)
    println("done parse")

    println("printing dump")
    var i = 0

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
                          second.asInstanceOf[Atom].symbol.equalsIgnoreCase("F.SilkS")
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

                    // TODO jesus christ
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
                    case a:Atom => a.symbol.equalsIgnoreCase("rect")
                    case _ => false
                  }
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

                    // TODO jesus christ
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

      for (k <- kicadComponents) {
        println(k)
      }

//      case p: Atom => println(p)
//      case p: SList => println("")
    }
    println("done dump")


    fileReader.close()
  }


  // Kicad IR
  sealed trait KicadComponent
  case class Rectangle(x:Float, y:Float, width:Float, height:Float) extends KicadComponent
  case class Line(x0:Float, y0:Float, x1:Float, y1:Float) extends KicadComponent

  /*
  Source: https://github.com/ZenBowman/sexpr
  TODO how to cite properly?
  */

  sealed trait Element
  case class Atom(symbol: String) extends Element
  case class SList(values: List[Element]) extends Element

  class InvalidSExpressionException extends Exception

  object ExpressionParser {
    private var remainingTokens: List[String] = List()

    def tokenize(expression: String): List[String] = {
      expression.replace("(", " ( ").replace(")", " ) ").trim().split("\\s+").toList
    }

    def parse(expression: String): SList = {
      remainingTokens = tokenize(expression)
      parseTokens()
    }

    def parseTokens(): SList = {
      val elements = new ListBuffer[Element]

      while (remainingTokens.nonEmpty) {
        val first = remainingTokens.head
        remainingTokens = remainingTokens.tail
        if (first == "(") {
          val element = parseTokens()
          elements.append(element)
        }
        else if (first == ")") {
          return SList(elements.toList)
        } else {
          elements.append(new Atom(first))
        }
      }
      println("elements", elements.length)
      for (p <- elements) {
        p match {
          case Atom(symbol) =>
            println("symbol", symbol.toString)
            for (c <- 1 until symbol.length) {
              print(symbol.charAt(c))
            }
            printf("%s", symbol)
          case SList(values) => println("list", values.length)
        }

      }

      try {
        elements.head.asInstanceOf[SList]
      } catch {
        case e: Exception =>
          println("erra", e)
          println(elements.head.asInstanceOf[Atom].symbol)
          throw new InvalidSExpressionException
      }
    }
  }




}


