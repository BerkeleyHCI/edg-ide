package edg_ide.ui
import scala.collection.mutable.ListBuffer
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
    for (p <- parsed.values) {
      println(p)
//      case p: Atom => println(p)
//      case p: SList => println("")
    }
    println("done dump")


    fileReader.close()
  }


  // Kicad IR
  sealed trait KicadComponent
  case class Rectangle(x:Int, y:Int, width:Int, height:Int) extends KicadComponent
  case class Line(x0:Int, y0:Int, x1:Int, y1:Int) extends KicadComponent

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


