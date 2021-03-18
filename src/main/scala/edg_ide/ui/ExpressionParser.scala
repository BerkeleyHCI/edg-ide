package edg_ide.ui

import scala.collection.mutable.ListBuffer


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

    try {
      elements.head.asInstanceOf[SList]
    } catch {
      case e: Exception =>
        println(e.getMessage)
        println(elements.head.asInstanceOf[Atom].symbol)
        throw new InvalidSExpressionException
    }
  }
}


