package edg_ide.ui

import scala.collection.mutable.ListBuffer


/*
Source: https://github.com/ZenBowman/sexpr

The MIT License (MIT)

Copyright (c) 2013 ZenBowman

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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


