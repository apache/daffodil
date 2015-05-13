package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.ScalaPatternParser
import scala.collection.mutable.Queue
import java.util.regex.Pattern
import edu.illinois.ncsa.daffodil.processors.Delimiter

trait NilMatcherMixin {

  protected def nilValues: List[String]

  /**
   * Constructs an Array of Parser[String] which holds the Parser representations
   * of the delimList.
   *
   * Constructs an Array of String which holds the Regex representations of the
   * delimList.
   */
  private def buildDelims(delimList: Set[String]): (Array[ScalaPatternParser], Array[String]) = {
    var delimsParser: Queue[ScalaPatternParser] = Queue.empty
    var delimsRegex: Queue[String] = Queue.empty

    // We probably always want delims ordered:
    // Multi-char delims containing WSP+/*, WSP+, WSP*, multi-char delims, WSP, single-char delims

    sortDelims(delimList).toList.foreach(str => {
      val d = new Delimiter()
      d.compile(str)
      delimsParser.enqueue(ScalaPatternParser(d.delimRegExParseDelim.r)) // The regex representing the actual delimiter
      delimsRegex.enqueue(d.delimRegExParseDelim) // The regex representing the actual delimiter
    })
    (delimsParser.toArray, delimsRegex.toArray)
  }

  private def sortDelims(delimList: Set[String]): Seq[String] = {
    val wspStarByItself = delimList.filter(s => s == "%WSP*;")
    val wspPlusByItself = delimList.filter(s => s == "%WSP+;")

    val filteredDelimList = (delimList -- (wspStarByItself union wspPlusByItself))

    val multiCharUnboundedLength = filteredDelimList.filter(s => (s.contains("%WSP*;") || s.contains("%WSP+;")))
    val multiChar = (filteredDelimList -- multiCharUnboundedLength).filter(s => s.length() > 1)
    val singleChar = filteredDelimList -- (multiChar union multiCharUnboundedLength)

    val sortedUnbounded = multiCharUnboundedLength.toArray[String]
    val sortedMultiChar = multiChar.toArray[String]

    scala.util.Sorting.quickSort(sortedUnbounded)
    scala.util.Sorting.quickSort(sortedMultiChar)

    val orderedResultSeq: Seq[String] = sortedUnbounded.reverse.toSeq ++ wspPlusByItself ++ wspStarByItself ++ sortedMultiChar.reverse.toSeq ++ singleChar
    orderedResultSeq
  }

  /**
   * Combines the delimiters into a single alternation
   */
  private def combineDelimitersRegex(sepsRegex: Array[String], termsRegex: Array[String]): String = {
    val sb = new StringBuilder()
    sepsRegex.foreach(x => {
      sb.append(x)
      sb.append("|")
    })
    termsRegex.foreach(x => {
      sb.append(x)
      sb.append("|")
    })
    val delimRegex = sb.toString().replaceFirst("[\\|]$", "")

    delimRegex
  }

  private def getDfdlLiteralRegex(dfdlLiteralList: Set[String]): String = {
    val (_, regex) = this.buildDelims(dfdlLiteralList)
    combineDelimitersRegex(regex, Array.empty[String])
  }

  private lazy val matcher = {
    val dfdlLiteralRegex = getDfdlLiteralRegex(nilValues.toSet)
    val m = Pattern.compile(dfdlLiteralRegex).matcher("")
    m
  }

  // TODO: does this handle %ES; or do we have to have outside separate checks for that?
  // There is a separate check right now in LiteralNilDelimitedOrEndOfData. 
  final def isFieldNilLiteral(field: String): Boolean = {
    matcher.reset(field)
    matcher.find()
    matcher.matches()
  }
}
