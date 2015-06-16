package edu.illinois.ncsa.daffodil.processors.parsers

import scala.collection.mutable.Queue
import java.util.regex.Pattern
import edu.illinois.ncsa.daffodil.processors.Delimiter
import edu.illinois.ncsa.daffodil.util.OnStack
import java.util.regex.Matcher

trait NilMatcherMixin {

  protected def cookedNilValuesForParse: List[String]

  final lazy val isEmptyAllowed: Boolean =
    cookedNilValuesForParse.contains("%ES;")

  /**
   * Constructs an Array of Parser[String] which holds the Parser representations
   * of the delimList.
   *
   * Constructs an Array of String which holds the Regex representations of the
   * delimList.
   */
  private def buildDelims(delimList: Set[String]): Array[String] = {
    var delimsRegex: Queue[String] = Queue.empty

    // We probably always want delims ordered:
    // Multi-char delims containing WSP+/*, WSP+, WSP*, multi-char delims, WSP, single-char delims

    sortDelims(delimList).toList.foreach(str => {
      val d = new Delimiter()
      d.compile(str)
      delimsRegex.enqueue(d.delimRegExParseDelim) // The regex representing the actual delimiter
    })
    delimsRegex.toArray
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
    val regex = this.buildDelims(dfdlLiteralList)
    combineDelimitersRegex(regex, Array.empty[String])
  }

  private lazy val pattern: Pattern = {
    val dfdlLiteralRegex = getDfdlLiteralRegex(cookedNilValuesForParse.toSet)
    val p = Pattern.compile(dfdlLiteralRegex)
    p
  }

  private def newMatcher(): Matcher = {
    pattern.matcher("")
  }

  object withFieldNilMatcher extends OnStack[Matcher](newMatcher(), (m: Matcher) => m.reset(""))

  // TODO: does this handle %ES; or do we have to have outside separate checks for that?
  // There is a separate check right now in LiteralNilDelimitedOrEndOfData. 
  final def isFieldNilLiteral(field: String): Boolean = {
    withFieldNilMatcher { matcher =>
      matcher.reset(field)
      matcher.find()
      matcher.matches()
    }
  }
}

