/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

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
    val delimsRegex: Queue[String] = Queue.empty

    // We probably always want delims ordered:
    // Multi-char delims containing WSP+/*, WSP+, WSP*, multi-char delims, WSP, single-char delims

    sortDelims(delimList).toList.foreach(str => {
      val d = new Delimiter()
      d.compileDelimiter(str)
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

  private lazy val patternLiteralValue: Pattern = {
    val dfdlLiteralRegex = getDfdlLiteralRegex(cookedNilValuesForParse.toSet)
    val p = Pattern.compile(dfdlLiteralRegex)
    p
  }

  private lazy val patternLiteralCharacter: Pattern = {
    // LiteralCharacter is legal for only fixed length elements.
    // It's a single character or byte that, when repeated to
    // length of the element, is the nil representation.
    //
    val dfdlLiteralRegex = getDfdlLiteralRegex(cookedNilValuesForParse.toSet)
    val sb = new StringBuilder(dfdlLiteralRegex)
    sb.append("+")
    val p = Pattern.compile(sb.toString())
    p
  }

  private def newMatcher(): Matcher = {
    patternLiteralValue.matcher("")
  }

  private def newLiteralCharacterMatcher(): Matcher = {
    patternLiteralCharacter.matcher("")
  }

  object withFieldNilMatcher extends OnStack[Matcher](newMatcher(), (m: Matcher) => m.reset(""))
  object withFieldNilLiteralCharacterMatcher extends OnStack[Matcher](newLiteralCharacterMatcher(), (m: Matcher) => m.reset(""))

  // TODO: does this handle %ES; or do we have to have outside separate checks for that?
  // There is a separate check right now in LiteralNilDelimitedOrEndOfData.
  final def isFieldNilLiteralValue(field: String): Boolean = {
    withFieldNilMatcher { matcher =>
      matcher.reset(field)
      matcher.find()
      matcher.matches()
    }
  }

  final def isFieldNilLiteralCharacter(field: String): Boolean = {
    withFieldNilLiteralCharacterMatcher { matcher =>
      matcher.reset(field)
      matcher.find()
      matcher.matches()
    }
  }
}
