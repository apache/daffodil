package edu.illinois.ncsa.daffodil.processors

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import scala.util.parsing.combinator.RegexParsers
import edu.illinois.ncsa.daffodil.util.Logging
import scala.util.parsing.input.Reader
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util._
import scala.collection.mutable.Queue
import java.util.regex.Pattern
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.DelimiterType._
import edu.illinois.ncsa.daffodil.processors.DelimiterLocation._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import scala.Array.canBuildFrom
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import scala.language.reflectiveCalls
import scala.language.reflectiveCalls
import edu.illinois.ncsa.daffodil.dsom.AnnotatedSchemaComponent
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.RuntimeEncodingMixin

object TextJustificationType extends Enum {
  sealed abstract trait Type extends EnumValueType
  case object None extends Type
  case object Left extends Type
  case object Right extends Type
  case object Center extends Type
}

sealed abstract class DelimParseResult(nextArg: Reader[Char]) {
  def isSuccess: Boolean
  def next = nextReader.asInstanceOf[DFDLCharReader]
  def nextReader = nextArg
}

case class DelimParseSuccess(val delimiter: String,
  val delimiterType: DelimiterType.Type,
  val delimiterLoc: DelimiterLocation.Type,
  val numBits: Int,
  fieldArg: String,
  nextArg: Reader[Char],
  val numCharsRead: Int)
  extends DelimParseResult(nextArg) {
  def isSuccess = true
  def field = fieldArg
  def get = field
}

case class DelimParseFailure(msgArg: String, nextArg: Reader[Char])
  extends DelimParseResult(nextArg) {
  def isSuccess = false
  def msg = msgArg
}

class DFDLDelimParserCommon(
  override val context: RuntimeData,
  override val encodingInfo: EncodingInfo)
  extends RegexParsers with DebugRegexParsers with RuntimeEncodingMixin with Serializable {
  /**
   * Thisobject has to be nested because it has as an argument type Success[String]
   * and that type is only availble to things that implement the scala...Parsers trait.
   *
   * This is why you don't want to ball up all your stuff into a trait, you make reuse
   * by derivation work, but you make reuse by encapsulation very difficult.
   */
  object DelimParseSuccessFactory {
    /**
     * If content is supplied then it is used to determine the field length.
     * If None then the extracted field value itself is used.
     */
    def apply(res: Success[String], delimiter: String, delimiterType: DelimiterType.Type, contentOpt: Maybe[String],
      dLoc: DelimiterLocation.Type) = {

      val Success(fieldResult, next) = res
      val content = contentOpt.getOrElse(res.get)
      val charLength = content.length
      val fieldResultBits = knownEncodingStringBitLength(content)
      val result = new DelimParseSuccess(delimiter, delimiterType,
        dLoc, fieldResultBits, fieldResult, next, charLength)
      result
    }
  }

  override val skipWhitespace = false

  /**
   * Need a parser that will always fail.
   * Essentially a parser to reflect the ability to pass in an empty Separator
   * or empty Terminator Set.
   */
  def parserAlwaysFail[T](expected: String)(name: String) = new Parser[T] {
    def apply(in: Input): ParseResult[T] = {
      Failure(name + " expected to fail.", in)
    }
  }

  /**
   * Constructs an Array of Parser[String] which holds the Parser representations
   * of the delimList.
   *
   * Constructs an Array of String which holds the Regex representations of the
   * delimList.
   */
  def buildDelims(delimList: Set[String]): (Array[Parser[String]], Array[String]) = {
    var delimsParser: Queue[Parser[String]] = Queue.empty
    var delimsRegex: Queue[String] = Queue.empty

    // We probably always want delims ordered:
    // Multi-char delims containing WSP+/*, WSP+, WSP*, multi-char delims, WSP, single-char delims

    sortDelims(delimList).toList.foreach(str => {
      val d = new Delimiter()
      d.compile(str)
      delimsParser.enqueue(d.delimRegExParseDelim.r) // The regex representing the actual delimiter
      delimsRegex.enqueue(d.delimRegExParseDelim) // The regex representing the actual delimiter
    })
    (delimsParser.toArray, delimsRegex.toArray)
  }

  def sortDelims(delimList: Set[String]): Seq[String] = {
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
  def combineDelimitersRegex(sepsRegex: Array[String], termsRegex: Array[String]): String = {
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

  def getDfdlLiteralRegex(dfdlLiteralList: Set[String]): String = {
    val (_, regex) = this.buildDelims(dfdlLiteralList)
    combineDelimitersRegex(regex, Array.empty[String])
  }

  // TODO: does this handle %ES; or do we have to have outside separate checks for that?
  // There is a separate check right now in LiteralNilDelimitedOrEndOfData. 
  def isFieldDfdlLiteral(field: String, dfdlLiteralList: Set[String]): Boolean = {
    val dfdlLiteralRegex = getDfdlLiteralRegex(dfdlLiteralList)
    val m = Pattern.compile(dfdlLiteralRegex).matcher(field)
    m.find()
    m.matches()
  }

  lazy val EOF: Parser[String] = """\z""".r

  def generateInputPatternedParser(pattern: String, sc: ThrowsSDE): Parser[String] = {
    try {
      val thePattern: Parser[String] = "generateInputPatternedParser.thePattern".!!!(("(?s)" + pattern).r)
      val entry = "generateInputPatternedParser.entry".!!!(thePattern ~! opt(EOF)) ^^ {
        case (p ~ _) => p
      }
      entry
    } catch { case ex: java.util.regex.PatternSyntaxException => { sc.SDE("%s", ex.getMessage) } }

  }

}
