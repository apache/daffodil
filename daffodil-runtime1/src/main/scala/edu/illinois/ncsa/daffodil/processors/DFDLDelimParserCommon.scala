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

trait DebugRegexParsers extends RegexParsers with Logging {
  /**
   * Used to trace parsing
   */
  implicit def toLogged(name: String) = new {
    def !!![T](p: Parser[T]) = log(p)(name)
  }

  /**
   * A helper method that turns a `Parser` into one that will
   *  print debugging information to stdout before and after
   *  being applied.
   */
  override def log[T](p: => Parser[T])(name: String): Parser[T] = Parser { in =>
    log(LogLevel.DelimDebug, "trying %s at %s", name, in)
    val r = p(in)
    log(LogLevel.DelimDebug, "end %s --> %s", name, r)
    r
  }

}

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

class DFDLDelimParserCommon(stringBitLengthFunction: String => Int) extends RegexParsers with DebugRegexParsers {
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
    def apply(res: Success[String], delimiter: String, delimiterType: DelimiterType.Type, contentOpt: Option[String],
      dLoc: DelimiterLocation.Type) = {

      val Success(fieldResult, next) = res
      val content = contentOpt.getOrElse(res.get)
      val charLength = content.length
      val fieldResultBits = stringBitLengthFunction(content)
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
      d(str)
      delimsParser.enqueue(d.delimRegExParseDelim.r) // The regex representing the actual delimiter
      delimsRegex.enqueue(d.delimRegExParseDelim) // The regex representing the actual delimiter
    })
    (delimsParser.toArray, delimsRegex.toArray)
  }

  /**
   * Receives a Set of delimiters.  Returns a Set of (DelimRegexStr, OrigDelimStr)
   */
  def getDelimsRegex(delimList: Set[String]): Set[(String, String)] = {
    var delims: Queue[(String, String)] = Queue.empty

    delimList.foreach(str => {
      val d = new Delimiter()
      d(str)
      delims.enqueue((d.delimRegExParseDelim, str))
    })
    delims.toSet
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
    val delimRegex = sb.toString().replaceFirst("[\\|]$", "") // trimEnd("|")
    //    val seps = sepsRegex.mkString("|")
    //    val terms = termsRegex.mkString("|")
    //    val delimRegex = (seps + terms) //.replaceFirst("[\\|]$", "")
    delimRegex
  }

  /**
   * This function performs the "longest match" alternation by recursively combining
   * all of the Parsers present in pArray.
   *
   * Assumes pArray is at least of length 1.
   *
   * pArray: An array of delimiters where each delimiter is represented by a Parser
   * idx: The index of the current delimiter/Parser, defaults to 0
   */
  def combineLongest[T](pArray: Array[Parser[T]], idx: Int = 0): Parser[T] = {
    val len = pArray.length

    if (len == 0) { return parserAlwaysFail("")("empty delimiter list") }

    val p0 = pArray(idx)

    if ((idx + 1) == len) { p0 }
    else {
      (p0) ||| (combineLongest(pArray, idx + 1))
    }
  }

  def convertDFDLLiteralToRegex(dfdlLiteral: String): String = {
    var sb: StringBuilder = new StringBuilder("(")

    dfdlLiteral foreach {
      char =>
        {
          char match {
            case '[' => sb.append("\\[")
            case '\\' => sb.append("\\\\")
            case '^' => sb.append("\\^")
            case '$' => sb.append("\\$")
            case '.' => sb.append("\\.")
            case '|' => sb.append("\\|")
            case '?' => sb.append("\\?")
            case '*' => sb.append("\\*")
            case '+' => sb.append("\\+")
            case '(' => sb.append("\\(")
            case ')' => sb.append("\\)")
            case '{' => sb.append("\\{")
            case '}' => sb.append("\\}")
            case x => sb.append(x)
          }
        }
    }
    sb.append(")")
    val res = sb.toString()
    if (res.equals("()")) "" // get rid of empty group in regular expressions
    else res
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

  def generateRemovePaddingParser(justification: TextJustificationType.Type, padChar: String): Option[Parser[String]] = {
    if (padChar.length() == 0) return None
    //val rAnything = """(.*?)(?=%s*$)""".format(padChar)
    val rAnything = """(.*?)(?=""" + padChar + """*$)"""
    val anything: Parser[String] = "generateRemovePaddingParser.anything".!!!(rAnything.r)
    val padCharRegex = this.convertDFDLLiteralToRegex(padChar)
    //val rPadCharLeft = """^(%s*)""".format(padCharRegex)
    val rPadCharLeft = """^(""" + padCharRegex + """*)"""
    val pPadCharLeft: Parser[String] = "generateRemovePaddingParser.pPadCharLeft".!!!(rPadCharLeft.r)
    //val rPadCharRight = """(%s*)$""".format(padCharRegex)
    val rPadCharRight = """(""" + padCharRegex + """*)$"""
    val pPadCharRight: Parser[String] = "generateRemovePaddingParser.pPadCharRight".!!!(rPadCharRight.r)
    val fieldCenter: Parser[String] = "generateRemovePaddingParser.fieldCenter".!!!(pPadCharLeft ~! anything ~! pPadCharRight) ^^ { case (l ~ a ~ r) => a }
    val fieldLeft: Parser[String] = "generateRemovePaddingParser.fieldLeft".!!!(anything ~! pPadCharRight) ^^ { case (a ~ r) => a }
    val fieldRight: Parser[String] = "generateRemovePaddingParser.fieldRight".!!!(pPadCharLeft ~! anything) ^^ { case (l ~ a) => a }
    val result: Option[Parser[String]] = justification match {
      case TextJustificationType.None => None
      case TextJustificationType.Left => Some(fieldLeft)
      case TextJustificationType.Right => Some(fieldRight)
      case TextJustificationType.Center => Some(fieldCenter)
    }
    result
  }

  def generateInputPatternedParser(pattern: String): Parser[String] = {
    val thePattern: Parser[String] = "generateInputPatternedParser.thePattern".!!!(("(?s)" + pattern).r)
    //val entry = thePattern <~ opt(EOF)
    //    val e0 = thePattern
    //    val e1 = thePattern ~! EOF ^^ { case(p ~ e) => p}
    //    val entry = e0 ||| e1
    val entry = "generateInputPatternedParser.entry".!!!(thePattern ~! opt(EOF)) ^^ {
      case (p ~ _) => p
    }
    entry
  }

  def generateInputNCharactersParser(nChars: Long): Parser[String] = {
    val anything: Parser[String] = "generateInputNCharactersParser.anything".!!!(""".*""".r)
    val rNChars = """(?s).{""" + nChars.toString() + """}"""
    //val firstNChars: Parser[String] = String.format("""(?s).{%s}""", nChars.toString()).r
    val firstNChars: Parser[String] = "generateInputNCharactersParser.firstNChars".!!!(rNChars.r)
    val entry = firstNChars //<~ anything // Technically shouldn't need to add anything, we only want the first nChars
    entry
  }

  def generateCombinedDelimsParser(localDelims: Set[String], remoteDelims: Set[String]): Parser[String] = {
    val (localDelimsParser, localDelimsRegex) = this.buildDelims(localDelims)
    val (remoteDelimsParser, remoteDelimsRegex) = this.buildDelims(remoteDelims)
    val combinedDelims = remoteDelimsParser ++ localDelimsParser
    val combinedDelimsParser = this.combineLongest(combinedDelims)
    combinedDelimsParser
  }

  def generateCombinedDelimsParser(localDelimsParser: Array[Parser[String]],
    remoteDelimsParser: Array[Parser[String]]): Parser[String] = {
    val combinedDelims = remoteDelimsParser ++ localDelimsParser
    val combinedDelimsParser = this.combineLongest(combinedDelims)
    combinedDelimsParser
  }

  def generateLocalDelimsRegex(localDelimsRegex: Array[String]): String = {
    val newLocalDelimsRegex = "(?s)^(" + combineDelimitersRegex(localDelimsRegex, Array.empty[String]) + ")$"
    newLocalDelimsRegex
  }

  def generateIsLocalDelimParser(localDelimsRegex: Array[String]): Parser[String] = {
    val isLocalDelimsParser: Parser[String] = "generateIsLocalDelimParser.isLocalDelimsParser".!!!(generateLocalDelimsRegex(localDelimsRegex).r)
    isLocalDelimsParser
  }

  def generateDelimiter(delims: Set[String]): (Array[Parser[String]], Array[String]) = {
    val (delimsParser, delimsRegex) = this.buildDelims(delims)
    (delimsParser, delimsRegex)
  }

  def generateInputDelimiterParsers(localDelims: Set[String], remoteDelims: Set[String]): (Parser[String], Parser[String], Set[(String, String)]) = {
    val (localDelimsParser, localDelimsRegex) = this.buildDelims(localDelims)
    val (remoteDelimsParser, remoteDelimsRegex) = this.buildDelims(remoteDelims)
    val combinedDelims = localDelimsParser ++ remoteDelimsParser
    val combinedDelimsParser = this.combineLongest(combinedDelims)
    val isLocalDelimParser = generateIsLocalDelimParser(localDelimsRegex)
    //val inputDelimiterParser = combinedDelimsParser <~ opt(EOF) // Should yield longest match of all the delimiters
    //    val p0 = combinedDelimsParser ~! EOF ^^ { case(p ~ e) => p}
    //    val p1 = combinedDelimsParser
    //    val inputDelimiterParser = p0 ||| p1
    val inputDelimiterParser = combinedDelimsParser ~! opt(EOF) ^^ {
      case (d ~ _) => d
    }
    val isRemoteRegex = combineDelimitersRegex(remoteDelimsRegex, Array.empty)
    (inputDelimiterParser, isLocalDelimParser, getDelimsRegex(remoteDelims))
  }

  def generateInputDelimiterParser(localDelims: Set[String], remoteDelims: Set[String]): Parser[String] = {
    val combinedDelimsParser = generateCombinedDelimsParser(localDelims, remoteDelims)
    //val entry = combinedDelimsParser <~ opt(EOF) // Should yield longest match of all the delimiters
    //    val p0 = combinedDelimsParser ~! EOF ^^ { case (p ~ e) => p}
    //    val p1 = combinedDelimsParser
    //    val entry = p0 ||| p1
    //    entry
    val inputDelimiterParser = combinedDelimsParser ~! opt(EOF) ^^ {
      case (d ~ _) => d
    }
    inputDelimiterParser
  }

  def generateInputDelimiterParser(localDelims: Array[Parser[String]], remoteDelims: Array[Parser[String]]): Parser[String] = {
    val combinedDelimsParser = generateCombinedDelimsParser(localDelims, remoteDelims)
    //val entry = combinedDelimsParser <~ opt(EOF) // Should yield longest match of all the delimiters
    //    val p0 = combinedDelimsParser ~! EOF ^^ { case (p ~ e) => p}
    //    val p1 = combinedDelimsParser
    //    val entry = p0 ||| p1
    val inputDelimiterParser = combinedDelimsParser ~! opt(EOF) ^^ {
      case (d ~ _) => d
    }
    inputDelimiterParser
  }

  def generateCharacterRegex(character: String): String = {
    val characterRegex = convertDFDLLiteralToRegex(character)
    characterRegex
  }
  protected def generateSeparators(separators: Set[String]): (Parser[String], Array[String]) = {
    val (sepsParser, sepsRegex) = this.buildDelims(separators)
    val pSeps: Parser[String] = this.combineLongest(sepsParser)
    (pSeps, sepsRegex)
  }
  protected def generateTerminators(terminators: Set[String]): (Parser[String], Array[String]) = {
    val (termsParser, termsRegex) = this.buildDelims(terminators)
    val pTerms: Parser[String] = this.combineLongest(termsParser)
    (pTerms, termsRegex)
  }

  protected def generateInputWithPadParser(separators: Parser[String], terminators: Parser[String],
    delimsRegex: String,
    padCharRegex: String,
    justification: TextJustificationType.Type,
    hasDelim: Boolean,
    isMissingDelimAllowed: Boolean = true): Parser[(Vector[String], String)] = {

    val pSeps: Parser[String] = "generateInputWithPadParser.pSeps".!!!(separators)
    val pTerms: Parser[String] = "generateInputWithPadParser.pTerms".!!!(terminators)

    //val rPadChar = """(%s*)""".format(padCharRegex)
    val rPadChar = """(""" + padCharRegex + """*)"""
    val pPadChar: Parser[String] = "generateInputWithPadParser.pPadChar".!!!(rPadChar.r)
    //val rBefore = """(?s)(.*?)(?=(%1$s+(%2$s))|(%1$s+\z)|(%2$s)|(\z))""".format(padCharRegex, delimsRegex)
    val rBefore = """(?s)(.*?)(?=(""" + padCharRegex + """+(""" + delimsRegex + """))|(""" + padCharRegex + """+\z)|(""" + delimsRegex + """)|(\z))"""
    val pBefore: Parser[String] = "generateInputWithPadParser.pBefore".!!!(rBefore.r)
    //val rBeforeNoDelims = """(?s)(.*?)(?=(%1$s+\z)|(\z))""".format(padCharRegex)
    val rBeforeNoDelims = """(?s)(.*?)(?=(""" + padCharRegex + """+\z)|(\z))"""
    val pBeforeNoDelims: Parser[String] = "generateInputWithPadParser.pBeforeNoDelims".!!!(rBeforeNoDelims.r)
    //val rBeforeNoPadding = """(?s)((.*?)(?=(%1$s)|(\z)))|(.*)""".format(delimsRegex)
    val rBeforeNoPadding = """(?s)((.*?)(?=(""" + delimsRegex + """)|(\z)))|(.*)"""
    val pBeforeNoPadding: Parser[String] = "generateInputWithPadParser.pBeforeNoPadding".!!!(rBeforeNoPadding.r)
    val rBeforeNoPaddingOrDelims = """(?s)(.*?)(?=(\z))"""
    val pBeforeNoPaddingOrDelims: Parser[String] = "generateInputWithPadParser.pBeforeNoPaddingOrDelims".!!!(rBeforeNoPaddingOrDelims.r)
    val pDelims: Parser[String] = "generateInputWithPadParser.pDelims".!!!(pSeps ||| pTerms)

    val paddedContent = "generateInputWithPadParser.paddedContent".!!!(pPadChar ~! pBefore ~! pPadChar) ^^ { case (lp ~ c ~ rp) => Vector(lp, c, rp) }
    val leftPaddedContent = "generateInputWithPadParser.leftPaddedContent".!!!(pPadChar ~! pBeforeNoPadding) ^^ { case (lp ~ c) => Vector(lp, c) }
    val rightPaddedContent = "generateInputWithPadParser.rightPaddedContent".!!!(pBefore ~! pPadChar) ^^ { case (c ~ rp) => Vector(c, rp) }

    val pFieldAndDelim: Parser[(Vector[String], String)] = (justification, isMissingDelimAllowed, hasDelim) match {
      case (TextJustificationType.Left, false, true) => {
        val contentLeftDelimReq = "generateInputWithPadParser.contentLeft_ReqDelims".!!!(rightPaddedContent ~! pDelims) ^^ { case (c ~ d) => (c, d) }
        contentLeftDelimReq
      }
      case (TextJustificationType.Right, false, true) => {
        val contentRightDelimReq = "generateInputWithPadParser.contentRight_ReqDelims".!!!(leftPaddedContent ~! pDelims) ^^ { case (c ~ d) => (c, d) }
        contentRightDelimReq
      }
      case (TextJustificationType.Center, false, true) => {
        val contentCenterDelimReq = "generateInputWithPadParser.contentCenter_ReqDelims".!!!(paddedContent ~! pDelims) ^^ { case (c ~ d) => (c, d) }
        contentCenterDelimReq
      }
      case (TextJustificationType.Left, true, true) => {
        val contentLeft = "generateInputWithPadParser.contentLeft_OptDelims".!!!(rightPaddedContent ~! (pDelims | EOF)) ^^ { case (c ~ d) => (c, d) }
        contentLeft
      }
      case (TextJustificationType.Left, true, false) => {
        val rightPaddedContent = "generateInputWithPadParser.rightPaddedContent_NoDelims".!!!(pBeforeNoDelims ~! pPadChar) ^^ { case (c ~ rp) => Vector(c, rp) }
        val contentLeft = "generateInputWithPadParser.contentLeft_NoDelims".!!!(rightPaddedContent ~! (EOF)) ^^ { case (c ~ d) => (c, d) }
        contentLeft
      }
      case (TextJustificationType.Right, true, true) => {
        val contentRight = "generateInputWithPadParser.contentRight_OptDelims".!!!(leftPaddedContent ~! (pDelims | EOF)) ^^ { case (c ~ d) => (c, d) }
        contentRight
      }
      case (TextJustificationType.Right, true, false) => {
        val leftPaddedContent = "generateInputWithPadParser.leftPaddedContent_NoDelims".!!!(pPadChar ~! pBeforeNoDelims) ^^ { case (lp ~ c) => Vector(lp, c) }
        val contentRight = "generateInputWithPadParser.contentRight_NoDelims".!!!(leftPaddedContent ~! (EOF)) ^^ { case (c ~ d) => (c, d) }
        contentRight
      }
      case (TextJustificationType.Center, true, true) => {
        val contentCenter = "generateInputWithPadParser.contentCenter_OptDelims".!!!(paddedContent ~! (pDelims | EOF)) ^^ { case (c ~ d) => (c, d) }
        contentCenter
      }
      case (TextJustificationType.Center, true, false) => {
        val paddedContent = "generateInputWithPadParser.paddedContent_NoDelims".!!!(pPadChar ~! pBeforeNoDelims ~ pPadChar) ^^ { case (lp ~ c ~ rp) => Vector(lp, c, rp) }
        val contentCenter = "generateInputWithPadParser.contentCenter_NoDelims".!!!(paddedContent ~! (EOF)) ^^ { case (c ~ d) => (c, d) }
        contentCenter
      }
      case _ => Assert.invariantFailed("not one of the combinations.") // return failedResult
    }
    pFieldAndDelim
  }

  protected def generateInputNoPadParser(separators: Parser[String], terminators: Parser[String],
    delimsRegex: String, hasDelim: Boolean,
    isMissingDelimAllowed: Boolean = true): Parser[(Vector[String], String)] = {

    val pSeps: Parser[String] = "generateInputNoPadParser.pSeps".!!!(separators)
    val pTerms: Parser[String] = "generateInputNoPadParser.pTerms".!!!(terminators)

    //val rBeforeNoPadding = """(?s)((.*?)(?=(%1$s)|(\z)))|(.*)"""
    //val pBeforeNoPadding: Parser[String] = rBeforeNoPadding.format(delimsRegex).r
    val rBeforeNoPadding = """(?s)((.*?)(?=(""" + delimsRegex + """)|(\z)))|(.*)"""
    val pBeforeNoPadding: Parser[String] = "generateInputNoPadParser.pBeforeNoPadding".!!!(rBeforeNoPadding.r)

    val rBeforeNoPaddingOrDelims = """(?s)(.*?)(?=(\z))"""
    val pBeforeNoPaddingOrDelims: Parser[String] = "generateInputNoPadParser.pBeforeNoPaddingOrDelims".!!!(rBeforeNoPaddingOrDelims.r)
    val pDelims: Parser[String] = "generateInputNoPadParser.pDelims".!!!(pSeps ||| pTerms)

    val pFieldAndDelim: Parser[(Vector[String], String)] = (isMissingDelimAllowed, hasDelim) match {
      case (false, true) => {
        val contentDelimReq = "generateInputNoPadParser.content_ReqDelims".!!!(pBeforeNoPadding ~! pDelims) ^^ { case (c ~ d) => (Vector(c), d) }
        contentDelimReq
      }
      case (true, true) => {
        val content = "generateInputNoPadParser.content_OptDelims".!!!(pBeforeNoPadding ~! (pDelims | EOF)) ^^ { case (c ~ d) => (Vector(c), d) }
        content
      }
      case (true, false) => {
        val content = "generateInputNoPadParser.content_NoDelims".!!!(pBeforeNoPaddingOrDelims ~! (EOF)) ^^ { case (c ~ d) => (Vector(c), d) }
        content
      }
      case _ => Assert.invariantFailed("impossible combination") // return failedResult
    }
    pFieldAndDelim
  }

  def generateInputParser(separators: Set[String], terminators: Set[String],
    justification: TextJustificationType.Type,
    padChar: String,
    isMissingDelimAllowed: Boolean = true): Parser[(Vector[String], String)] = {
    val hasDelim: Boolean = separators.size > 0 || terminators.size > 0
    val (pSeps, sepsRegex) = this.generateSeparators(separators)
    val (pTerms, termsRegex) = this.generateTerminators(terminators)
    val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)
    val padCharRegex = generateCharacterRegex(padChar);

    justification match {
      case TextJustificationType.None => return generateInputNoPadParser(pSeps, pTerms,
        delimsRegex, hasDelim, isMissingDelimAllowed)
      case _ => return generateInputWithPadParser(pSeps, pTerms, delimsRegex, padCharRegex,
        justification, hasDelim, isMissingDelimAllowed)
    }
  }

  def generateInputParser2(pSeps: Parser[String], pTerms: Parser[String],
    sepsRegex: Array[String], termsRegex: Array[String], hasDelim: Boolean,
    justification: TextJustificationType.Type, padChar: String,
    isMissingDelimAllowed: Boolean = true): Parser[(Vector[String], String)] = {

    val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)
    val padCharRegex = generateCharacterRegex(padChar);

    justification match {
      case TextJustificationType.None => return generateInputNoPadParser(pSeps, pTerms,
        delimsRegex, hasDelim, isMissingDelimAllowed)
      case _ => return generateInputWithPadParser(pSeps, pTerms, delimsRegex, padCharRegex,
        justification, hasDelim, isMissingDelimAllowed)
    }
  }

  protected def generateEscapeBlockWithPadParser(separators: Parser[String], terminators: Parser[String],
    escapeBlockStartRegex: String, escapeBlockEndRegex: String,
    escapeEscapeCharacterRegex: String,
    padCharRegex: String,
    justification: TextJustificationType.Type,
    hasEscEsc: Boolean,
    isMissingDelimAllowed: Boolean = true): Parser[(Vector[String], String)] = {

    val pEscape: Parser[String] = "generateEscapeBlockWithPadParser.pEscape".!!!(escapeEscapeCharacterRegex.r)
    val pBlockStart: Parser[String] = "generateEscapeBlockWithPadParser.pBlockStart".!!!(escapeBlockStartRegex.r)
    val pBlockEnd: Parser[String] = "generateEscapeBlockWithPadParser.pBlockEnd".!!!(escapeBlockEndRegex.r)

    val pSeps: Parser[String] = "generateEscapeBlockWithPadParser.pSeps".!!!(separators)
    val pTerms: Parser[String] = "generateEscapeBlockWithPadParser.pTerms".!!!(terminators)
    val pDelims: Parser[String] = "generateEscapeBlockWithPadParser.pDelims".!!!(pSeps ||| pTerms)

    //    val rUnescapedBlockStart = if (hasEscEsc) """(?<!%1$s)%2$s""" else """%2$s"""
    //    val pUnescapedBlockStart: Parser[String] = rUnescapedBlockStart.format(escapeEscapeCharacterRegex, escapeBlockStartRegex).r
    val rUnescapedBlockStart = if (hasEscEsc) """(?<!""" + escapeEscapeCharacterRegex + """)""" + escapeBlockStartRegex + """""" else escapeBlockStartRegex
    val pUnescapedBlockStart: Parser[String] = "generateEscapeBlockWithPadParser.pUnescapedBlockStart".!!!(rUnescapedBlockStart.r)
    //    val rUnescapedBlockEnd = if (hasEscEsc) """(?<!%1$s)%2$s""" else """%2$s"""
    //    val pUnescapedBlockEnd: Parser[String] = rUnescapedBlockEnd.format(escapeEscapeCharacterRegex, escapeBlockEndRegex).r
    val rUnescapedBlockEnd = if (hasEscEsc) """(?<!""" + escapeEscapeCharacterRegex + """)""" + escapeBlockEndRegex + """""" else escapeBlockEndRegex
    val pUnescapedBlockEnd: Parser[String] = "generateEscapeBlockWithPadParser.pUnescapedBlockEnd".!!!(rUnescapedBlockEnd.r)
    val rBeforeUnescapedBlockEnd = """(?s)(.*?)(?=(""" + rUnescapedBlockEnd + """))"""
    //    val pBeforeUnescapedBlockEnd: Parser[String] = rBeforeUnescapedBlockEnd.format(escapeEscapeCharacterRegex, escapeBlockEndRegex).r
    val pBeforeUnescapedBlockEnd: Parser[String] = "generateEscapeBlockWithPadParser.pBeforeUnescapedBlockEnd".!!!(rBeforeUnescapedBlockEnd.r)
    val blockedContent: Parser[Vector[String]] = "generateEscapeBlockWithPadParser.blockedContent".!!!(pUnescapedBlockStart ~! pBeforeUnescapedBlockEnd ~! pUnescapedBlockEnd) ^^ { case (bs ~ c ~ be) => Vector(bs, c, be) }

    //    val rPadChar = """(%s*)"""
    //    val pPadChar: Parser[String] = rPadChar.format(padCharRegex).r
    val rPadChar = """(""" + padCharRegex + """*)"""
    val pPadChar: Parser[String] = "generateEscapeBlockWithPadParser.pPadChar".!!!(rPadChar.r)
    val paddedBlockedContent: Parser[Vector[String]] = "generateEscapeBlockWithPadParser.paddedBlockedContent".!!!(pPadChar ~! blockedContent ~! pPadChar) ^^ { case (lp ~ bc ~ rp) => Vector(lp) ++ bc ++ Vector(rp) }
    val leftPaddedBlockedContent: Parser[Vector[String]] = "generateEscapeBlockWithPadParser.leftPaddedBlockedContent".!!!(pPadChar ~! blockedContent) ^^ { case (lp ~ bc) => Vector(lp) ++ bc }
    val rightPaddedBlockedContent: Parser[Vector[String]] = "generateEscapeBlockWithPadParser.rightPaddedBlockedContent".!!!(blockedContent ~! pPadChar) ^^ { case (bc ~ rp) => bc ++ Vector(rp) }

    val pFieldAndDelim: Parser[(Vector[String], String)] = (justification -> isMissingDelimAllowed) match {
      case (TextJustificationType.Left, false) => {
        val contentLeftDelimReq: Parser[(Vector[String], String)] = "generateEscapeBlockWithPadParser.contentLeft_ReqDelims".!!!(rightPaddedBlockedContent ~! pDelims) ^^ { case (bc ~ d) => (bc -> d) }
        contentLeftDelimReq
      }
      case (TextJustificationType.Right, false) => {
        val contentRightDelimReq: Parser[(Vector[String], String)] = "generateEscapeBlockWithPadParser.contentRight_ReqDelims".!!!(leftPaddedBlockedContent ~! pDelims) ^^ { case (bc ~ d) => (bc -> d) }
        contentRightDelimReq
      }
      case (TextJustificationType.Center, false) => {
        val contentCenterDelimReq: Parser[(Vector[String], String)] = "generateEscapeBlockWithPadParser.contentCenter_ReqDelims".!!!(paddedBlockedContent ~! pDelims) ^^ { case (bc ~ d) => (bc -> d) }
        contentCenterDelimReq
      }
      case (TextJustificationType.Left, true) => {
        val contentLeft: Parser[(Vector[String], String)] = "generateEscapeBlockWithPadParser.contentLeft_OptDelims".!!!(rightPaddedBlockedContent ~! (pDelims | EOF)) ^^ { case (bc ~ d) => (bc -> d) }
        contentLeft
      }
      case (TextJustificationType.Right, true) => {
        val contentRight: Parser[(Vector[String], String)] = "generateEscapeBlockWithPadParser.contentRight_OptDelims".!!!(leftPaddedBlockedContent ~! (pDelims | EOF)) ^^ { case (bc ~ d) => (bc -> d) }
        contentRight
      }
      case (TextJustificationType.Center, true) => {
        val contentCenter: Parser[(Vector[String], String)] = "generateEscapeBlockWithPadParser.contentCenter_OptDelims".!!!(paddedBlockedContent ~! (pDelims | EOF)) ^^ { case (bc ~ d) => (bc -> d) }
        contentCenter
      }
      case (TextJustificationType.None, _) => {
        Assert.invariantFailed("shouldn't be None. We got to this match based on knowing it is not None")
      }
    }
    pFieldAndDelim
  }

  protected def generateEscapeBlockNoPadParser(separators: Parser[String], terminators: Parser[String],
    escapeBlockStartRegex: String, escapeBlockEndRegex: String,
    escapeEscapeCharacterRegex: String = "",
    hasEscEsc: Boolean,
    isMissingDelimAllowed: Boolean = true): Parser[(Vector[String], String)] = {

    val pEscape: Parser[String] = "generateEscapeBlockNoPadParser.pEscape".!!!(escapeEscapeCharacterRegex.r)
    val pBlockStart: Parser[String] = "generateEscapeBlockNoPadParser.pBlockStart".!!!(escapeBlockStartRegex.r)
    val pBlockEnd: Parser[String] = "generateEscapeBlockNoPadParser.pBlockEnd".!!!(escapeBlockEndRegex.r)

    val pSeps: Parser[String] = "generateEscapeBlockNoPadParser.pSeps".!!!(separators)
    val pTerms: Parser[String] = "generateEscapeBlockNoPadParser.pTerms".!!!(terminators)
    val pDelims: Parser[String] = "generateEscapeBlockNoPadParser.pDelims".!!!(pSeps ||| pTerms)

    //    val rBeforeUnescapedBlockEnd = """(?s)(.*?)(?=(""" + rUnescapedBlockEnd + """))"""
    //    val pBeforeUnescapedBlockEnd: Parser[String] = rBeforeUnescapedBlockEnd.format(escapeEscapeCharacterRegex, escapeBlockEndRegex).r
    //    val rUnescapedBlockStart = if (hasEscEsc) """(?<!%1$s)%2$s""" else """%2$s"""
    //    val pUnescapedBlockStart: Parser[String] = rUnescapedBlockStart.format(escapeEscapeCharacterRegex, escapeBlockStartRegex).r
    val rUnescapedBlockStart = if (hasEscEsc) """(?<!""" + escapeEscapeCharacterRegex + """)""" + escapeBlockStartRegex else escapeBlockStartRegex
    val pUnescapedBlockStart: Parser[String] = "generateEscapeBlockNoPadParser.pUnescapedBlockStart".!!!(rUnescapedBlockStart.r)

    //    val rUnescapedBlockEnd = if (hasEscEsc) """(?<!%1$s)%2$s""" else """%2$s"""
    //    val pUnescapedBlockEnd: Parser[String] = rUnescapedBlockEnd.format(escapeEscapeCharacterRegex, escapeBlockEndRegex).r
    val rUnescapedBlockEnd = if (hasEscEsc) """(?<!""" + escapeEscapeCharacterRegex + """)""" + escapeBlockEndRegex else escapeBlockEndRegex
    val pUnescapedBlockEnd: Parser[String] = "generateEscapeBlockNoPadParser.pUnescapedBlockEnd".!!!(rUnescapedBlockEnd.r)
    val rBeforeUnescapedBlockEnd = """(?s)(.*?)(?=(""" + rUnescapedBlockEnd + """))"""
    val pBeforeUnescapedBlockEnd: Parser[String] = "generateEscapeBlockNoPadParser.pBeforeUnescapedBlockEnd".!!!(rBeforeUnescapedBlockEnd.r)
    val blockedContent: Parser[Vector[String]] = "generateEscapeBlockNoPadParser.blockedContent".!!!(pUnescapedBlockStart ~! pBeforeUnescapedBlockEnd ~! pUnescapedBlockEnd) ^^ { case (bs ~ c ~ be) => Vector(bs, c, be) }

    val pFieldAndDelim: Parser[(Vector[String], String)] = isMissingDelimAllowed match {
      case false => {
        val contentDelimReq: Parser[(Vector[String], String)] = "generateEscapeBlockNoPadParser.content_ReqDelims".!!!(blockedContent ~! pDelims) ^^ { case (bc ~ d) => (bc -> d) }
        contentDelimReq
      }
      case true => {
        val content: Parser[(Vector[String], String)] = "generateEscapeBlockNoPadParser.content_OptDelims".!!!(blockedContent ~! (pDelims | EOF)) ^^ { case (bc ~ d) => (bc -> d) }
        content
      }
    }
    pFieldAndDelim
  }

  def generateEscapeBlockParser(separators: Set[String], terminators: Set[String],
    escapeBlockStart: String, escapeBlockEnd: String,
    escapeEscapeCharacter: String = "",
    justification: TextJustificationType.Type,
    padChar: String,
    isMissingDelimAllowed: Boolean = true): Parser[(Vector[String], String)] = {

    val hasEscEsc: Boolean = escapeEscapeCharacter.length() > 0
    val escapeBlockStartRegex = this.generateCharacterRegex(escapeBlockStart)
    val escapeBlockEndRegex = generateCharacterRegex(escapeBlockEnd)
    val escapeEscapeRegex = generateCharacterRegex(escapeEscapeCharacter)
    val padCharRegex = generateCharacterRegex(padChar);
    val (pSeps, sepsRegex) = this.generateSeparators(separators)
    val (pTerms, termsRegex) = this.generateTerminators(terminators)

    justification match {
      case TextJustificationType.None => return this.generateEscapeBlockNoPadParser(pSeps, pTerms,
        escapeBlockStartRegex, escapeBlockEndRegex, escapeEscapeRegex, hasEscEsc, isMissingDelimAllowed)
      case _ => return this.generateEscapeBlockWithPadParser(pSeps, pTerms,
        escapeBlockStartRegex, escapeBlockEndRegex, escapeEscapeRegex, padCharRegex, justification, hasEscEsc, isMissingDelimAllowed)
    }
  }

  def generateEscapeBlockParsers(separators: Set[String], terminators: Set[String],
    escapeBlockStart: String, escapeBlockEnd: String,
    escapeEscapeCharacter: String = "",
    justification: TextJustificationType.Type,
    padChar: String,
    isMissingDelimAllowed: Boolean = true): (Parser[(Vector[String], String)], Parser[String], Parser[String], Array[String], Array[String]) = {

    val hasEscEsc: Boolean = escapeEscapeCharacter.length() > 0
    val escapeBlockStartRegex = this.generateCharacterRegex(escapeBlockStart)
    val escapeBlockEndRegex = generateCharacterRegex(escapeBlockEnd)
    val escapeEscapeRegex = generateCharacterRegex(escapeEscapeCharacter)
    val padCharRegex = generateCharacterRegex(padChar);
    val (pSeps, sepsRegex) = this.generateSeparators(separators)
    val (pTerms, termsRegex) = this.generateTerminators(terminators)

    val escapeBlockParser = justification match {
      case TextJustificationType.None => this.generateEscapeBlockNoPadParser(pSeps, pTerms,
        escapeBlockStartRegex, escapeBlockEndRegex, escapeEscapeRegex, hasEscEsc, isMissingDelimAllowed)
      case _ => this.generateEscapeBlockWithPadParser(pSeps, pTerms,
        escapeBlockStartRegex, escapeBlockEndRegex, escapeEscapeRegex, padCharRegex, justification, hasEscEsc, isMissingDelimAllowed)
    }
    (escapeBlockParser, pSeps, pTerms, sepsRegex, termsRegex)
  }

  lazy val emptyParser = { parserAlwaysFail("")("empty delimiter list") }

  def generateEscapeBlockParsers2(delims: Parser[String],
    escapeBlockStart: String, escapeBlockEnd: String,
    escapeEscapeCharacter: String = "",
    justification: TextJustificationType.Type,
    padChar: String,
    isMissingDelimAllowed: Boolean = true): (Parser[(Vector[String], String)], String, String) = {

    val hasEscEsc: Boolean = escapeEscapeCharacter.length() > 0
    val escapeBlockStartRegex = this.generateCharacterRegex(escapeBlockStart)
    val escapeBlockEndRegex = generateCharacterRegex(escapeBlockEnd)
    val escapeEscapeRegex = generateCharacterRegex(escapeEscapeCharacter)
    val padCharRegex = generateCharacterRegex(padChar);

    val escapeBlockParser = justification match {
      case TextJustificationType.None => this.generateEscapeBlockNoPadParser(emptyParser, delims,
        escapeBlockStartRegex, escapeBlockEndRegex, escapeEscapeRegex, hasEscEsc, isMissingDelimAllowed)
      case _ => this.generateEscapeBlockWithPadParser(emptyParser, delims,
        escapeBlockStartRegex, escapeBlockEndRegex, escapeEscapeRegex, padCharRegex, justification, hasEscEsc, isMissingDelimAllowed)
    }
    (escapeBlockParser, escapeBlockEndRegex, escapeEscapeRegex)
  }

  protected def generateEscapeCharacterDiffWithPadParser(separators: Parser[String], terminators: Parser[String],
    escapeCharacterRegex: String, escapeEscapeCharacterRegex: String, padCharRegex: String,
    delimsRegex: String,
    justification: TextJustificationType.Type, hasEscEsc: Boolean, hasDelim: Boolean,
    isMissingDelimAllowed: Boolean = true): Parser[(Vector[String], String)] = {

    val pSeps: Parser[String] = "generateEscapeCharacterDiffWithPadParser.pSeps".!!!(separators)
    val pTerms: Parser[String] = "generateEscapeCharacterDiffWithPadParser.pTerms".!!!(terminators)
    val pDelims: Parser[String] = "generateEscapeCharacterDiffWithPadParser.pDelims".!!!(pSeps ||| pTerms)

    val pEscape: Parser[String] = "generateEscapeCharacterDiffWithPadParser.pEscape".!!!(escapeCharacterRegex.r)
    val pEscapeEscape: Parser[String] = "generateEscapeCharacterDiffWithPadParser.pEscapeEscape".!!!(escapeEscapeCharacterRegex.r)
    val pEscapedEscape = "generateEscapeCharacterDiffWithPadParser.pEscapedEscape".!!!(pEscapeEscape ~! pEscape)

    val pEscapeEscapeFollowedByEscapeFollowedByDelim = "generateEscapeCharacterDiffWithPadParser.pEscapeEscapeFollowedByEscapeFollowedByDelim".!!!((pEscapeEscape ~! pEscape) ~! pDelims) ^^ { case (eses ~ es ~ d) => d }
    val pDelimNotPrecededByEscape = "generateEscapeCharacterDiffWithPadParser.pDelimNotPrecededByEscape".!!!(not(pEscape) ~! pDelims) ^^ { case (notEsc ~ d) => d }
    val pEscEscFollowedByDelim = "generateEscapeCharacterDiffWithPadParser.pEscEscFollowedByDelim".!!!(pEscapeEscape ~! pDelims) ^^ { case (eses ~ d) => d }
    //val pUnescapedDelims = ((pEscapeEscape ~! pEscape) ~> pDelims) | (not(pEscape) ~> pDelims) | (pEscapeEscape ~> pDelims) | pDelims
    val pUnescapedDelims = "generateEscapeCharacterDiffWithPadParser.pUnescapedDelims".!!!(pEscapeEscapeFollowedByEscapeFollowedByDelim | pDelimNotPrecededByEscape | pEscEscFollowedByDelim | pDelims)

    //val rPadChar = """(%s*)"""
    val rPadChar = """(""" + padCharRegex + """*)"""
    val pPadChar: Parser[String] = "generateEscapeCharacterDiffWithPadParser.pPadChar".!!!(rPadChar.r)
    //val rLeftPadChar = """(%1$s*)(?=([^%1$s]))""" // LeftPad precedes non pad characters
    val rLeftPadChar = """(""" + padCharRegex + """*)(?=([^""" + padCharRegex + """]))""" // LeftPad precedes non pad characters
    val pLeftPadChar: Parser[String] = "generateEscapeCharacterDiffWithPadParser.pLeftPadChar".!!!(rLeftPadChar.format(padCharRegex).r)

    // Content is anything until:
    // 1. Padding ~ delimiter
    // 2. unescaped delimiter
    // 3. Optional Padding ~ EndOfData
    //    val rBefore = """(?s)(.*?)(?=(%4$s+%3$s)|((?<!(?<!%1$s)%2$s)%3$s)|(%4$s*\z))"""
    //    val rBeforeNoEscEsc = """(?s)(.*?)(?=(%3$s+%2$s)|((?<!%1$s)%2$s)|(%3$s*\z))"""
    //    val pBefore: Parser[String] = {
    //      if (hasEscEsc) rBefore.format(escapeEscapeCharacterRegex, escapeCharacterRegex, delimsRegex, padCharRegex).r
    //      else rBeforeNoEscEsc.format(escapeCharacterRegex, delimsRegex, padCharRegex).r
    //    }

    val pBefore: Parser[String] = {
      if (hasEscEsc) {
        val rBefore = """(?s)(.*?)(?=(""" + padCharRegex + """+""" + delimsRegex + """)|((?<!(?<!""" +
          escapeEscapeCharacterRegex + """)""" + escapeCharacterRegex + """)""" + delimsRegex + """)|(""" + padCharRegex + """*\z))"""
        "generateEscapeCharacterDiffWithPadParser.pBeforeWithEscEsc".!!!(rBefore.r)
      } else {
        val rBeforeNoEscEsc = """(?s)(.*?)(?=(""" + padCharRegex + """+""" + delimsRegex + """)|((?<!""" +
          escapeCharacterRegex + """)""" + delimsRegex + """)|(""" + padCharRegex + """*\z))"""
        "generateEscapeCharacterDiffWithPadParser.BeforeNoEscEsc".!!!(rBeforeNoEscEsc.r)
      }
    }

    //    val rBeforeIgnoreTrailingPadding = """(?s)(.*?)(?=(?:(?<!(?<!%1$s)%2$s)%3$s)|\z)"""
    //    val rBeforeIgnoreTrailingPaddingNoEscEsc = """(?s)(.*?)(?=(?:(?<!%1$s)%2$s)|\z)"""
    //    val pBeforeIgnoreTrailingPadding: Parser[String] = {
    //      if (hasEscEsc) rBeforeIgnoreTrailingPadding.format(escapeEscapeCharacterRegex, escapeCharacterRegex, delimsRegex).r
    //      else rBeforeIgnoreTrailingPaddingNoEscEsc.format(escapeCharacterRegex, delimsRegex).r
    //    }

    val pBeforeIgnoreTrailingPadding: Parser[String] = {
      if (hasEscEsc) {
        val rBeforeIgnoreTrailingPadding = """(?s)(.*?)(?=(?:(?<!(?<!""" + escapeEscapeCharacterRegex + """)""" +
          escapeCharacterRegex + """)""" + delimsRegex + """)|\z)"""
        "generateEscapeCharacterDiffWithPadParser.BeforeIgnoreTrailingPadding".!!!(rBeforeIgnoreTrailingPadding.r)
      } else {
        val rBeforeIgnoreTrailingPaddingNoEscEsc = """(?s)(.*?)(?=(?:(?<!""" + escapeCharacterRegex + """)""" +
          delimsRegex + """)|\z)"""
        "generateEscapeCharacterDiffWithPadParser.BeforeIgnoreTrailingPaddingNoEscEsc".!!!(rBeforeIgnoreTrailingPaddingNoEscEsc.r)
      }
    }

    //    val rBeforeNoPadding = """(?s)(.*?)(?=((?<!(?<!%1$s)%2$s)%3$s)|(\z))"""
    //    val rBeforeNoPaddingNoEscEsc = """(?s)(.*?)(?=((?<!%1$s)%2$s)|(\z))"""
    //    val pBeforeNoPadding: Parser[String] = {
    //      if (hasEscEsc) rBeforeNoPadding.format(escapeEscapeCharacterRegex, escapeCharacterRegex, delimsRegex).r
    //      else rBeforeNoPaddingNoEscEsc.format(escapeCharacterRegex, delimsRegex).r
    //    }

    val pBeforeNoPadding: Parser[String] = {
      if (hasEscEsc) {
        val rBeforeNoPadding = """(?s)(.*?)(?=((?<!(?<!""" + escapeEscapeCharacterRegex + """)""" +
          escapeCharacterRegex + """)""" + delimsRegex + """)|(\z))"""
        "generateEscapeCharacterDiffWithPadParser.BeforeNoPadding".!!!(rBeforeNoPadding.format(escapeEscapeCharacterRegex, escapeCharacterRegex, delimsRegex).r)
      } else {
        val rBeforeNoPaddingNoEscEsc = """(?s)(.*?)(?=((?<!""" + escapeCharacterRegex + """)""" +
          delimsRegex + """)|(\z))"""
        "generateEscapeCharacterDiffWithPadParser.BeforeNoPaddingNoEscEsc".!!!(rBeforeNoPaddingNoEscEsc.format(escapeCharacterRegex, delimsRegex).r)
      }
    }

    val rBeforeNoPaddingOrDelims = """(?s)(.*?)(?=(\z))"""
    val pBeforeNoPaddingOrDelims: Parser[String] = "generateEscapeCharacterDiffWithPadParser.pBeforeNoPaddingOrDelims".!!!(rBeforeNoPaddingOrDelims.r)

    //    val rBeforeNoDelims = """(?s)(.*?)(?=(%1$s*\z))"""
    //    val pBeforeNoDelims: Parser[String] = rBeforeNoDelims.format(padCharRegex).r
    val rBeforeNoDelims = """(?s)(.*?)(?=(""" + padCharRegex + """*\z))"""
    val pBeforeNoDelims: Parser[String] = "generateEscapeCharacterDiffWithPadParser.pBeforeNoDelims".!!!(rBeforeNoDelims.r)

    val paddedContent: Parser[Vector[String]] = "generateEscapeCharacterDiffWithPadParser.paddedContent".!!!(pLeftPadChar ~! pBefore ~! pPadChar) ^^ { case (lp ~ c ~ rp) => Vector(lp, c, rp) }
    val leftPaddedContent: Parser[Vector[String]] = "generateEscapeCharacterDiffWithPadParser.leftPaddedContent".!!!(pLeftPadChar ~! pBefore) ^^ { case (lp ~ c) => Vector(lp, c) }
    val rightPaddedContent: Parser[Vector[String]] = "generateEscapeCharacterDiffWithPadParser.rightPaddedContent".!!!(pBefore ~! pPadChar) ^^ { case (c ~ rp) => Vector(c, rp) }

    val pFieldAndDelim: Parser[(Vector[String], String)] = (justification, isMissingDelimAllowed, hasDelim) match {
      case (TextJustificationType.Left, false, true) => {
        val contentLeftDelimReq: Parser[(Vector[String], String)] = "generateEscapeCharacterDiffWithPadParser.contentLeft_ReqDelims".!!!(rightPaddedContent ~! pUnescapedDelims) ^^ { case (c ~ d) => (c, d) }
        contentLeftDelimReq
      }
      case (TextJustificationType.Right, false, true) => {
        val contentRightDelimReq: Parser[(Vector[String], String)] = "generateEscapeCharacterDiffWithPadParser.contentRight_ReqDelims".!!!(leftPaddedContent ~! pUnescapedDelims) ^^ { case (c ~ d) => (c, d) }
        contentRightDelimReq
      }
      case (TextJustificationType.Center, false, true) => {
        val contentCenterDelimReq: Parser[(Vector[String], String)] = "generateEscapeCharacterDiffWithPadParser.contentCenter_ReqDelims".!!!(paddedContent ~! pDelims) ^^ { case (bc ~ d) => (bc -> d) }
        contentCenterDelimReq
      }
      case (TextJustificationType.Left, true, true) => {
        val contentLeft: Parser[(Vector[String], String)] = rightPaddedContent ~! (pUnescapedDelims | EOF) ^^ { case (c ~ d) => (c, d) }
        "generateEscapeCharacterDiffWithPadParser.contentLeft_OptDelims".!!!(contentLeft)
      }
      case (TextJustificationType.Left, true, false) => {
        val rightPaddedContent: Parser[Vector[String]] = "generateEscapeCharacterDiffWithPadParser.rightPaddedContent_NoDelims".!!!(pBeforeNoDelims ~! pPadChar) ^^ { case (c ~ rp) => Vector(c, rp) }
        val contentLeft: Parser[(Vector[String], String)] = rightPaddedContent ~! (EOF) ^^ { case (c ~ d) => (c, d) }
        "generateEscapeCharacterDiffWithPadParser.contentLeft_NoDelims".!!!(contentLeft)
      }
      case (TextJustificationType.Right, true, true) => {
        val contentRight: Parser[(Vector[String], String)] = leftPaddedContent ~! (pUnescapedDelims | EOF) ^^ { case (c ~ d) => (c, d) }
        "generateEscapeCharacterDiffWithPadParser.contentRight_OptDelims".!!!(contentRight)
      }
      case (TextJustificationType.Right, true, false) => {
        val leftPaddedContent: Parser[Vector[String]] = "generateEscapeCharacterDiffWithPadParser.leftPaddedContent_NoDelims".!!!(pLeftPadChar ~! pBeforeNoDelims) ^^ { case (lp ~ c) => Vector(lp, c) }
        val contentRight: Parser[(Vector[String], String)] = leftPaddedContent ~! (EOF) ^^ { case (c ~ d) => (c, d) }
        "generateEscapeCharacterDiffWithPadParser.contentRight_NoDelims".!!!(contentRight)
      }
      case (TextJustificationType.Center, true, true) => {
        val contentCenter: Parser[(Vector[String], String)] = paddedContent ~! (pUnescapedDelims | EOF) ^^ { case (c ~ d) => (c, d) }
        "generateEscapeCharacterDiffWithPadParser.contentCenter_OptDelims".!!!(contentCenter)
      }
      case (TextJustificationType.Center, true, false) => {
        val paddedContent: Parser[Vector[String]] = "generateEscapeCharacterDiffWithPadParser.paddedContent_NoDelims".!!!(pLeftPadChar ~! pBeforeNoDelims ~! pPadChar) ^^ { case (lp ~ c ~ rp) => Vector(lp, c, rp) }
        val contentCenter: Parser[(Vector[String], String)] = paddedContent ~! (EOF) ^^ { case (c ~ d) => (c, d) }
        "generateEscapeCharacterDiffWithPadParser.contentCenter_NoDelims".!!!(contentCenter)
      }
      case _ => Assert.invariantFailed("illegal combination") //return failedResult
    }
    pFieldAndDelim
  }

  protected def generateEscapeCharacterDiffNoPadParser(separators: Parser[String], terminators: Parser[String],
    escapeCharacterRegex: String, escapeEscapeCharacterRegex: String, delimsRegex: String,
    hasEscEsc: Boolean, hasDelim: Boolean,
    isMissingDelimAllowed: Boolean = true): Parser[(Vector[String], String)] = {

    val pSeps: Parser[String] = "generateEscapeCharacterDiffNoPadParser.pSeps".!!!(separators)
    val pTerms: Parser[String] = "generateEscapeCharacterDiffNoPadParser.pTerms".!!!(terminators)
    val pDelims: Parser[String] = "generateEscapeCharacterDiffNoPadParser.pDelims".!!!(pSeps ||| pTerms)

    val pEscape: Parser[String] = "generateEscapeCharacterDiffNoPadParser.pEscape".!!!(escapeCharacterRegex.r)
    val pEscapeEscape: Parser[String] = "generateEscapeCharacterDiffNoPadParser.pEscapeEscape".!!!(escapeEscapeCharacterRegex.r)
    val pEscapedEscape = "generateEscapeCharacterDiffNoPadParser.pEscapedEscape".!!!(pEscapeEscape ~! pEscape)
    //val pUnescapedDelims = ((pEscapeEscape ~ pEscape) ~> pDelims) | (not(pEscape) ~> pDelims) | (pEscapeEscape ~> pDelims) | pDelims
    //    val pUnescapedDelims = {
    //      if (hasEscEsc) { ((pEscapeEscape ~! pEscape) ~> pDelims) | (not(pEscape) ~> pDelims) | (pEscapeEscape ~> pDelims) | pDelims }
    //      else { (not(pEscape) ~> pDelims) | pDelims }
    //    }

    // TODO: Same as generateEscapeCharacterDiffWithPadParser, look at generating this separately and passing in
    //val pEscapeEscapeFollowedByEscapeFollowedByDelim = ((pEscapeEscape ~! pEscape) ~! pDelims) ^^ { case (eses ~ es ~ d) => d }
    val pDelimNotPrecededByEscape = "generateEscapeCharacterDiffNoPadParser.pDelimNotPrecededByEscape".!!!(not(pEscape) ~! pDelims) ^^ { case (notEsc ~ d) => d }
    //val pEscEscFollowedByDelim = (pEscapeEscape ~! pDelims) ^^ { case (eses ~ d) => d }
    //val pUnescapedDelims = pEscapeEscapeFollowedByEscapeFollowedByDelim | pDelimNotPrecededByEscape | pEscEscFollowedByDelim | pDelims
    val pUnescapedDelims = "generateEscapeCharacterDiffNoPadParser.pUnescapedDelims".!!! {
      if (hasEscEsc) {
        // Seems like this is a case where backtracking has to be allowed for pDelimNotPrecededByEscape
        val pEscapeEscapeFollowedByEscapeFollowedByDelim = "generateEscapeCharacterDiffNoPadParser.pEscapeEscapeFollowedByEscapeFollowedByDelim".!!!((pEscapeEscape ~! pEscape) ~! pDelims) ^^ { case (eses ~ es ~ d) => d }
        val pDelimNotPrecededByEscape = "generateEscapeCharacterDiffNoPadParser.pDelimNotPrecededByEscape".!!!(not(pEscape) ~ pDelims) ^^ { case (notEsc ~ d) => d }
        val pEscEscFollowedByDelim = "generateEscapeCharacterDiffNoPadParser.pEscEscFollowedByDelim".!!!(pEscapeEscape ~! pDelims) ^^ { case (eses ~ d) => d }
        pEscapeEscapeFollowedByEscapeFollowedByDelim | pDelimNotPrecededByEscape | pEscEscFollowedByDelim | pDelims
      } else {
        pDelimNotPrecededByEscape | pDelims
      }
    }

    //    val rBeforeIgnoreTrailingPadding = """(?s)(.*?)(?=(?:(?<!(?<!%1$s)%2$s)%3$s)|\z)"""
    //    val rBeforeIgnoreTrailingPaddingNoEscEsc = """(?s)(.*?)(?=(?:(?<!%1$s)%2$s)|\z)"""
    //    val pBeforeIgnoreTrailingPadding: Parser[String] = {
    //      if (hasEscEsc) rBeforeIgnoreTrailingPadding.format(escapeEscapeCharacterRegex, escapeCharacterRegex, delimsRegex).r
    //      else rBeforeIgnoreTrailingPaddingNoEscEsc.format(escapeCharacterRegex, delimsRegex).r
    //    }

    val pBeforeIgnoreTrailingPadding: Parser[String] = {
      if (hasEscEsc) {
        val rBeforeIgnoreTrailingPadding = """(?s)(.*?)(?=(?:(?<!(?<!""" + escapeEscapeCharacterRegex + """)""" +
          escapeCharacterRegex + """)(""" + delimsRegex + """))|\z)"""
        "generateEscapeCharacterDiffNoPadParser.BeforeIgnoreTrailingPadding".!!!(rBeforeIgnoreTrailingPadding.r)
      } else {
        val rBeforeIgnoreTrailingPaddingNoEscEsc = """(?s)(.*?)(?=(?:(?<!""" + escapeCharacterRegex + """)(""" +
          delimsRegex + """))|\z)"""
        "generateEscapeCharacterDiffNoPadParser.BeforeIgnoreTrailingPaddingNoEscEsc".!!!(rBeforeIgnoreTrailingPaddingNoEscEsc.r)
      }
    }

    val pBeforeNoPadding: Parser[String] = {
      if (hasEscEsc) {
        val rBeforeNoPadding = """(?s)(.*?)(?=((?<!(?<!""" + escapeEscapeCharacterRegex + """)""" + escapeCharacterRegex + """)(""" + delimsRegex + """))|(\z))"""
        "generateEscapeCharacterDiffNoPadParser.BeforeNoPadding".!!!(rBeforeNoPadding.r)
      } else {
        val rBeforeNoPaddingNoEscEsc = """(?s)(.*?)(?=((?<!""" + escapeCharacterRegex + """)(""" + delimsRegex + """))|(\z))"""
        "generateEscapeCharacterDiffNoPadParser.BeforeNoPaddingNoEscEsc".!!!(rBeforeNoPaddingNoEscEsc.r)
      }
    }

    val rBeforeNoPaddingOrDelims = """(?s)(.*?)(?=(\z))"""
    val pBeforeNoPaddingOrDelims: Parser[String] = "generateEscapeCharacterDiffNoPadParser.pBeforeNoPaddingOrDelims".!!!(rBeforeNoPaddingOrDelims.r)

    val rBeforeNoDelims = """(?s)(.*?)(?=(\z))"""
    val pBeforeNoDelims: Parser[String] = "generateEscapeCharacterDiffNoPadParser.pBeforeNoDelims".!!!(rBeforeNoDelims.r)

    val pFieldAndDelim: Parser[(Vector[String], String)] = (isMissingDelimAllowed, hasDelim) match {
      case (false, true) => {
        val contentDelimReq: Parser[(Vector[String], String)] = pBeforeNoPadding ~! pUnescapedDelims ^^ { case (b ~ d) => (Vector(b) -> d) }
        "generateEscapeCharacterDiffNoPadParser.content_ReqDelims".!!!(contentDelimReq)
      }
      case (true, true) => {
        val content: Parser[(Vector[String], String)] = pBeforeIgnoreTrailingPadding ~! (pUnescapedDelims | EOF) ^^ { case (b ~ d) => (Vector(b) -> d) }
        "generateEscapeCharacterDiffNoPadParser.content_OptDelims".!!!(content)
      }
      case (true, false) => {
        val content: Parser[(Vector[String], String)] = pBeforeNoPaddingOrDelims ~! (EOF) ^^ { case (b ~ d) => (Vector(b) -> d) }
        "generateEscapeCharacterDiffNoPadParser.content_NoDelims".!!!(content)
      }
      case _ => Assert.invariantFailed() // return failedResult
    }
    pFieldAndDelim
  }

  protected def generateEscapeCharacterSameNoPadParser(separators: Parser[String], terminators: Parser[String],
    escapeCharacterRegex: String, delimsRegex: String, hasDelim: Boolean,
    isMissingDelimAllowed: Boolean = true): Parser[(Vector[String], String)] = {

    val pSeps: Parser[String] = "generateEscapeCharacterSameNoPadParser.pSeps".!!!(separators)
    val pTerms: Parser[String] = "generateEscapeCharacterSameNoPadParser.pTerms".!!!(terminators)
    val pDelims: Parser[String] = "generateEscapeCharacterSameNoPadParser.pDelims".!!!(pSeps ||| pTerms)

    val pEscape: Parser[String] = "generateEscapeCharacterSameNoPadParser.pEscape".!!!(escapeCharacterRegex.r)
    val pEscapedEscape = "generateEscapeCharacterSameNoPadParser.pEscapedEscape".!!!(pEscape ~! pEscape) ^^ { case (e1 ~ e2) => (e1 + e2) } // concatenate escapes

    // Parser captures and creates a string representation of the escapes
    val pEscapes = "generateEscapeCharacterSameNoPadParser.pEscapes".!!!((pEscapedEscape.*) ~! opt(pEscape)) ^^ {
      case (l ~ None) => l.mkString
      case (l ~ Some(esc)) => l.mkString + esc
    }

    //    val rBeforeIgnoreTrailingPadding = """(?s)(.*?)(?=((?<!%1$s)((%1$s%1$s)*)(%2$s))|(\z))"""
    //    val pBeforeIgnoreTrailingPadding: Parser[String] = rBeforeIgnoreTrailingPadding.format(escapeCharacterRegex, delimsRegex).r
    val rBeforeIgnoreTrailingPadding = """(?s)(.*?)(?=((?<!""" + escapeCharacterRegex + """)((""" + escapeCharacterRegex + escapeCharacterRegex + """)*)(""" + delimsRegex + """))|(\z))"""
    val pBeforeIgnoreTrailingPadding: Parser[String] = "generateEscapeCharacterSameNoPadParser.pBeforeIgnoreTrailingPadding".!!!(rBeforeIgnoreTrailingPadding.r)

    //    val rBeforeNoPadding = """(?s)(.*?)(?=(""" + // Give me everything from this point until...
    //      """((?<!%1$s)((%1$s%1$s)*)(%2$s))""" + // unescaped delimiter
    //      """|""" + // OR
    //      """\z))""" // End of data/file
    //    val pBeforeNoPadding: Parser[String] = rBeforeNoPadding.format(escapeCharacterRegex, delimsRegex).r
    val rBeforeNoPadding = """(?s)(.*?)(?=(""" + // Give me everything from this point until...
      """((?<!""" + escapeCharacterRegex + """)((""" +
      escapeCharacterRegex + escapeCharacterRegex + """)*)(""" + delimsRegex + """))""" + // unescaped delimiter
      """|""" + // OR
      """\z))""" // End of data/file

    val pBeforeNoPadding: Parser[String] = "generateEscapeCharacterSameNoPadParser.pBeforeNoPadding".!!!(rBeforeNoPadding.r)

    val rBeforeNoPaddingOrDelim = """(?s)(.*?)(?=(\z))"""
    val pBeforeNoPaddingOrDelim: Parser[String] = "generateEscapeCharacterSameNoPadParser.pBeforeNoPaddingOrDelim".!!!(rBeforeNoPaddingOrDelim.r)

    // Here because of the nature of using the same character for escape and escapeEscape
    // we need to capture the escapes if they exist and make them part of the 'before'
    val pBeforeAndEscsIgnoreTrailingPadding = "generateEscapeCharacterSameNoPadParser.pBeforeAndEscsIgnoreTrailingPadding".!!!(pBeforeIgnoreTrailingPadding ~! opt(pEscapes)) ^^ {
      case (b ~ None) => b
      case (b ~ Some(e)) => (b + e)
    }
    val pBeforeAndEscsNoPadding = "generateEscapeCharacterSameNoPadParser.pBeforeAndEscsNoPadding".!!!(pBeforeNoPadding ~! opt(pEscapes)) ^^ {
      case (b ~ None) => b
      case (b ~ Some(e)) => (b + e)
    }

    val pEscapedEscapeFollowedByDelim = "generateEscapeCharacterSameNoPadParser.pEscapedEscapeFollowedByDelim".!!!(pEscapedEscape ~! pDelims) ^^ { case (eses ~ d) => d }
    val pDelimNotPrecededByEscape = "generateEscapeCharacterSameNoPadParser.pDelimNotPrecededByEscape".!!!(not(pEscape) ~ pDelims) ^^ { case (notEsc ~ d) => d } // Backtracking has to be allowed here
    //val pUnescapedDelims = ((pEscapedEscape) ~> pDelims) | (not(pEscape) ~> pDelims) | pDelims
    val pUnescapedDelims = "generateEscapeCharacterSameNoPadParser.pUnescapedDelims".!!!(pEscapedEscapeFollowedByDelim | pDelimNotPrecededByEscape | pDelims)

    val pFieldAndDelim: Parser[(Vector[String], String)] = (isMissingDelimAllowed, hasDelim) match {
      case (false, true) => {
        val contentDelimReq: Parser[(Vector[String], String)] = "generateEscapeCharacterSameNoPadParser.content_ReqDelims".!!!(pBeforeAndEscsIgnoreTrailingPadding ~! pUnescapedDelims) ^^ { case (b ~ d) => (Vector(b) -> d) }
        contentDelimReq
      }
      case (true, true) => {
        val content: Parser[(Vector[String], String)] = "generateEscapeCharacterSameNoPadParser.content_OptDelims".!!!((pBeforeAndEscsNoPadding) ~! (pUnescapedDelims | EOF)) ^^ { case (b ~ d) => (Vector(b) -> d) }
        content
      }
      case (true, false) => {
        val content: Parser[(Vector[String], String)] = "generateEscapeCharacterSameNoPadParser.content_NoDelims".!!!(pBeforeNoPaddingOrDelim ~! (EOF)) ^^ { case (b ~ d) => (Vector(b) -> d) }
        content
      }
      case _ => Assert.invariantFailed() // return failedResult
    }
    pFieldAndDelim
  }

  protected def generateEscapeCharacterSameWithPadParser(separators: Parser[String], terminators: Parser[String],
    escapeCharacterRegex: String, delimsRegex: String,
    justification: TextJustificationType.Type,
    padCharRegex: String, hasDelim: Boolean,
    isMissingDelimAllowed: Boolean = true): Parser[(Vector[String], String)] = {

    val pSeps: Parser[String] = "generateEscapeCharacterSameWithPadParser.pSeps".!!!(separators)
    val pTerms: Parser[String] = "generateEscapeCharacterSameWithPadParser.pTerms".!!!(terminators)
    val pDelims: Parser[String] = "generateEscapeCharacterSameWithPadParser.pDelims".!!!(pSeps ||| pTerms)

    val pEscape: Parser[String] = "generateEscapeCharacterSameWithPadParser.pEscape".!!!(escapeCharacterRegex.r)
    val pEscapedEscape = "generateEscapeCharacterSameWithPadParser.pEscapedEscape".!!!(pEscape ~! pEscape) ^^ { case (e1 ~ e2) => (e1 + e2) } // concatenate escapes

    // Parser captures and creates a string representation of the escapes
    val pEscapes = "generateEscapeCharacterSameWithPadParser.pEscapes".!!!((pEscapedEscape.*) ~! opt(pEscape)) ^^ {
      case (l ~ None) => l.mkString
      case (l ~ Some(esc)) => l.mkString + esc
    }

    // Need to tolerate reading to end of data here
    //
    // Assumptions:
    //	A field can be terminated by...
    //		Unescaped delimiter
    //		Padding followed by unescaped delimiter
    //		Padding followed by end of data
    //		End of data
    //
    //    val rBefore = """(?s)(.*?)(?=""" + // Give me everything from this point until...
    //      """(?:(%1$s*)(%3$s*)(((?<!%1$s)(%2$s))|(\z)))""" + // An unescaped/escaped pad char followed by either an unescaped delimiter or end of data
    //      """|""" + // OR
    //      """(%1$s(%3$s+)(%2$s|\z))""" + // escape followed by one or more pad chars followed by a delimiter or end of data
    //      """|""" + // OR
    //      """((?<!%1$s)((%1$s%1$s)*)(%2$s))""" + // unescaped delimiter
    //      """|""" + // OR
    //      """\z)""" // End of data/file
    //    val pBefore: Parser[String] = rBefore.format(escapeCharacterRegex, delimsRegex, padCharRegex).r

    val rBefore = """(?s)(.*?)(?=""" + // Give me everything from this point until...
      """(?:(""" + escapeCharacterRegex + """*)(""" + padCharRegex + """*)(((?<!""" +
      escapeCharacterRegex + """)(""" + delimsRegex + """))|(\z)))""" + // An unescaped/escaped pad char followed by either an unescaped delimiter or end of data
      """|""" + // OR
      """(""" + escapeCharacterRegex + """(""" + padCharRegex + """+)(""" + delimsRegex + """|\z))""" + // escape followed by one or more pad chars followed by a delimiter or end of data
      """|""" + // OR
      """((?<!""" + escapeCharacterRegex + """)((""" + escapeCharacterRegex + escapeCharacterRegex + """)*)(""" + delimsRegex + """))""" + // unescaped delimiter
      """|""" + // OR
      """\z)""" // End of data/file
    val pBefore: Parser[String] = "generateEscapeCharacterSameWithPadParser.pBefore".!!!(rBefore.r)

    //    val rBeforeIgnoreTrailingPadding = """(?s)(.*?)(?=((?<!%1$s)((%1$s%1$s)*)(%2$s))|(\z))"""
    //    val pBeforeIgnoreTrailingPadding: Parser[String] = rBeforeIgnoreTrailingPadding.format(escapeCharacterRegex, delimsRegex).r
    val rBeforeIgnoreTrailingPadding = """(?s)(.*?)(?=((?<!""" + escapeCharacterRegex + """)((""" +
      escapeCharacterRegex + escapeCharacterRegex + """)*)(""" + delimsRegex + """))|(\z))"""
    val pBeforeIgnoreTrailingPadding: Parser[String] = "generateEscapeCharacterSameWithPadParser.pBeforeIgnoreTrailingPadding".!!!(rBeforeIgnoreTrailingPadding.r)

    //    val rBeforeNoPadding = """(?s)(.*?)(?=(""" + // Give me everything from this point until...
    //      """((?<!%1$s)((%1$s%1$s)*)(%2$s))""" + // unescaped delimiter
    //      """|""" + // OR
    //      """\z))""" // End of data/file
    //    val pBeforeNoPadding: Parser[String] = rBeforeNoPadding.format(escapeCharacterRegex, delimsRegex).r

    // TODO: This appears to be used in multiple places, pull out
    val rBeforeNoPadding = """(?s)(.*?)(?=(""" + // Give me everything from this point until...
      """((?<!""" + escapeCharacterRegex + """)((""" + escapeCharacterRegex + escapeCharacterRegex + """)*)(""" + delimsRegex + """))""" + // unescaped delimiter
      """|""" + // OR
      """\z))""" // End of data/file
    val pBeforeNoPadding: Parser[String] = "generateEscapeCharacterSameWithPadParser.pBeforeNoPadding".!!!(rBeforeNoPadding.r)

    val rBeforeNoPaddingOrDelim = """(?s)(.*?)(?=(\z))"""
    val pBeforeNoPaddingOrDelim: Parser[String] = "generateEscapeCharacterSameWithPadParser.pBeforeNoPaddingOrDelim".!!!(rBeforeNoPaddingOrDelim.r)

    //    val rBeforeNoDelim = """(?s)(.*?)(?=""" + // Give me everything from this point until...
    //      """(?:(%1$s*)(%3$s*)(\z))""" + // An unescaped/escaped pad char followed by end of data
    //      """|""" + // OR
    //      """(%1$s(%3$s+)(\z))""" + // escape followed by one or more pad chars followed by a delimiter or end of data
    //      """|""" + // OR
    //      """\z)""" // End of data/file
    //    val pBeforeNoDelim: Parser[String] = rBeforeNoDelim.format(escapeCharacterRegex, delimsRegex, padCharRegex).r

    val rBeforeNoDelim = """(?s)(.*?)(?=""" + // Give me everything from this point until...
      """(?:(""" + escapeCharacterRegex + """*)(""" + padCharRegex + """*)(\z))""" + // An unescaped/escaped pad char followed by end of data
      """|""" + // OR
      """(""" + escapeCharacterRegex + """(""" + padCharRegex + """+)(\z))""" + // escape followed by one or more pad chars followed by a delimiter or end of data
      """|""" + // OR
      """\z)""" // End of data/file
    val pBeforeNoDelim: Parser[String] = "generateEscapeCharacterSameWithPadParser.pBeforeNoDelim".!!!(rBeforeNoDelim.r)

    // Here because of the nature of using the same character for escape and escapeEscape
    // we need to capture the escapes if they exist and make them part of the 'before'
    val pBeforeAndEscsIgnoreTrailingPadding = "generateEscapeCharacterSameWithPadParser.pBeforeAndEscsIgnoreTrailingPadding".!!!(pBeforeIgnoreTrailingPadding ~! opt(pEscapes)) ^^ {
      case (b ~ None) => b
      case (b ~ Some(e)) => (b + e)
    }

    val pBeforeAndEscs = "generateEscapeCharacterSameWithPadParser.pBeforeAndEscs".!!!(pBefore ~! opt(pEscapes | pEscapedEscape | pEscape)) ^^ {
      case (b ~ None) => b
      case (b ~ Some(e)) => (b + e)
    }

    val pEscapedEscapeFollowedByDelim = "generateEscapeCharacterSameWithPadParser.pEscapedEscapeFollwedByDelim".!!!(pEscapedEscape ~! pDelims) ^^ { case (eses ~ d) => d }
    val pDelimNotPrecededByEscape = "generateEscapeCharacterSameWithPadParser.pDelimNotPrecededByEscape".!!!(not(pEscape) ~! pDelims) ^^ { case (notEsc ~ d) => d }
    //val pUnescapedDelims = ((pEscapedEscape) ~> pDelims) | (not(pEscape) ~> pDelims) | pDelims
    val pUnescapedDelims = "generateEscapeCharacterSameWithPadParser.pUnescapedDelims".!!!(pEscapedEscapeFollowedByDelim | pDelimNotPrecededByEscape | pDelims)

    //    val rPadChar = """(%s*)"""
    //    val pPadChar: Parser[String] = rPadChar.format(padCharRegex).r
    val rPadChar = """(""" + padCharRegex + """*)"""
    val pPadChar: Parser[String] = "generateEscapeCharacterSameWithPadParser.pPadChar".!!!(rPadChar.r)
    //    val rLeftPadChar = """(%1$s*)(?=([^%1$s]))""" // LeftPad precedes non pad characters
    //    val pLeftPadChar: Parser[String] = rLeftPadChar.format(padCharRegex).r
    val rLeftPadChar = """(""" + padCharRegex + """*)(?=([^""" + padCharRegex + """]))""" // LeftPad precedes non pad characters
    val pLeftPadChar: Parser[String] = "generateEscapeCharacterSameWithPadParser.pLeftPadChar".!!!(rLeftPadChar.r)

    val paddedContent: Parser[Vector[String]] = "generateEscapeCharacterSameWithPadParser.paddedContent".!!!(pLeftPadChar ~! pBeforeAndEscs ~! pPadChar) ^^ { case (lp ~ c ~ rp) => Vector(lp, c, rp) }
    val leftPaddedContent: Parser[Vector[String]] = "generateEscapeCharacterSameWithPadParser.leftPaddedContent".!!!(pLeftPadChar ~! pBeforeAndEscsIgnoreTrailingPadding) ^^ { case (lp ~ c) => Vector(lp, c) }
    val rightPaddedContent: Parser[Vector[String]] = "generateEscapeCharacterSameWithPadParser.rightPaddedContent".!!!(pBeforeAndEscs ~! pPadChar) ^^ { case (c ~ rp) => Vector(c, rp) }

    //val hasDelim: Boolean = separators.size > 0 || terminators.size > 0

    val pFieldAndDelim: Parser[(Vector[String], String)] = (justification, isMissingDelimAllowed, hasDelim) match {
      case (TextJustificationType.Left, false, true) => {
        val contentLeftDelimReq: Parser[(Vector[String], String)] = rightPaddedContent ~! pUnescapedDelims ^^ { case (c ~ d) => (c, d) }
        "generateEscapeCharacterSameWithPadParser.contentLeft_ReqDelims".!!!(contentLeftDelimReq)
      }
      case (TextJustificationType.Right, false, true) => {
        val contentRightDelimReq: Parser[(Vector[String], String)] = leftPaddedContent ~! pUnescapedDelims ^^ { case (c ~ d) => (c, d) }
        "generateEscapeCharacterSameWithPadParser.contentRight_ReqDelims".!!!(contentRightDelimReq)
      }
      case (TextJustificationType.Center, false, true) => {
        val contentCenterDelimReq: Parser[(Vector[String], String)] = paddedContent ~! pDelims ^^ { case (bc ~ d) => (bc -> d) }
        "generateEscapeCharacterSameWithPadParser.contentCenter_ReqDelims".!!!(contentCenterDelimReq)
      }
      case (TextJustificationType.Left, true, true) => {
        val contentLeft: Parser[(Vector[String], String)] = rightPaddedContent ~! (pUnescapedDelims | EOF) ^^ { case (c ~ d) => (c, d) }
        "generateEscapeCharacterSameWithPadParser.contentLeft_OptDelims".!!!(contentLeft)
      }
      case (TextJustificationType.Left, true, false) => {
        val pBeforeAndEscs = "generateEscapeCharacterSameWithPadParser.pBeforeAndEscs".!!!(pBeforeNoDelim ~! opt(pEscapes | pEscapedEscape | pEscape)) ^^ {
          case (b ~ None) => b
          case (b ~ Some(e)) => (b + e)
        }
        val rightPaddedContent: Parser[Vector[String]] = "generateEscapeCharacterSameWithPadParser.rightPaddedContent".!!!(pBeforeAndEscs ~! pPadChar) ^^ { case (c ~ rp) => Vector(c, rp) }
        val contentLeft: Parser[(Vector[String], String)] = rightPaddedContent ~! (EOF) ^^ { case (c ~ d) => (c, d) }
        "generateEscapeCharacterSameWithPadParser.contentLeft_NoDelims".!!!(contentLeft)
      }
      case (TextJustificationType.Right, true, true) => {
        val contentRight: Parser[(Vector[String], String)] = leftPaddedContent ~! (pUnescapedDelims | EOF) ^^ { case (c ~ d) => (c, d) }
        "generateEscapeCharacterSameWithPadParser.contentRight_OptDelims".!!!(contentRight)
      }
      case (TextJustificationType.Right, true, false) => {
        val pBeforeAndEscsIgnoreTrailingPadding = "generateEscapeCharacterSameWithPadParser.pBeforeAndEscsIgnoreTrailingPadding".!!!(pBeforeNoDelim ~! opt(pEscapes)) ^^ {
          case (b ~ None) => b
          case (b ~ Some(e)) => (b + e)
        }
        val leftPaddedContent: Parser[Vector[String]] = "generateEscapeCharacterSameWithPadParser.leftPaddedContent".!!!(pLeftPadChar ~! pBeforeAndEscsIgnoreTrailingPadding) ^^ { case (lp ~ c) => Vector(lp, c) }
        val contentRight: Parser[(Vector[String], String)] = leftPaddedContent ~! (EOF) ^^ { case (c ~ d) => (c, d) }
        "generateEscapeCharacterSameWithPadParser.contentRight_NoDelims".!!!(contentRight)
      }
      case (TextJustificationType.Center, true, true) => {
        val contentCenter: Parser[(Vector[String], String)] = paddedContent ~! (pUnescapedDelims | EOF) ^^ { case (c ~ d) => (c, d) }
        "generateEscapeCharacterSameWithPadParser.contentCenter_OptDelims".!!!(contentCenter)
      }
      case (TextJustificationType.Center, true, false) => {
        val pBeforeAndEscs = "generateEscapeCharacterSameWithPadParser.pBeforeAndEscs".!!!(pBeforeNoDelim ~! opt(pEscapes | pEscapedEscape | pEscape)) ^^ {
          case (b ~ None) => b
          case (b ~ Some(e)) => (b + e)
        }
        val paddedContent: Parser[Vector[String]] = "generateEscapeCharacterSameWithPadParser.paddedContent".!!!(pLeftPadChar ~! pBeforeAndEscs ~! pPadChar) ^^ { case (lp ~ c ~ rp) => Vector(lp, c, rp) }
        val contentCenter: Parser[(Vector[String], String)] = paddedContent ~! (pUnescapedDelims | EOF) ^^ { case (c ~ d) => (c, d) }
        "generateEscapeCharacterSameWithPadParser.contentCenter_NoDelims".!!!(contentCenter)
      }
      case _ => Assert.invariantFailed() //return failedResult
    }
    pFieldAndDelim
  }

  def generateInputEscapeCharacterParser(separators: Set[String], terminators: Set[String],
    escapeCharacter: String, escapeEscapeCharacter: String = "",
    justification: TextJustificationType.Type,
    padChar: String,
    isMissingDelimAllowed: Boolean = true): Parser[(Vector[String], String)] = {

    Assert.invariant(escapeCharacter.length() != 0)

    val hasEscEsc = escapeEscapeCharacter.length() > 0
    val hasDelim = separators.size > 0 || terminators.size > 0
    val escapeCharacterRegex = this.generateCharacterRegex(escapeCharacter)
    val escapeEscapeCharacterRegex = this.generateCharacterRegex(escapeEscapeCharacter)
    val padCharacterRegex = this.generateCharacterRegex(padChar)
    val (pSeps, sepsRegex) = this.generateSeparators(separators)
    val (pTerms, termsRegex) = this.generateTerminators(terminators)
    val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)

    if (escapeEscapeCharacter.equals(escapeCharacter)) {
      justification match {
        case TextJustificationType.None => return this.generateEscapeCharacterSameNoPadParser(pSeps, pTerms,
          escapeCharacterRegex, delimsRegex, hasDelim, isMissingDelimAllowed)
        case _ => return this.generateEscapeCharacterSameWithPadParser(pSeps, pTerms,
          escapeCharacterRegex, delimsRegex, justification, padChar, hasDelim, isMissingDelimAllowed)
      }
    } else {
      justification match {
        case TextJustificationType.None => return this.generateEscapeCharacterDiffNoPadParser(pSeps, pTerms,
          escapeCharacterRegex, escapeEscapeCharacterRegex, delimsRegex, hasEscEsc, hasDelim,
          isMissingDelimAllowed)
        case _ => return this.generateEscapeCharacterDiffWithPadParser(pSeps, pTerms,
          escapeCharacterRegex, escapeEscapeCharacterRegex, padCharacterRegex, delimsRegex, justification,
          hasEscEsc, hasDelim, isMissingDelimAllowed)
      }
    }
  }

  def generateInputEscapeCharacterParser2(delims: Parser[String], delimsRegex: String,
    hasDelim: Boolean,
    escapeCharacter: String, escapeEscapeCharacter: String = "",
    justification: TextJustificationType.Type,
    padChar: String,
    isMissingDelimAllowed: Boolean = true): Parser[(Vector[String], String)] = {

    Assert.invariant(escapeCharacter.length() != 0)

    val hasEscEsc = escapeEscapeCharacter.length() > 0
    val escapeCharacterRegex = this.generateCharacterRegex(escapeCharacter)
    val escapeEscapeCharacterRegex = this.generateCharacterRegex(escapeEscapeCharacter)
    val padCharacterRegex = this.generateCharacterRegex(padChar)

    if (escapeEscapeCharacter.equals(escapeCharacter)) {
      justification match {
        case TextJustificationType.None => return this.generateEscapeCharacterSameNoPadParser(this.emptyParser, delims,
          escapeCharacterRegex, delimsRegex, hasDelim, isMissingDelimAllowed)
        case _ => return this.generateEscapeCharacterSameWithPadParser(this.emptyParser, delims,
          escapeCharacterRegex, delimsRegex, justification, padChar, hasDelim, isMissingDelimAllowed)
      }
    } else {
      justification match {
        case TextJustificationType.None => return this.generateEscapeCharacterDiffNoPadParser(this.emptyParser, delims,
          escapeCharacterRegex, escapeEscapeCharacterRegex, delimsRegex, hasEscEsc, hasDelim,
          isMissingDelimAllowed)
        case _ => return this.generateEscapeCharacterDiffWithPadParser(this.emptyParser, delims,
          escapeCharacterRegex, escapeEscapeCharacterRegex, padCharacterRegex, delimsRegex, justification,
          hasEscEsc, hasDelim, isMissingDelimAllowed)
      }
    }
  }

  def generateRemoveEscapeCharactersSameRegex(es: String): scala.util.matching.Regex = {
    //val ERSplit = """(?s)(.*?)%1$s(.)(.*)""".format(es).r
    val ERSplit = ("""(?s)(.*?)""" + es + """(.)(.*)""").r
    ERSplit
  }
  def removeUnescapedEscapesRegex(eses: String, es: String): String = {
    // ((?<!S)E)
    //    val rRemoveUnescapedEscapes = """((?<!%1$s)%2$s)"""
    //    val removeUnescapedEscapes = rRemoveUnescapedEscapes.format(eses, es)
    val removeUnescapedEscapes = """((?<!""" + eses + """)""" + es + """)"""
    removeUnescapedEscapes
  }
  def removeEscapeEscapesThatEscapeRegex(eses: String, es: String): String = {
    // (S(?=E))
    //    val rRemoveEscapeEscapesThatEscape = """(%1$s(?=%2$s))"""
    //    val removeEscapeEscapesThatEscape = rRemoveEscapeEscapesThatEscape.format(eses, es)
    val removeEscapeEscapesThatEscape = """(""" + eses + """(?=""" + es + """))"""
    removeEscapeEscapesThatEscape
  }
  def removeEscapeRegex(es: String): String = {
    //val rRemoveEscape = """(%1$s)""".format(es)
    val rRemoveEscape = """(""" + es + """)"""
    rRemoveEscape
  }
  def removeEscapesBlocksRegex(eses: String, endBlockRegex: String): String = {
    // Replace escape at end of line OR replace escapeEscape preceding escapeBlockEnd
    //    val rRemoveEscape = """(%1$s$)|(%1$s(?=%2$s))"""
    //    val removeEscapes = rRemoveEscape.format(eses, endBlockRegex)
    val removeEscapes = """(""" + eses + """$)|(""" + eses + """(?=""" + endBlockRegex + """))"""
    removeEscapes
  }

  protected def parseInputDefaultContent(fieldParser: Parser[(Vector[String], String)], seps: Parser[String], terms: Parser[String],
    input: Reader[Char], justification: TextJustificationType.Type): DelimParseResult = {
    //     withLoggingLevel(LogLevel.Debug)
    {
      val res = parseInputCommon(fieldParser, seps, terms, input,
        "DelimParser.parseInputDefaultContent",
        DelimiterLocation.Local) {
          content =>
            justification match {
              case TextJustificationType.None => {
                // content == Vector(content)
                val field = content(0)
                (field, content.mkString)
              }
              case TextJustificationType.Left => {
                // content == Vector(content, padChars)
                val field = content(0)
                (field, content.mkString)
              }
              case TextJustificationType.Right => {
                // content == Vector(padChars, content)
                val field = content(1)
                (field, content.mkString)
              }
              case TextJustificationType.Center => {
                // content == Vector(padChars, content, padChars)
                val field = content(1)
                (field, content.mkString)
              }
            }
        }
      res
    }
  }

  /**
   * Notice this is curried. It takes a set of parameters, and then a body function
   * which converts the results of a scala combinator parser (which our combinators
   * use a Vector[String] for), and classifies it into two strings. One is the value region,
   * the other the content region.
   */
  protected def parseInputCommon(
    fieldParser: Parser[(Vector[String], String)],
    seps: Parser[String],
    terms: Parser[String],
    input: Reader[Char],
    logString: String,
    dLoc: DelimiterLocation.Type)(
      body: Vector[String] => (String, String)): DelimParseResult = {
    //val pResult = this.parse(this.log(fieldParser)(logString), input)
    val pResult = getLoggingLevel match {
      case LogLevel.Debug => this.parse(this.log(fieldParser)(logString), input)
      case _ => this.parse(fieldParser, input)
    }

    val result = pResult match {
      case Success((blockedContent, theDelim), next) => {
        val (theField, theParsedContent) = body(blockedContent)
        val dResult = this.parse(seps, theDelim) // does our delimiter match the possible seps?
        val dType =
          if (dResult.isEmpty) DelimiterType.Terminator
          else DelimiterType.Separator
        DelimParseSuccessFactory(Success(theField, pResult.next), theDelim, dType, Some(theParsedContent),
          dLoc)
      }
      case NoSuccess(msg, next) => {
        DelimParseFailure(msg, next)
      }
    }
    result
  }

  protected def parseInputEscapeBlockContent(
    fieldParser: Parser[(Vector[String], String)], seps: Parser[String], terms: Parser[String],
    input: Reader[Char], justification: TextJustificationType.Type): DelimParseResult = {
    // withLoggingLevel(LogLevel.Debug) 
    val res = parseInputCommon(fieldParser, seps, terms, input, "DelimParser.parseInputEscapeBlockContent",
      DelimiterLocation.Local) {
        blockedContent =>
          justification match {
            case TextJustificationType.None => {
              // blockedContent == Vector(blockStart, content, blockEnd)
              val field = blockedContent(1)
              (field, blockedContent.mkString)
            }
            case TextJustificationType.Left => {
              // blockedContent == Vector(blockStart, content, padChars, blockEnd)
              val field = blockedContent(1)
              (field, blockedContent.mkString)
            }
            case TextJustificationType.Right => {
              // blockedContent == Vector(blockStart, padChars, content, blockEnd)
              val field = blockedContent(2)
              (field, blockedContent.mkString)
            }
            case TextJustificationType.Center => {
              // blockedContent == Vector(blockStart, padChars, content, padChars, blockEnd)
              val field = blockedContent(2)
              (field, blockedContent.mkString)
            }
          }
      }
    res
  }

  protected def parseInputEscapeCharContent(fieldParser: Parser[(Vector[String], String)], seps: Parser[String], terms: Parser[String],
    input: Reader[Char], justification: TextJustificationType.Type): DelimParseResult = {
    parseInputDefaultContent(fieldParser, seps, terms, input, justification)
  }
}
