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

import scala.annotation.migration
import scala.collection.mutable.Queue
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Reader
import edu.illinois.ncsa.daffodil.processors.DelimiterType._
import edu.illinois.ncsa.daffodil.processors.DelimiterLocation.DelimiterLocation
import scala.util.matching.Regex
import java.nio.charset.Charset
import java.util.regex.Pattern
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Debug
import edu.illinois.ncsa.daffodil.dsom.AnnotatedSchemaComponent
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Info

object TextJustificationType extends Enumeration {
  type Type = Value
  val None, Left, Right, Center = Value
}

sealed abstract class DelimParseResult(nextArg: Reader[Char]) {
  def isSuccess: Boolean
  def next = nextReader.asInstanceOf[DFDLCharReader]
  def nextReader = nextArg
}

case class DelimParseSuccess(val delimiter: String,
                             val delimiterType: DelimiterType,
                             val delimiterLoc: DelimiterLocation,
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

class DelimParser(stringBitLengthFunction: String => Int) extends RegexParsers with Logging {

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
    def apply(res: Success[String], delimiter: String, delimiterType: DelimiterType, contentOpt: Option[String],
              dLoc: DelimiterLocation) = {

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
   * A helper method that turns a `Parser` into one that will
   *  print debugging information to stdout before and after
   *  being applied.
   */
  override def log[T](p: => Parser[T])(name: String): Parser[T] = Parser { in =>
    log(Debug("trying %s at %s", name, in))
    val r = p(in)
    log(Debug("%s --> %s", name, r))
    r
  }

  // Need a parser that will always fail.
  // Essentially a parser to reflect the ability to pass in an empty Separator
  // or empty Terminator Set.
  //
  def parserAlwaysFail[T](expected: String)(name: String) = new Parser[T] {
    def apply(in: Input): ParseResult[T] = {
      Failure(name + " expected to fail.", in)
    }
  }

  // This function performs the "longest match" alternation by recursively combining
  // all of the Parsers present in pArray.
  //
  // Assumes pArray is at least of length 1.
  //
  // pArray: An array of delimiters where each delimiter is represented by a Parser
  // idx: The index of the current delimiter/Parser, defaults to 0
  //
  def combineLongest[T](pArray: Array[Parser[T]], idx: Int = 0): Parser[T] = {
    val len = pArray.length

    if (len == 0) { return parserAlwaysFail("")("empty delimiter list") }

    val p0 = pArray(idx)

    if ((idx + 1) == len) { p0 }
    else {
      (p0) ||| (combineLongest(pArray, idx + 1))
    }
  }

  // Constructs an Array of Parser[String] which holds the Parser representations
  // of the delimList.
  //
  // Constructs an Array of String which holds the Regex representations of the
  // delimList.
  //
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

  // Combines the delimiters into a single alternation
  //
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
    delimRegex
  }

  def removePadding(input: String, justification: TextJustificationType.Type, padChar: String): String = {
    if ((padChar.length() == 0)) { return input }
    val rAnything = """(.*?)(?=%s*$)""".format(padChar)
    val anything: Parser[String] = rAnything.r
    val padCharRegex = this.convertDFDLLiteralToRegex(padChar)
    val rPadCharLeft = """^(%s*)""".format(padCharRegex)
    val pPadCharLeft: Parser[String] = rPadCharLeft.r
    val rPadCharRight = """(%s*)$""".format(padCharRegex)
    val pPadCharRight: Parser[String] = rPadCharRight.r
    val fieldCenter: Parser[String] = pPadCharLeft ~ anything ~ pPadCharRight ^^ { case (l ~ a ~ r) => a }
    val fieldLeft: Parser[String] = anything ~ pPadCharRight ^^ { case (a ~ r) => a }
    val fieldRight: Parser[String] = pPadCharLeft ~ anything ^^ { case (l ~ a) => a }
    // Remove padding if it exists
    val result = justification match {
      case TextJustificationType.None => input
      case TextJustificationType.Left => {
        val res = this.parse(this.log(fieldLeft)("DelimParser.removePadding.leftJustified"), input)
        res.getOrElse(input)
      }
      case TextJustificationType.Right => {
        val res = this.parse(this.log(fieldRight)("DelimParser.removePadding.rightJustified"), input)
        res.getOrElse(input)
      }
      case TextJustificationType.Center => {
        val res = this.parse(this.log(fieldCenter)("DelimParser.removePadding.centerJustified"), input)
        res.getOrElse(input)
      }
    }
    result
  }

  def parseInputPatterned(pattern: String, input: Reader[Char]): DelimParseResult = {
    val EOF: Parser[String] = """\z""".r

    val thePattern: Parser[String] = ("(?s)" + pattern).r
    val entry = thePattern <~ opt(EOF)

    // FOR DEBUGGING might want this logging version
    val res = this.parse(this.log(entry)("DelimParser.parseInputPatterned"), input)
    //val res = this.parse(entry, input)

    res match {
      case s @ Success(_, _) => DelimParseSuccessFactory(s, "", DelimiterType.NotDelimited, None, DelimiterLocation.Local)
      case f: NoSuccess => DelimParseFailure(f.msg, f.next)
    }
  }

  def parseInputNCharacters(nChars: Long, input: Reader[Char],
                            justification: TextJustificationType.Type,
                            padChar: String): DelimParseResult = {

    val EOF: Parser[String] = """\z""".r
    val anything: Parser[String] = """.*""".r
    val firstNChars: Parser[String] = String.format("""(?s).{%s}""", nChars.toString()).r

    val entry = firstNChars //<~ anything // Technically shouldn't need to add anything, we only want the first nChars

    // For debug can use this logging parser instead.
    val res = this.parse(this.log(entry)("DelimParser.parseInputNCharacters"), input)
    //val res = this.parse(entry, input)

    res match {
      case s @ Success(field, next) => {
        val fieldNoPadding = removePadding(field, justification, padChar)
        DelimParseSuccessFactory(Success(fieldNoPadding, next), "", DelimiterType.NotDelimited, Some(field), DelimiterLocation.Local)
      }
      case f: NoSuccess => DelimParseFailure(f.msg, f.next)
    }
  }

  private def parseInputDefaultContent(fieldParser: Parser[(Vector[String], String)], seps: Parser[String], terms: Parser[String],
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

  private def parseInputEscapeCharContent(fieldParser: Parser[(Vector[String], String)], seps: Parser[String], terms: Parser[String],
                                          input: Reader[Char], justification: TextJustificationType.Type): DelimParseResult = {
    parseInputDefaultContent(fieldParser, seps, terms, input, justification)

  }

  /**
   * Notice this is curried. It takes a set of parameters, and then a body function
   * which converts the results of a scala combinator parser (which our combinators
   * use a Vector[String] for), and classifies it into two strings. One is the value region,
   * the other the content region.
   */
  private def parseInputCommon(
    fieldParser: Parser[(Vector[String], String)],
    seps: Parser[String],
    terms: Parser[String],
    input: Reader[Char],
    logString: String,
    dLoc: DelimiterLocation)(
      body: Vector[String] => (String, String)): DelimParseResult = {
    val pResult = this.parse(this.log(fieldParser)(logString), input)

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

  private def parseInputEscapeBlockContent(
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

  /**
   * localDelims - delimiters local to the component in question
   * remoteDelims - delimiters of an enclosing container of this component
   *
   * Assumes that remoteDelims does not contain any String found in localDelims
   *
   * The call to buildDelims sorts the delimiters by length or possible length.
   */
  def parseInputDelimiter(localDelims: Set[String], remoteDelims: Set[String],
                          input: Reader[Char]): DelimParseResult = {
    val (localDelimsParser, localDelimsRegex) = this.buildDelims(localDelims)
    val combinedLocalDelimsParser = this.combineLongest(localDelimsParser)

    val (remoteDelimsParser, remoteDelimsRegex) = this.buildDelims(remoteDelims)

    val combinedDelims = remoteDelimsParser ++ localDelimsParser
    val combinedDelimsParser = this.combineLongest(combinedDelims)

    val EOF: Parser[String] = """\z""".r

    //val entry = combinedLocalDelimsParser <~ opt(EOF)
    val entry = combinedDelimsParser <~ opt(EOF) // Should yield longest match of all the delimiters

    val res = this.parse(this.log(entry)("DelimParser.parseInputDelimiter.allDelims"), input)

    // TODO: This seems pretty inefficient. We're redoing a match in order to know 
    // whether it was local or remote?? 
    val result = res match {
      case s @ Success(delimiterResult, next) => {
        // We have a result but was it a remote or local match?
        // We need the regex to match exactly the whole delimiterResult
        // Here localDelimsRegex should have already been sorted by the buildDelims call
        // we simply need to tell the regex that it has to match the full delimiterResult String.
        // If it doesn't match, then that means the match had to be a remote delimiter.
        val newLocalDelimsRegex = "(?s)^(" + combineDelimitersRegex(localDelimsRegex, Array.empty[String]) + ")$"
        val newLocalDelimsParser: Parser[String] = newLocalDelimsRegex.r

        val subResult = this.parseAll(this.log(newLocalDelimsParser)("DelimParser.parseInputDelimiter.isLocal"), delimiterResult)
        val delimiterLocation = if (subResult.isEmpty) DelimiterLocation.Remote else DelimiterLocation.Local
        //
        // TODO: ?? Is None the right thing to pass here?? If we pass none, then it is 
        // going to determine the length based on the delimiterResult. Does that include
        // everything it needs to include?
        //
        DelimParseSuccessFactory(s, delimiterResult, DelimiterType.NotDelimited, None, // Is None right?
          delimiterLocation)
      }
      case NoSuccess(msg, next) => DelimParseFailure(msg, next)
    }
    result
  }

  /**
   * Parses text input without escape schemes
   */
  def parseInput(separators: Set[String], terminators: Set[String], input: Reader[Char],
                 justification: TextJustificationType.Type,
                 padChar: String,
                 isMissingDelimAllowed: Boolean = true): DelimParseResult = {
    justification match {
      case TextJustificationType.None => return parseInput_NoPad(separators, terminators, input, isMissingDelimAllowed)
      case _ => return parseInput_WithPad(separators, terminators, input, justification, padChar, isMissingDelimAllowed)
    }
  }

  private def parseInput_WithPad(separators: Set[String], terminators: Set[String], input: Reader[Char],
                                 justification: TextJustificationType.Type,
                                 padChar: String,
                                 isMissingDelimAllowed: Boolean = true): DelimParseResult = {
    // TODO: Move regular expressions out to central class
    val (sepsParser, sepsRegex) = this.buildDelims(separators)
    val (termsParser, termsRegex) = this.buildDelims(terminators)
    val pSeps: Parser[String] = this.combineLongest(sepsParser)
    val pTerms: Parser[String] = this.combineLongest(termsParser)

    val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)
    val padCharRegex = convertDFDLLiteralToRegex(padChar)

    val rPadChar = """(%s*)"""
    val pPadChar: Parser[String] = rPadChar.format(padCharRegex).r
    val rBefore = """(?s)(.*?)(?=(%1$s+(%2$s))|(%1$s+\z)|(%2$s)|(\z))"""
    val pBefore: Parser[String] = rBefore.format(padCharRegex, delimsRegex).r
    val rBeforeNoDelims = """(?s)(.*?)(?=(%1$s+\z)|(\z))"""
    val pBeforeNoDelims: Parser[String] = rBeforeNoDelims.format(padCharRegex).r
    val rBeforeNoPadding = """(?s)((.*?)(?=(%1$s)|(\z)))|(.*)"""
    val pBeforeNoPadding: Parser[String] = rBeforeNoPadding.format(delimsRegex).r
    val rBeforeNoPaddingOrDelims = """(?s)(.*?)(?=(\z))"""
    val pBeforeNoPaddingOrDelims: Parser[String] = rBeforeNoPaddingOrDelims.r
    val pDelims: Parser[String] = pSeps ||| pTerms
    val pEOF: Parser[String] = """\z""".r

    val paddedContent = pPadChar ~ pBefore ~ pPadChar ^^ { case (lp ~ c ~ rp) => Vector(lp, c, rp) }
    val leftPaddedContent = pPadChar ~ pBeforeNoPadding ^^ { case (lp ~ c) => Vector(lp, c) }
    val rightPaddedContent = pBefore ~ pPadChar ^^ { case (c ~ rp) => Vector(c, rp) }

    val hasDelim: Boolean = separators.size > 0 || terminators.size > 0

    val pFieldAndDelim: Parser[(Vector[String], String)] = (justification, isMissingDelimAllowed, hasDelim) match {
      case (TextJustificationType.Left, false, true) => {
        val contentLeftDelimReq = rightPaddedContent ~ pDelims ^^ { case (c ~ d) => (c, d) }
        contentLeftDelimReq
      }
      case (TextJustificationType.Right, false, true) => {
        val contentRightDelimReq = leftPaddedContent ~ pDelims ^^ { case (c ~ d) => (c, d) }
        contentRightDelimReq
      }
      case (TextJustificationType.Center, false, true) => {
        val contentCenterDelimReq = paddedContent ~ pDelims ^^ { case (c ~ d) => (c, d) }
        contentCenterDelimReq
      }
      case (TextJustificationType.Left, true, true) => {
        val contentLeft = rightPaddedContent ~ (pDelims | pEOF) ^^ { case (c ~ d) => (c, d) }
        contentLeft
      }
      case (TextJustificationType.Left, true, false) => {
        val rightPaddedContent = pBeforeNoDelims ~ pPadChar ^^ { case (c ~ rp) => Vector(c, rp) }
        val contentLeft = rightPaddedContent ~ (pEOF) ^^ { case (c ~ d) => (c, d) }
        contentLeft
      }
      case (TextJustificationType.Right, true, true) => {
        val contentRight = leftPaddedContent ~ (pDelims | pEOF) ^^ { case (c ~ d) => (c, d) }
        contentRight
      }
      case (TextJustificationType.Right, true, false) => {
        val leftPaddedContent = pPadChar ~ pBeforeNoDelims ^^ { case (lp ~ c) => Vector(lp, c) }
        val contentRight = leftPaddedContent ~ (pEOF) ^^ { case (c ~ d) => (c, d) }
        contentRight
      }
      case (TextJustificationType.Center, true, true) => {
        val contentCenter = paddedContent ~ (pDelims | pEOF) ^^ { case (c ~ d) => (c, d) }
        contentCenter
      }
      case (TextJustificationType.Center, true, false) => {
        val paddedContent = pPadChar ~ pBeforeNoDelims ~ pPadChar ^^ { case (lp ~ c ~ rp) => Vector(lp, c, rp) }
        val contentCenter = paddedContent ~ (pEOF) ^^ { case (c ~ d) => (c, d) }
        contentCenter
      }
      case _ => Assert.invariantFailed("not one of the combinations.") // return failedResult
    }

    val result = parseInputDefaultContent(pFieldAndDelim, pSeps, pTerms, input, justification)
    result
  }

  private def parseInput_NoPad(separators: Set[String], terminators: Set[String], input: Reader[Char],
                               isMissingDelimAllowed: Boolean = true): DelimParseResult = {
    // TODO: Move regular expressions out to central class
    val (sepsParser, sepsRegex) = this.buildDelims(separators)
    val (termsParser, termsRegex) = this.buildDelims(terminators)
    val pSeps: Parser[String] = this.combineLongest(sepsParser)
    val pTerms: Parser[String] = this.combineLongest(termsParser)

    val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)

    val rBeforeNoPadding = """(?s)((.*?)(?=(%1$s)|(\z)))|(.*)"""
    val pBeforeNoPadding: Parser[String] = rBeforeNoPadding.format(delimsRegex).r
    val rBeforeNoPaddingOrDelims = """(?s)(.*?)(?=(\z))"""
    val pBeforeNoPaddingOrDelims: Parser[String] = rBeforeNoPaddingOrDelims.r
    val pDelims: Parser[String] = pSeps ||| pTerms
    val pEOF: Parser[String] = """\z""".r

    val hasDelim: Boolean = separators.size > 0 || terminators.size > 0

    val pFieldAndDelim: Parser[(Vector[String], String)] = (isMissingDelimAllowed, hasDelim) match {
      case (false, true) => {
        val contentDelimReq = pBeforeNoPadding ~ pDelims ^^ { case (c ~ d) => (Vector(c), d) }
        contentDelimReq
      }
      case (true, true) => {
        val content = pBeforeNoPadding ~ (pDelims | pEOF) ^^ { case (c ~ d) => (Vector(c), d) }
        content
      }
      case (true, false) => {
        val content = pBeforeNoPaddingOrDelims ~ (pEOF) ^^ { case (c ~ d) => (Vector(c), d) }
        content
      }
      case _ => Assert.invariantFailed("impossible combination") // return failedResult
    }

    val result = parseInputDefaultContent(pFieldAndDelim, pSeps, pTerms, input, TextJustificationType.None)
    result
  }

  /**
   * Parses input that can have escape blocks
   */
  def parseInputEscapeBlock(separators: Set[String], terminators: Set[String],
                            input: Reader[Char], escapeBlockStart: String, escapeBlockEnd: String,
                            escapeEscapeCharacter: String = "",
                            justification: TextJustificationType.Type,
                            padChar: String,
                            isMissingDelimAllowed: Boolean = true): DelimParseResult = {

    justification match {
      case TextJustificationType.None => return parseInputEscapeBlock_NoPad(separators, terminators,
        input, escapeBlockStart, escapeBlockEnd, escapeEscapeCharacter, isMissingDelimAllowed)
      case _ => return parseInputEscapeBlock_WithPad(separators, terminators,
        input, escapeBlockStart, escapeBlockEnd, escapeEscapeCharacter, justification, padChar, isMissingDelimAllowed)
    }
  }

  private def parseInputEscapeBlock_WithPad(separators: Set[String], terminators: Set[String],
                                            input: Reader[Char], escapeBlockStart: String, escapeBlockEnd: String,
                                            escapeEscapeCharacter: String = "",
                                            justification: TextJustificationType.Type,
                                            padChar: String,
                                            isMissingDelimAllowed: Boolean = true): DelimParseResult = {

    //TODO: Move regular expressions out into central class
    //    if (escapeBlockStart.length() == 0 || escapeBlockEnd.length() == 0 || padChar.length() == 0) { 
    //      return failedResult }
    Assert.invariant(escapeBlockStart.length() != 0 && escapeBlockEnd.length() != 0 && padChar.length() != 0)
    val escapeBlockStartRegex = convertDFDLLiteralToRegex(escapeBlockStart)
    val escapeBlockEndRegex = convertDFDLLiteralToRegex(escapeBlockEnd)
    val escapeEscapeRegex = convertDFDLLiteralToRegex(escapeEscapeCharacter)
    val padCharRegex = convertDFDLLiteralToRegex(padChar)

    val pEscape: Parser[String] = escapeEscapeRegex.r
    val pBlockStart: Parser[String] = escapeBlockStartRegex.r
    val pBlockEnd: Parser[String] = escapeBlockEndRegex.r
    val pEOF: Parser[String] = """\z""".r

    val (sepsParser, sepsRegex) = this.buildDelims(separators)
    val (termsParser, termsRegex) = this.buildDelims(terminators)
    val pSeps: Parser[String] = this.combineLongest(sepsParser)
    val pTerms: Parser[String] = this.combineLongest(termsParser)
    val pDelims: Parser[String] = pSeps ||| pTerms

    val hasEscEsc: Boolean = escapeEscapeCharacter.length() > 0

    val rUnescapedBlockStart = if (hasEscEsc) """(?<!%1$s)%2$s""" else """%2$s"""
    val pUnescapedBlockStart: Parser[String] = rUnescapedBlockStart.format(escapeEscapeRegex, escapeBlockStartRegex).r
    val rUnescapedBlockEnd = if (hasEscEsc) """(?<!%1$s)%2$s""" else """%2$s"""
    val pUnescapedBlockEnd: Parser[String] = rUnescapedBlockEnd.format(escapeEscapeRegex, escapeBlockEndRegex).r
    val rBeforeUnescapedBlockEnd = """(?s)(.*?)(?=(""" + rUnescapedBlockEnd + """))"""
    val pBeforeUnescapedBlockEnd: Parser[String] = rBeforeUnescapedBlockEnd.format(escapeEscapeRegex, escapeBlockEndRegex).r
    val blockedContent: Parser[Vector[String]] = pUnescapedBlockStart ~ pBeforeUnescapedBlockEnd ~ pUnescapedBlockEnd ^^ { case (bs ~ c ~ be) => Vector(bs, c, be) }

    val rPadChar = """(%s*)"""
    val pPadChar: Parser[String] = rPadChar.format(padCharRegex).r
    val paddedBlockedContent: Parser[Vector[String]] = pPadChar ~ blockedContent ~ pPadChar ^^ { case (lp ~ bc ~ rp) => Vector(lp) ++ bc ++ Vector(rp) }
    val leftPaddedBlockedContent: Parser[Vector[String]] = pPadChar ~ blockedContent ^^ { case (lp ~ bc) => Vector(lp) ++ bc }
    val rightPaddedBlockedContent: Parser[Vector[String]] = blockedContent ~ pPadChar ^^ { case (bc ~ rp) => bc ++ Vector(rp) }

    val pFieldAndDelim: Parser[(Vector[String], String)] = (justification -> isMissingDelimAllowed) match {
      case (TextJustificationType.Left, false) => {
        val contentLeftDelimReq: Parser[(Vector[String], String)] = rightPaddedBlockedContent ~ pDelims ^^ { case (bc ~ d) => (bc -> d) }
        contentLeftDelimReq
      }
      case (TextJustificationType.Right, false) => {
        val contentRightDelimReq: Parser[(Vector[String], String)] = leftPaddedBlockedContent ~ pDelims ^^ { case (bc ~ d) => (bc -> d) }
        contentRightDelimReq
      }
      case (TextJustificationType.Center, false) => {
        val contentCenterDelimReq: Parser[(Vector[String], String)] = paddedBlockedContent ~ pDelims ^^ { case (bc ~ d) => (bc -> d) }
        contentCenterDelimReq
      }
      case (TextJustificationType.Left, true) => {
        val contentLeft: Parser[(Vector[String], String)] = rightPaddedBlockedContent ~ (pDelims | pEOF) ^^ { case (bc ~ d) => (bc -> d) }
        contentLeft
      }
      case (TextJustificationType.Right, true) => {
        val contentRight: Parser[(Vector[String], String)] = leftPaddedBlockedContent ~ (pDelims | pEOF) ^^ { case (bc ~ d) => (bc -> d) }
        contentRight
      }
      case (TextJustificationType.Center, true) => {
        val contentCenter: Parser[(Vector[String], String)] = paddedBlockedContent ~ (pDelims | pEOF) ^^ { case (bc ~ d) => (bc -> d) }
        contentCenter
      }
    }

    val result1 = parseInputEscapeBlockContent(pFieldAndDelim, pSeps, pTerms, input, justification)

    // If failed, try regular parse.
    val result = result1 match {
      case s: DelimParseSuccess => {
        val field = s.get
        val newField = removeEscapesBlocks(field, escapeEscapeRegex, escapeBlockEndRegex)
        s.copy(fieldArg = newField, nextArg = s.next)
        //        DelimParseSuccessFactory(Success(newField, s.next), s.delimiter, s.delimiterType, Some(field),
        //          s.delimiterLoc)

      }
      case f: DelimParseFailure => parseInput(separators, terminators, input, justification, padChar, isMissingDelimAllowed)
    }
    result
  }

  private def parseInputEscapeBlock_NoPad(separators: Set[String], terminators: Set[String],
                                          input: Reader[Char], escapeBlockStart: String, escapeBlockEnd: String,
                                          escapeEscapeCharacter: String = "",
                                          isMissingDelimAllowed: Boolean = true): DelimParseResult = {
    //TODO: Move regular expressions out into central class
    //if (escapeBlockStart.length() == 0 || escapeBlockEnd.length() == 0) { return failedResult }
    Assert.invariant(escapeBlockStart.length() != 0 && escapeBlockEnd.length() != 0)

    val escapeBlockStartRegex = convertDFDLLiteralToRegex(escapeBlockStart)
    val escapeBlockEndRegex = convertDFDLLiteralToRegex(escapeBlockEnd)
    val escapeEscapeRegex = convertDFDLLiteralToRegex(escapeEscapeCharacter)

    val pEscape: Parser[String] = escapeEscapeRegex.r
    val pBlockStart: Parser[String] = escapeBlockStartRegex.r
    val pBlockEnd: Parser[String] = escapeBlockEndRegex.r
    val pEOF: Parser[String] = """\z""".r

    val (sepsParser, sepsRegex) = this.buildDelims(separators)
    val (termsParser, termsRegex) = this.buildDelims(terminators)
    val pSeps: Parser[String] = this.combineLongest(sepsParser)
    val pTerms: Parser[String] = this.combineLongest(termsParser)
    val pDelims: Parser[String] = pSeps ||| pTerms

    val hasEscEsc: Boolean = escapeEscapeCharacter.length() > 0

    val rUnescapedBlockStart = if (hasEscEsc) """(?<!%1$s)%2$s""" else """%2$s"""
    val pUnescapedBlockStart: Parser[String] = rUnescapedBlockStart.format(escapeEscapeRegex, escapeBlockStartRegex).r
    val rUnescapedBlockEnd = if (hasEscEsc) """(?<!%1$s)%2$s""" else """%2$s"""
    val pUnescapedBlockEnd: Parser[String] = rUnescapedBlockEnd.format(escapeEscapeRegex, escapeBlockEndRegex).r
    val rBeforeUnescapedBlockEnd = """(?s)(.*?)(?=(""" + rUnescapedBlockEnd + """))"""
    val pBeforeUnescapedBlockEnd: Parser[String] = rBeforeUnescapedBlockEnd.format(escapeEscapeRegex, escapeBlockEndRegex).r
    val blockedContent: Parser[Vector[String]] = pUnescapedBlockStart ~ pBeforeUnescapedBlockEnd ~ pUnescapedBlockEnd ^^ { case (bs ~ c ~ be) => Vector(bs, c, be) }

    val pFieldAndDelim: Parser[(Vector[String], String)] = isMissingDelimAllowed match {
      case false => {
        val contentDelimReq: Parser[(Vector[String], String)] = blockedContent ~ pDelims ^^ { case (bc ~ d) => (bc -> d) }
        contentDelimReq
      }
      case true => {
        val content: Parser[(Vector[String], String)] = blockedContent ~ (pDelims | pEOF) ^^ { case (bc ~ d) => (bc -> d) }
        content
      }
    }

    val result1 = parseInputEscapeBlockContent(pFieldAndDelim, pSeps, pTerms, input, TextJustificationType.None)

    val result = result1 match {
      case s: DelimParseSuccess => {
        val field = s.get // we move past this, though the resulting string will have escapes dropped.
        val newField = removeEscapesBlocks(field, escapeEscapeRegex, escapeBlockEndRegex)
        s.copy(fieldArg = newField, nextArg = s.next)
      }
      // If failed, try regular parse.
      case _: DelimParseFailure => parseInput(separators, terminators, input, TextJustificationType.None, "", isMissingDelimAllowed)
    }
    result
  }

  /**
   * Parses input that can have character escape schemes
   */
  def parseInputEscapeCharacter(separators: Set[String], terminators: Set[String],
                                input: Reader[Char], escapeCharacter: String, escapeEscapeCharacter: String = "",
                                justification: TextJustificationType.Type,
                                padChar: String,
                                isMissingDelimAllowed: Boolean = true): DelimParseResult = {

    //if (terminators.size == 0 && separators.size == 0) { return failedResult }
    // if (escapeCharacter.length() == 0) { return failedResult }
    Assert.invariant(escapeCharacter.length() != 0)

    if (escapeEscapeCharacter.equals(escapeCharacter)) {
      justification match {
        case TextJustificationType.None => return parseInputEscapeCharacter_SameNoPad(separators, terminators, input,
          escapeCharacter, isMissingDelimAllowed)
        case _ => return parseInputEscapeCharacter_SameWithPad(separators, terminators, input,
          escapeCharacter, justification, padChar, isMissingDelimAllowed)
      }
    } else {
      justification match {
        case TextJustificationType.None => return parseInputEscapeCharacter_DiffNoPad(separators, terminators, input,
          escapeCharacter, escapeEscapeCharacter, isMissingDelimAllowed)
        case _ => return parseInputEscapeCharacter_DiffWithPad(separators, terminators, input,
          escapeCharacter, escapeEscapeCharacter, justification, padChar, isMissingDelimAllowed)
      }
    }
  }

  /*
   * Parses input for escapeSchemeKind='character' when the escape and escapeEscape
   * characters are not the same.
   */
  private def parseInputEscapeCharacter_DiffWithPad(separators: Set[String], terminators: Set[String],
                                                    input: Reader[Char], escapeCharacter: String, escapeEscapeCharacter: String = "",
                                                    justification: TextJustificationType.Type,
                                                    padChar: String,
                                                    isMissingDelimAllowed: Boolean = true): DelimParseResult = {

    // if (escapeCharacter.length() == 0 || padChar.length() == 0) { return failedResult }
    Assert.invariant(escapeCharacter.length() != 0 && padChar.length() != 0)

    val (sepsParser, sepsRegex) = this.buildDelims(separators)
    val (termsParser, termsRegex) = this.buildDelims(terminators)
    val pSeps: Parser[String] = this.combineLongest(sepsParser)
    val pTerms: Parser[String] = this.combineLongest(termsParser)
    val pDelims: Parser[String] = pSeps ||| pTerms
    val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)

    val escapeEscapeCharacterRegex = convertDFDLLiteralToRegex(escapeEscapeCharacter)
    val escapeCharacterRegex = convertDFDLLiteralToRegex(escapeCharacter)
    val padCharRegex = convertDFDLLiteralToRegex(padChar)

    val pEscape: Parser[String] = escapeCharacterRegex.r
    val pEscapeEscape: Parser[String] = escapeEscapeCharacterRegex.r
    val pEscapedEscape = pEscapeEscape ~ pEscape
    val pUnescapedDelims = ((pEscapeEscape ~ pEscape) ~> pDelims) | (not(pEscape) ~> pDelims) | (pEscapeEscape ~> pDelims) | pDelims

    val rPadChar = """(%s*)"""
    val pPadChar: Parser[String] = rPadChar.format(padCharRegex).r
    val rLeftPadChar = """(%1$s*)(?=([^%1$s]))""" // LeftPad precedes non pad characters
    val pLeftPadChar: Parser[String] = rLeftPadChar.format(padCharRegex).r
    val pEOF: Parser[String] = """\z""".r

    val hasEscEsc: Boolean = escapeEscapeCharacter.length() > 0

    // Content is anything until:
    // 1. Padding ~ delimiter
    // 2. unescaped delimiter
    // 3. Optional Padding ~ EndOfData
    val rBefore = """(?s)(.*?)(?=(%4$s+%3$s)|((?<!(?<!%1$s)%2$s)%3$s)|(%4$s*\z))"""
    val rBeforeNoEscEsc = """(?s)(.*?)(?=(%3$s+%2$s)|((?<!%1$s)%2$s)|(%3$s*\z))"""
    val pBefore: Parser[String] = {
      if (hasEscEsc) rBefore.format(escapeEscapeCharacterRegex, escapeCharacterRegex, delimsRegex, padCharRegex).r
      else rBeforeNoEscEsc.format(escapeCharacterRegex, delimsRegex, padCharRegex).r
    }

    val rBeforeIgnoreTrailingPadding = """(?s)(.*?)(?=(?:(?<!(?<!%1$s)%2$s)%3$s)|\z)"""
    val rBeforeIgnoreTrailingPaddingNoEscEsc = """(?s)(.*?)(?=(?:(?<!%1$s)%2$s)|\z)"""
    val pBeforeIgnoreTrailingPadding: Parser[String] = {
      if (hasEscEsc) rBeforeIgnoreTrailingPadding.format(escapeEscapeCharacterRegex, escapeCharacterRegex, delimsRegex).r
      else rBeforeIgnoreTrailingPaddingNoEscEsc.format(escapeCharacterRegex, delimsRegex).r
    }

    val rBeforeNoPadding = """(?s)(.*?)(?=((?<!(?<!%1$s)%2$s)%3$s)|(\z))"""
    val rBeforeNoPaddingNoEscEsc = """(?s)(.*?)(?=((?<!%1$s)%2$s)|(\z))"""
    val pBeforeNoPadding: Parser[String] = {
      if (hasEscEsc) rBeforeNoPadding.format(escapeEscapeCharacterRegex, escapeCharacterRegex, delimsRegex).r
      else rBeforeNoPaddingNoEscEsc.format(escapeCharacterRegex, delimsRegex).r
    }

    val rBeforeNoPaddingOrDelims = """(?s)(.*?)(?=(\z))"""
    val pBeforeNoPaddingOrDelims: Parser[String] = rBeforeNoPaddingOrDelims.r

    val rBeforeNoDelims = """(?s)(.*?)(?=(%1$s*\z))"""
    val pBeforeNoDelims: Parser[String] = rBeforeNoDelims.format(padCharRegex).r

    val paddedContent: Parser[Vector[String]] = pLeftPadChar ~ pBefore ~ pPadChar ^^ { case (lp ~ c ~ rp) => Vector(lp, c, rp) }
    val leftPaddedContent: Parser[Vector[String]] = pLeftPadChar ~ pBefore ^^ { case (lp ~ c) => Vector(lp, c) }
    val rightPaddedContent: Parser[Vector[String]] = pBefore ~ pPadChar ^^ { case (c ~ rp) => Vector(c, rp) }

    val hasDelim: Boolean = separators.size > 0 || terminators.size > 0

    val pFieldAndDelim: Parser[(Vector[String], String)] = (justification, isMissingDelimAllowed, hasDelim) match {
      case (TextJustificationType.Left, false, true) => {
        val contentLeftDelimReq: Parser[(Vector[String], String)] = rightPaddedContent ~ pUnescapedDelims ^^ { case (c ~ d) => (c, d) }
        contentLeftDelimReq
      }
      case (TextJustificationType.Right, false, true) => {
        val contentRightDelimReq: Parser[(Vector[String], String)] = leftPaddedContent ~ pUnescapedDelims ^^ { case (c ~ d) => (c, d) }
        contentRightDelimReq
      }
      case (TextJustificationType.Center, false, true) => {
        val contentCenterDelimReq: Parser[(Vector[String], String)] = paddedContent ~ pDelims ^^ { case (bc ~ d) => (bc -> d) }
        contentCenterDelimReq
      }
      case (TextJustificationType.Left, true, true) => {
        val contentLeft: Parser[(Vector[String], String)] = rightPaddedContent ~ (pUnescapedDelims | pEOF) ^^ { case (c ~ d) => (c, d) }
        contentLeft
      }
      case (TextJustificationType.Left, true, false) => {
        val rightPaddedContent: Parser[Vector[String]] = pBeforeNoDelims ~ pPadChar ^^ { case (c ~ rp) => Vector(c, rp) }
        val contentLeft: Parser[(Vector[String], String)] = rightPaddedContent ~ (pEOF) ^^ { case (c ~ d) => (c, d) }
        contentLeft
      }
      case (TextJustificationType.Right, true, true) => {
        val contentRight: Parser[(Vector[String], String)] = leftPaddedContent ~ (pUnescapedDelims | pEOF) ^^ { case (c ~ d) => (c, d) }
        contentRight
      }
      case (TextJustificationType.Right, true, false) => {
        val leftPaddedContent: Parser[Vector[String]] = pLeftPadChar ~ pBeforeNoDelims ^^ { case (lp ~ c) => Vector(lp, c) }
        val contentRight: Parser[(Vector[String], String)] = leftPaddedContent ~ (pEOF) ^^ { case (c ~ d) => (c, d) }
        contentRight
      }
      case (TextJustificationType.Center, true, true) => {
        val contentCenter: Parser[(Vector[String], String)] = paddedContent ~ (pUnescapedDelims | pEOF) ^^ { case (c ~ d) => (c, d) }
        contentCenter
      }
      case (TextJustificationType.Center, true, false) => {
        val paddedContent: Parser[Vector[String]] = pLeftPadChar ~ pBeforeNoDelims ~ pPadChar ^^ { case (lp ~ c ~ rp) => Vector(lp, c, rp) }
        val contentCenter: Parser[(Vector[String], String)] = paddedContent ~ (pEOF) ^^ { case (c ~ d) => (c, d) }
        contentCenter
      }
      case _ => Assert.invariantFailed("illegal combination") //return failedResult
    }

    val result1 = parseInputEscapeCharContent(pFieldAndDelim, pSeps, pTerms, input, justification)
    val result = result1 match {
      case s: DelimParseSuccess => {
        val field = s.get
        val newField = removeEscapeCharacters(field, escapeEscapeCharacterRegex, escapeCharacterRegex, delimsRegex)
        s.copy(fieldArg = newField, nextArg = s.next)
        //        DelimParseSuccessFactory(Success(newField, s.next), s.delimiter, s.delimiterType, Some(field),
        //          s.delimiterLoc)
      }
      case f: DelimParseFailure => f
    }
    result
  }

  /*
   * Parses input for escapeSchemeKind='character' when the escape and escapeEscape
   * characters are not the same.
   */
  private def parseInputEscapeCharacter_DiffNoPad(separators: Set[String], terminators: Set[String],
                                                  input: Reader[Char], escapeCharacter: String, escapeEscapeCharacter: String = "",
                                                  isMissingDelimAllowed: Boolean = true): DelimParseResult = {

    //if (terminators.size == 0 && separators.size == 0) { return failedResult }
    // if (escapeCharacter.length() == 0) { return failedResult }
    Assert.invariant(escapeCharacter.length() > 0)

    val (sepsParser, sepsRegex) = this.buildDelims(separators)
    val (termsParser, termsRegex) = this.buildDelims(terminators)
    val pSeps: Parser[String] = this.combineLongest(sepsParser)
    val pTerms: Parser[String] = this.combineLongest(termsParser)
    val pDelims: Parser[String] = pSeps ||| pTerms
    val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)

    val escapeEscapeCharacterRegex = convertDFDLLiteralToRegex(escapeEscapeCharacter)
    val escapeCharacterRegex = convertDFDLLiteralToRegex(escapeCharacter)

    val pEscape: Parser[String] = escapeCharacterRegex.r
    val pEscapeEscape: Parser[String] = escapeEscapeCharacterRegex.r
    val pEscapedEscape = pEscapeEscape ~ pEscape
    val pUnescapedDelims = ((pEscapeEscape ~ pEscape) ~> pDelims) | (not(pEscape) ~> pDelims) | (pEscapeEscape ~> pDelims) | pDelims

    val pEOF: Parser[String] = """\z""".r

    val hasEscEsc: Boolean = escapeEscapeCharacter.length() > 0

    val rBeforeIgnoreTrailingPadding = """(?s)(.*?)(?=(?:(?<!(?<!%1$s)%2$s)%3$s)|\z)"""
    val rBeforeIgnoreTrailingPaddingNoEscEsc = """(?s)(.*?)(?=(?:(?<!%1$s)%2$s)|\z)"""
    val pBeforeIgnoreTrailingPadding: Parser[String] = {
      if (hasEscEsc) rBeforeIgnoreTrailingPadding.format(escapeEscapeCharacterRegex, escapeCharacterRegex, delimsRegex).r
      else rBeforeIgnoreTrailingPaddingNoEscEsc.format(escapeCharacterRegex, delimsRegex).r
    }

    val rBeforeNoPadding = """(?s)(.*?)(?=((?<!(?<!%1$s)%2$s)%3$s)|(\z))"""
    val rBeforeNoPaddingNoEscEsc = """(?s)(.*?)(?=((?<!%1$s)%2$s)|(\z))"""
    val pBeforeNoPadding: Parser[String] = {
      if (hasEscEsc) rBeforeNoPadding.format(escapeEscapeCharacterRegex, escapeCharacterRegex, delimsRegex).r
      else rBeforeNoPaddingNoEscEsc.format(escapeCharacterRegex, delimsRegex).r
    }

    val rBeforeNoPaddingOrDelims = """(?s)(.*?)(?=(\z))"""
    val pBeforeNoPaddingOrDelims: Parser[String] = rBeforeNoPaddingOrDelims.r

    val rBeforeNoDelims = """(?s)(.*?)(?=(\z))"""
    val pBeforeNoDelims: Parser[String] = rBeforeNoDelims.r

    val hasDelim: Boolean = separators.size > 0 || terminators.size > 0

    val pFieldAndDelim: Parser[(Vector[String], String)] = (isMissingDelimAllowed, hasDelim) match {
      case (false, true) => {
        val contentDelimReq: Parser[(Vector[String], String)] = pBeforeNoPadding ~ pUnescapedDelims ^^ { case (b ~ d) => (Vector(b) -> d) }
        contentDelimReq
      }
      case (true, true) => {
        val content: Parser[(Vector[String], String)] = pBeforeIgnoreTrailingPadding ~ (pUnescapedDelims | pEOF) ^^ { case (b ~ d) => (Vector(b) -> d) }
        content
      }
      case (true, false) => {
        val content: Parser[(Vector[String], String)] = pBeforeNoPaddingOrDelims ~ (pEOF) ^^ { case (b ~ d) => (Vector(b) -> d) }
        content
      }
      case _ => Assert.invariantFailed() // return failedResult
    }

    val result1 = parseInputEscapeCharContent(pFieldAndDelim, pSeps, pTerms, input, TextJustificationType.None)
    val result = result1 match {
      case s: DelimParseSuccess => {
        val field = s.get
        val newField = removeEscapeCharacters(field, escapeEscapeCharacterRegex, escapeCharacterRegex, delimsRegex)
        s.copy(fieldArg = newField, nextArg = s.next)
        //        DelimParseSuccessFactory(Success(newField, s.next), s.delimiter, s.delimiterType, Some(field),
        //          s.delimiterLoc)
      }
      case f: DelimParseFailure => f
    }
    result
  }

  /* 
   * Parses the input using escapeSchemeKind='character' for when the
   * escape and escapeEscape are the same.
   */
  private def parseInputEscapeCharacter_SameNoPad(separators: Set[String], terminators: Set[String],
                                                  input: Reader[Char], escapeCharacter: String,
                                                  isMissingDelimAllowed: Boolean = true): DelimParseResult = {

    //if (escapeCharacter.length() == 0) { return failedResult }
    Assert.invariant(escapeCharacter.length > 0)

    val (sepsParser, sepsRegex) = this.buildDelims(separators)
    val (termsParser, termsRegex) = this.buildDelims(terminators)
    val pSeps: Parser[String] = this.combineLongest(sepsParser)
    val pTerms: Parser[String] = this.combineLongest(termsParser)
    val pDelims: Parser[String] = pSeps ||| pTerms
    val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)

    val escapeCharacterRegex = convertDFDLLiteralToRegex(escapeCharacter)

    val pEscape: Parser[String] = escapeCharacterRegex.r
    val pEscapedEscape = (pEscape ~ pEscape) ^^ { case (e1 ~ e2) => (e1 + e2) } // concatenate escapes

    // Parser captures and creates a string representation of the escapes
    val pEscapes = ((pEscapedEscape*) ~ opt(pEscape)) ^^ {
      case (l ~ None) => l.mkString
      case (l ~ Some(esc)) => l.mkString + esc
    }

    val rBeforeIgnoreTrailingPadding = """(?s)(.*?)(?=((?<!%1$s)((%1$s%1$s)*)(%2$s))|(\z))"""
    val pBeforeIgnoreTrailingPadding: Parser[String] = rBeforeIgnoreTrailingPadding.format(escapeCharacterRegex, delimsRegex).r

    val rBeforeNoPadding = """(?s)(.*?)(?=(""" + // Give me everything from this point until...
      """((?<!%1$s)((%1$s%1$s)*)(%2$s))""" + // unescaped delimiter
      """|""" + // OR
      """\z))""" // End of data/file

    val pBeforeNoPadding: Parser[String] = rBeforeNoPadding.format(escapeCharacterRegex, delimsRegex).r

    val rBeforeNoPaddingOrDelim = """(?s)(.*?)(?=(\z))"""
    val pBeforeNoPaddingOrDelim: Parser[String] = rBeforeNoPaddingOrDelim.r

    // Here because of the nature of using the same character for escape and escapeEscape
    // we need to capture the escapes if they exist and make them part of the 'before'
    val pBeforeAndEscsIgnoreTrailingPadding = (pBeforeIgnoreTrailingPadding ~ opt(pEscapes)) ^^ {
      case (b ~ None) => b
      case (b ~ Some(e)) => (b + e)
    }
    val pBeforeAndEscsNoPadding = (pBeforeNoPadding ~ opt(pEscapes)) ^^ {
      case (b ~ None) => b
      case (b ~ Some(e)) => (b + e)
    }

    val pUnescapedDelims = ((pEscapedEscape) ~> pDelims) | (not(pEscape) ~> pDelims) | pDelims

    val pEOF: Parser[String] = """\z""".r

    val hasDelim: Boolean = separators.size > 0 || terminators.size > 0

    val pFieldAndDelim: Parser[(Vector[String], String)] = (isMissingDelimAllowed, hasDelim) match {
      case (false, true) => {
        val contentDelimReq: Parser[(Vector[String], String)] = pBeforeAndEscsIgnoreTrailingPadding ~ pUnescapedDelims ^^ { case (b ~ d) => (Vector(b) -> d) }
        contentDelimReq
      }
      case (true, true) => {
        val content: Parser[(Vector[String], String)] = (pBeforeAndEscsNoPadding ~ (pUnescapedDelims | pEOF)) ^^ { case (b ~ d) => (Vector(b) -> d) }
        content
      }
      case (true, false) => {
        val content: Parser[(Vector[String], String)] = (pBeforeNoPaddingOrDelim ~ (pEOF)) ^^ { case (b ~ d) => (Vector(b) -> d) }
        content
      }
      case _ => Assert.invariantFailed() // return failedResult
    }

    val result1 = parseInputEscapeCharContent(pFieldAndDelim, pSeps, pTerms, input, TextJustificationType.None)
    val result = result1 match {
      case s: DelimParseSuccess => {
        val field = s.get
        val newField = removeEscapeCharacters(field, escapeCharacterRegex, escapeCharacterRegex, delimsRegex)
        s.copy(fieldArg = newField, nextArg = s.next)
        //        DelimParseSuccessFactory(Success(newField, s.next), s.delimiter, s.delimiterType, Some(field),
        //          s.delimiterLoc)
      }
      case f: DelimParseFailure => f
    }
    result
  }

  /* 
   * Parses the input using escapeSchemeKind='character' for when the
   * escape and escapeEscape are the same.
   */
  private def parseInputEscapeCharacter_SameWithPad(separators: Set[String], terminators: Set[String],
                                                    input: Reader[Char], escapeCharacter: String,
                                                    justification: TextJustificationType.Type,
                                                    padChar: String,
                                                    isMissingDelimAllowed: Boolean = true): DelimParseResult = {

    // if (escapeCharacter.length() == 0 || padChar.length() == 0) { return failedResult }
    Assert.invariant(escapeCharacter.length() > 0 && padChar.length() > 0)

    val (sepsParser, sepsRegex) = this.buildDelims(separators)
    val (termsParser, termsRegex) = this.buildDelims(terminators)
    val pSeps: Parser[String] = this.combineLongest(sepsParser)
    val pTerms: Parser[String] = this.combineLongest(termsParser)
    val pDelims: Parser[String] = pSeps ||| pTerms
    val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)

    val escapeCharacterRegex = convertDFDLLiteralToRegex(escapeCharacter)
    val padCharRegex = convertDFDLLiteralToRegex(padChar)

    val pEscape: Parser[String] = escapeCharacterRegex.r
    val pEscapedEscape = (pEscape ~ pEscape) ^^ { case (e1 ~ e2) => (e1 + e2) } // concatenate escapes

    // Parser captures and creates a string representation of the escapes
    val pEscapes = ((pEscapedEscape*) ~ opt(pEscape)) ^^ {
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
    val rBefore = """(?s)(.*?)(?=""" + // Give me everything from this point until...
      """(?:(%1$s*)(%3$s*)(((?<!%1$s)(%2$s))|(\z)))""" + // An unescaped/escaped pad char followed by either an unescaped delimiter or end of data
      """|""" + // OR
      """(%1$s(%3$s+)(%2$s|\z))""" + // escape followed by one or more pad chars followed by a delimiter or end of data
      """|""" + // OR
      """((?<!%1$s)((%1$s%1$s)*)(%2$s))""" + // unescaped delimiter
      """|""" + // OR
      """\z)""" // End of data/file
    val pBefore: Parser[String] = rBefore.format(escapeCharacterRegex, delimsRegex, padCharRegex).r

    val rBeforeIgnoreTrailingPadding = """(?s)(.*?)(?=((?<!%1$s)((%1$s%1$s)*)(%2$s))|(\z))"""
    val pBeforeIgnoreTrailingPadding: Parser[String] = rBeforeIgnoreTrailingPadding.format(escapeCharacterRegex, delimsRegex).r

    val rBeforeNoPadding = """(?s)(.*?)(?=(""" + // Give me everything from this point until...
      """((?<!%1$s)((%1$s%1$s)*)(%2$s))""" + // unescaped delimiter
      """|""" + // OR
      """\z))""" // End of data/file
    val pBeforeNoPadding: Parser[String] = rBeforeNoPadding.format(escapeCharacterRegex, delimsRegex).r

    val rBeforeNoPaddingOrDelim = """(?s)(.*?)(?=(\z))"""
    val pBeforeNoPaddingOrDelim: Parser[String] = rBeforeNoPaddingOrDelim.r

    val rBeforeNoDelim = """(?s)(.*?)(?=""" + // Give me everything from this point until...
      """(?:(%1$s*)(%3$s*)(\z))""" + // An unescaped/escaped pad char followed by end of data
      """|""" + // OR
      """(%1$s(%3$s+)(\z))""" + // escape followed by one or more pad chars followed by a delimiter or end of data
      """|""" + // OR
      """\z)""" // End of data/file
    val pBeforeNoDelim: Parser[String] = rBeforeNoDelim.format(escapeCharacterRegex, delimsRegex, padCharRegex).r

    // Here because of the nature of using the same character for escape and escapeEscape
    // we need to capture the escapes if they exist and make them part of the 'before'
    val pBeforeAndEscsIgnoreTrailingPadding = (pBeforeIgnoreTrailingPadding ~ opt(pEscapes)) ^^ {
      case (b ~ None) => b
      case (b ~ Some(e)) => (b + e)
    }

    val pBeforeAndEscs = (pBefore ~ opt(pEscapes | pEscapedEscape | pEscape)) ^^ {
      case (b ~ None) => b
      case (b ~ Some(e)) => (b + e)
    }

    val pUnescapedDelims = ((pEscapedEscape) ~> pDelims) | (not(pEscape) ~> pDelims) | pDelims

    val rPadChar = """(%s*)"""
    val pPadChar: Parser[String] = rPadChar.format(padCharRegex).r
    val rLeftPadChar = """(%1$s*)(?=([^%1$s]))""" // LeftPad precedes non pad characters
    val pLeftPadChar: Parser[String] = rLeftPadChar.format(padCharRegex).r
    val pEOF: Parser[String] = """\z""".r

    val paddedContent: Parser[Vector[String]] = pLeftPadChar ~ pBeforeAndEscs ~ pPadChar ^^ { case (lp ~ c ~ rp) => Vector(lp, c, rp) }
    val leftPaddedContent: Parser[Vector[String]] = pLeftPadChar ~ pBeforeAndEscsIgnoreTrailingPadding ^^ { case (lp ~ c) => Vector(lp, c) }
    val rightPaddedContent: Parser[Vector[String]] = pBeforeAndEscs ~ pPadChar ^^ { case (c ~ rp) => Vector(c, rp) }

    val hasDelim: Boolean = separators.size > 0 || terminators.size > 0

    val pFieldAndDelim: Parser[(Vector[String], String)] = (justification, isMissingDelimAllowed, hasDelim) match {
      case (TextJustificationType.Left, false, true) => {
        val contentLeftDelimReq: Parser[(Vector[String], String)] = rightPaddedContent ~ pUnescapedDelims ^^ { case (c ~ d) => (c, d) }
        contentLeftDelimReq
      }
      case (TextJustificationType.Right, false, true) => {
        val contentRightDelimReq: Parser[(Vector[String], String)] = leftPaddedContent ~ pUnescapedDelims ^^ { case (c ~ d) => (c, d) }
        contentRightDelimReq
      }
      case (TextJustificationType.Center, false, true) => {
        val contentCenterDelimReq: Parser[(Vector[String], String)] = paddedContent ~ pDelims ^^ { case (bc ~ d) => (bc -> d) }
        contentCenterDelimReq
      }
      case (TextJustificationType.Left, true, true) => {
        val contentLeft: Parser[(Vector[String], String)] = rightPaddedContent ~ (pUnescapedDelims | pEOF) ^^ { case (c ~ d) => (c, d) }
        contentLeft
      }
      case (TextJustificationType.Left, true, false) => {
        val pBeforeAndEscs = (pBeforeNoDelim ~ opt(pEscapes | pEscapedEscape | pEscape)) ^^ {
          case (b ~ None) => b
          case (b ~ Some(e)) => (b + e)
        }
        val rightPaddedContent: Parser[Vector[String]] = pBeforeAndEscs ~ pPadChar ^^ { case (c ~ rp) => Vector(c, rp) }
        val contentLeft: Parser[(Vector[String], String)] = rightPaddedContent ~ (pEOF) ^^ { case (c ~ d) => (c, d) }
        contentLeft
      }
      case (TextJustificationType.Right, true, true) => {
        val contentRight: Parser[(Vector[String], String)] = leftPaddedContent ~ (pUnescapedDelims | pEOF) ^^ { case (c ~ d) => (c, d) }
        contentRight
      }
      case (TextJustificationType.Right, true, false) => {
        val pBeforeAndEscsIgnoreTrailingPadding = (pBeforeNoDelim ~ opt(pEscapes)) ^^ {
          case (b ~ None) => b
          case (b ~ Some(e)) => (b + e)
        }
        val leftPaddedContent: Parser[Vector[String]] = pLeftPadChar ~ pBeforeAndEscsIgnoreTrailingPadding ^^ { case (lp ~ c) => Vector(lp, c) }
        val contentRight: Parser[(Vector[String], String)] = leftPaddedContent ~ (pEOF) ^^ { case (c ~ d) => (c, d) }
        contentRight
      }
      case (TextJustificationType.Center, true, true) => {
        val contentCenter: Parser[(Vector[String], String)] = paddedContent ~ (pUnescapedDelims | pEOF) ^^ { case (c ~ d) => (c, d) }
        contentCenter
      }
      case (TextJustificationType.Center, true, false) => {
        val pBeforeAndEscs = (pBeforeNoDelim ~ opt(pEscapes | pEscapedEscape | pEscape)) ^^ {
          case (b ~ None) => b
          case (b ~ Some(e)) => (b + e)
        }
        val paddedContent: Parser[Vector[String]] = pLeftPadChar ~ pBeforeAndEscs ~ pPadChar ^^ { case (lp ~ c ~ rp) => Vector(lp, c, rp) }
        val contentCenter: Parser[(Vector[String], String)] = paddedContent ~ (pUnescapedDelims | pEOF) ^^ { case (c ~ d) => (c, d) }
        contentCenter
      }
      case _ => Assert.invariantFailed() //return failedResult
    }

    val result1 = parseInputEscapeCharContent(pFieldAndDelim, pSeps, pTerms, input, justification)
    val result = result1 match {
      case s: DelimParseSuccess => {
        val field = s.get
        val newField = removeEscapeCharacters(field, escapeCharacterRegex, escapeCharacterRegex, delimsRegex)
        s.copy(fieldArg = newField, nextArg = s.next)
        //        DelimParseSuccessFactory(Success(newField, s.next), s.delimiter, s.delimiterType, Some(field),
        //          s.delimiterLoc)
      }
      case f: DelimParseFailure => f
    }
    result
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

  /**
   * Assumes 'input' has had its delimiter picked off end already if it existed.
   */
  def removeEscapeCharacters(input: String, eses: String, es: String, delimRegex: String): String = {
    if (eses.equals(es)) {
      return removeEscapeCharactersSame(input, es)
    } else {
      return removeEscapeCharactersDiff(input, eses, es)
    }
  }

  private def removeEscapeCharactersSame(input: String, es: String): String = {
    // TODO: Move regular expressions out into central class
    // used to cleanup escape characters 
    val ERSplit = """(?s)(.*?)%1$s(.)(.*)""".format(es).r

    def removeActiveEscapes(str: String): String = {
      val res = str match {
        case ERSplit(before, theEsc, delim, after) => {
          val rest = removeActiveEscapes(after)
          before + delim + rest
        }
        case ERSplit(before, delim, after) => {
          val rest = removeActiveEscapes(after)
          before + delim + rest
        }
        case _ => str
      }
      res
    }
    removeActiveEscapes(input)
  }

  private def removeEscapeCharactersDiff(input: String, eses: String, es: String): String = {
    // TODO: Move regular expressions out into central class
    if (eses.length() > 0) {
      val rRemoveEscape = """(%1$s$)|(%1$s(?=%2$s))""" // Replace escape at end of line OR replace escapeEscape preceding escapeBlockEnd
      //val rRemoveUnescapedEscapes = """((?<!S)E)"""
      val rRemoveUnescapedEscapes = """((?<!%1$s)%2$s)"""
      val removeUnescapedEscapes = rRemoveUnescapedEscapes.format(eses, es)
      //val rRemoveEscapeEscapesThatEscape = """(S(?=E))"""
      val rRemoveEscapeEscapesThatEscape = """(%1$s(?=%2$s))"""
      val removeEscapeEscapesThatEscape = rRemoveEscapeEscapesThatEscape.format(eses, es)
      val r1 = input.replaceAll(removeUnescapedEscapes, "")
      val r2 = r1.replaceAll(removeEscapeEscapesThatEscape, "")
      return r2
    }
    val rRemoveEscape = """(%1$s)""".format(es)
    val r1 = input.replaceAll(rRemoveEscape, "")
    r1
  }

  /**
   * Assumes that valid escape block start and end were already removed.
   */
  def removeEscapesBlocks(input: String, eses: String, endBlockRegex: String): String = {
    //TODO: Move regular expressions out into central class
    val rRemoveEscape = """(%1$s$)|(%1$s(?=%2$s))""" // Replace escape at end of line OR replace escapeEscape preceding escapeBlockEnd
    val removeEscapes = rRemoveEscape.format(eses, endBlockRegex)
    input.replaceAll(removeEscapes, "")
  }

}
