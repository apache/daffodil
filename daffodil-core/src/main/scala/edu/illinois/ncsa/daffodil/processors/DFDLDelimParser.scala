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
import edu.illinois.ncsa.daffodil.processors.DelimiterLocation._
import scala.util.matching.Regex
import java.nio.charset.Charset
import java.util.regex.Pattern
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.dsom.AnnotatedSchemaComponent
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Info

class DelimParser(stringBitLengthFunction: String => Int) extends DFDLDelimParserCommon(stringBitLengthFunction) {

  def removePadding(input: String, justification: TextJustificationType.Type, padChar: String): String = {
    if ((padChar.length() == 0)) { return input }
    val field = this.generateRemovePaddingParser(justification, padChar)
    val result = field match {
      case None => input
      case Some(f) => {
        val res = this.parse(this.log(f)({
          justification match {
            case TextJustificationType.Left => "DelimParser.removePadding.leftJustified"
            case TextJustificationType.Right => "DelimParser.removePadding.rightJustified"
            case TextJustificationType.Center => "DelimParser.removePadding.centerJustified"
            case TextJustificationType.None => Assert.invariantFailed("should not be none if we're trimming.")
          }
        }), input)
        res.getOrElse(input)
      }
    }
    result
  }

  def parseInputPatterned(pattern: String, input: Reader[Char]): DelimParseResult = {
    val entry = this.generateInputPatternedParser(pattern)

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

    val entry = this.generateInputNCharactersParser(nChars)

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

  //  private def parseInputEscapeCharContent(fieldParser: Parser[(Vector[String], String)], seps: Parser[String], terms: Parser[String],
  //    input: Reader[Char], justification: TextJustificationType.Type): DelimParseResult = {
  //    parseInputDefaultContent(fieldParser, seps, terms, input, justification)
  //  }

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

    val (localDelimsParser, localDelimsRegex) = generateDelimiter(localDelims)
    val (remoteDelimsParser, remoteDelimsRegex) = generateDelimiter(remoteDelims)
    val entry = generateInputDelimiterParser(localDelimsParser, remoteDelimsParser)

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

    val hasDelim: Boolean = separators.size > 0 || terminators.size > 0
    val (pSeps, sepsRegex) = this.generateSeparators(separators)
    val (pTerms, termsRegex) = this.generateTerminators(terminators)
    val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)
    val padCharRegex = generateCharacterRegex(padChar);

    val pFieldAndDelim = justification match {
      case TextJustificationType.None => generateInputNoPadParser(pSeps, pTerms,
        delimsRegex, hasDelim, isMissingDelimAllowed)
      case _ => generateInputWithPadParser(pSeps, pTerms, delimsRegex, padCharRegex,
        justification, hasDelim, isMissingDelimAllowed)
    }

    val result = parseInputDefaultContent(pFieldAndDelim, pSeps, pTerms, input, justification)
    result
  }

  private def parseInput_WithPad(separators: Set[String], terminators: Set[String], input: Reader[Char],
                                 justification: TextJustificationType.Type,
                                 padChar: String,
                                 isMissingDelimAllowed: Boolean = true): DelimParseResult = {

    val hasDelim: Boolean = separators.size > 0 || terminators.size > 0
    val (pSeps, sepsRegex) = this.generateSeparators(separators)
    val (pTerms, termsRegex) = this.generateTerminators(terminators)
    val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)
    val padCharRegex = generateCharacterRegex(padChar);

    val pFieldAndDelim = this.generateInputWithPadParser(pSeps, pTerms, delimsRegex, padCharRegex,
      justification, hasDelim, isMissingDelimAllowed)

    val result = parseInputDefaultContent(pFieldAndDelim, pSeps, pTerms, input, justification)
    result
  }

  private def parseInput_NoPad(separators: Set[String], terminators: Set[String], input: Reader[Char],
                               isMissingDelimAllowed: Boolean = true): DelimParseResult = {
    // TODO: Move regular expressions out to central class

    val hasDelim: Boolean = separators.size > 0 || terminators.size > 0
    val (pSeps, sepsRegex) = this.generateSeparators(separators)
    val (pTerms, termsRegex) = this.generateTerminators(terminators)
    val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)

    val pFieldAndDelim = this.generateInputNoPadParser(pSeps, pTerms, delimsRegex, hasDelim, isMissingDelimAllowed)

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

    val hasEscEsc: Boolean = escapeEscapeCharacter.length() > 0
    val escapeBlockStartRegex = this.generateCharacterRegex(escapeBlockStart)
    val escapeBlockEndRegex = generateCharacterRegex(escapeBlockEnd)
    val escapeEscapeRegex = generateCharacterRegex(escapeEscapeCharacter)
    val padCharRegex = generateCharacterRegex(padChar);
    val (pSeps, sepsRegex) = this.generateSeparators(separators)
    val (pTerms, termsRegex) = this.generateTerminators(terminators)

    val pFieldAndDelim = {
      justification match {
        case TextJustificationType.None => this.generateEscapeBlockNoPadParser(pSeps, pTerms,
          escapeBlockStartRegex, escapeBlockEndRegex, escapeEscapeRegex, hasEscEsc, isMissingDelimAllowed)
        case _ => this.generateEscapeBlockWithPadParser(pSeps, pTerms,
          escapeBlockStartRegex, escapeBlockEndRegex, escapeEscapeRegex, padCharRegex, justification, hasEscEsc, isMissingDelimAllowed)
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

  private def parseInputEscapeBlock_WithPad(separators: Set[String], terminators: Set[String],
                                            input: Reader[Char], escapeBlockStart: String, escapeBlockEnd: String,
                                            escapeEscapeCharacter: String = "",
                                            justification: TextJustificationType.Type,
                                            padChar: String,
                                            isMissingDelimAllowed: Boolean = true): DelimParseResult = {

    Assert.invariant(escapeBlockStart.length() != 0 && escapeBlockEnd.length() != 0 && padChar.length() != 0)

    val hasEscEsc: Boolean = escapeEscapeCharacter.length() > 0
    val escapeBlockStartRegex = this.generateCharacterRegex(escapeBlockStart)
    val escapeBlockEndRegex = generateCharacterRegex(escapeBlockEnd)
    val escapeEscapeRegex = generateCharacterRegex(escapeEscapeCharacter)
    val padCharRegex = generateCharacterRegex(padChar);
    val (pSeps, sepsRegex) = this.generateSeparators(separators)
    val (pTerms, termsRegex) = this.generateTerminators(terminators)
    //val delimsRegex = this.generateDelimsRegex(sepsRegex, termsRegex)

    val pFieldAndDelim = generateEscapeBlockWithPadParser(pSeps, pTerms, escapeBlockStartRegex, escapeBlockEndRegex,
      escapeEscapeRegex, padCharRegex, justification, hasEscEsc, isMissingDelimAllowed)

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

    val hasEscEsc: Boolean = escapeEscapeCharacter.length() > 0
    val escapeBlockStartRegex = this.generateCharacterRegex(escapeBlockStart)
    val escapeBlockEndRegex = generateCharacterRegex(escapeBlockEnd)
    val escapeEscapeRegex = generateCharacterRegex(escapeEscapeCharacter)
    val (pSeps, sepsRegex) = this.generateSeparators(separators)
    val (pTerms, termsRegex) = this.generateTerminators(terminators)
    //val delimsRegex = this.generateDelimsRegex(sepsRegex, termsRegex)

    val pFieldAndDelim = generateEscapeBlockNoPadParser(pSeps, pTerms, escapeBlockStartRegex, escapeBlockEndRegex,
      escapeEscapeRegex, hasEscEsc, isMissingDelimAllowed)

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

    Assert.invariant(escapeCharacter.length() != 0)

    val hasEscEsc = escapeEscapeCharacter.length() > 0
    val hasDelim = separators.size > 0 || terminators.size > 0
    val escapeCharacterRegex = this.generateCharacterRegex(escapeCharacter)
    val escapeEscapeCharacterRegex = this.generateCharacterRegex(escapeEscapeCharacter)
    val padCharacterRegex = this.generateCharacterRegex(padChar)
    val (pSeps, sepsRegex) = this.generateSeparators(separators)
    val (pTerms, termsRegex) = this.generateTerminators(terminators)
    val delimsRegex = this.combineDelimitersRegex(sepsRegex, termsRegex)

    val pFieldAndDelim = {
      if (escapeEscapeCharacter.equals(escapeCharacter)) {
        justification match {
          case TextJustificationType.None => this.generateEscapeCharacterSameNoPadParser(pSeps, pTerms,
            escapeCharacterRegex, delimsRegex, hasDelim, isMissingDelimAllowed)
          case _ => this.generateEscapeCharacterSameWithPadParser(pSeps, pTerms,
            escapeCharacterRegex, delimsRegex, justification, padCharacterRegex, hasDelim, isMissingDelimAllowed)
        }
      } else {
        justification match {
          case TextJustificationType.None => this.generateEscapeCharacterDiffNoPadParser(pSeps, pTerms,
            escapeCharacterRegex, escapeEscapeCharacterRegex, delimsRegex, hasEscEsc, hasDelim, isMissingDelimAllowed)
          case _ => this.generateEscapeCharacterDiffWithPadParser(pSeps, pTerms,
            escapeCharacterRegex, escapeEscapeCharacterRegex, padCharacterRegex, delimsRegex,
            justification, hasEscEsc, hasDelim, isMissingDelimAllowed)
        }
      }
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
  private def parseInputEscapeCharacter_DiffWithPad(separators: Set[String], terminators: Set[String],
                                                    input: Reader[Char], escapeCharacter: String, escapeEscapeCharacter: String = "",
                                                    justification: TextJustificationType.Type,
                                                    padChar: String,
                                                    isMissingDelimAllowed: Boolean = true): DelimParseResult = {

    Assert.invariant(escapeCharacter.length() != 0 && padChar.length() != 0)

    val hasEscEsc = escapeEscapeCharacter.length() > 0
    val hasDelim = separators.size > 0 || terminators.size > 0
    val escapeCharacterRegex = this.generateCharacterRegex(escapeCharacter)
    val escapeEscapeCharacterRegex = this.generateCharacterRegex(escapeEscapeCharacter)
    val padCharacterRegex = this.generateCharacterRegex(padChar)
    val (pSeps, sepsRegex) = this.generateSeparators(separators)
    val (pTerms, termsRegex) = this.generateTerminators(terminators)
    val delimsRegex = this.combineDelimitersRegex(sepsRegex, termsRegex)
    val pFieldAndDelim = this.generateEscapeCharacterDiffWithPadParser(pSeps, pTerms,
      escapeCharacterRegex, escapeEscapeCharacterRegex, padCharacterRegex, delimsRegex, justification, hasEscEsc, hasDelim, isMissingDelimAllowed)

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

    Assert.invariant(escapeCharacter.length() > 0)

    val hasEscEsc = escapeEscapeCharacter.length() > 0
    val hasDelim = separators.size > 0 || terminators.size > 0

    val escapeCharacterRegex = this.generateCharacterRegex(escapeCharacter)
    val escapeEscapeCharacterRegex = this.generateCharacterRegex(escapeEscapeCharacter)
    val (pSeps, sepsRegex) = this.generateSeparators(separators)
    val (pTerms, termsRegex) = this.generateTerminators(terminators)
    val delimsRegex = this.combineDelimitersRegex(sepsRegex, termsRegex)
    val pFieldAndDelim = this.generateEscapeCharacterDiffNoPadParser(pSeps, pTerms,
      escapeCharacterRegex, escapeEscapeCharacterRegex, delimsRegex, hasEscEsc, hasDelim, isMissingDelimAllowed)

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

    val hasDelim = separators.size > 0 || terminators.size > 0
    val escapeCharacterRegex = this.generateCharacterRegex(escapeCharacter)
    val (pSeps, sepsRegex) = this.generateSeparators(separators)
    val (pTerms, termsRegex) = this.generateTerminators(terminators)
    val delimsRegex = this.combineDelimitersRegex(sepsRegex, termsRegex)
    val pFieldAndDelim = this.generateEscapeCharacterSameNoPadParser(pSeps, pTerms,
      escapeCharacterRegex, delimsRegex, hasDelim, isMissingDelimAllowed)

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

    val hasDelim = separators.size > 0 || terminators.size > 0
    val escapeCharacterRegex = this.generateCharacterRegex(escapeCharacter)
    val padCharacterRegex = this.generateCharacterRegex(padChar)
    val (pSeps, sepsRegex) = this.generateSeparators(separators)
    val (pTerms, termsRegex) = this.generateTerminators(terminators)
    val delimsRegex = this.combineDelimitersRegex(sepsRegex, termsRegex)
    val pFieldAndDelim = this.generateEscapeCharacterSameWithPadParser(pSeps, pTerms,
      escapeCharacterRegex, delimsRegex, justification, padChar, hasDelim, isMissingDelimAllowed)

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
    // used to cleanup escape characters 
    val ERSplit = this.generateRemoveEscapeCharactersSameRegex(es)
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
      val removeUnescapedEscapes = this.removeUnescapedEscapesRegex(eses, es) //rRemoveUnescapedEscapes.format(eses, es)
      val removeEscapeEscapesThatEscape = this.removeEscapeEscapesThatEscapeRegex(eses, es) //rRemoveEscapeEscapesThatEscape.format(eses, es)
      val r1 = input.replaceAll(removeUnescapedEscapes, "")
      val r2 = r1.replaceAll(removeEscapeEscapesThatEscape, "")
      return r2
    }
    val rRemoveEscape = this.removeEscapeRegex(es)
    val r1 = input.replaceAll(rRemoveEscape, "")
    r1
  }

  /**
   * Assumes that valid escape block start and end were already removed.
   */
  def removeEscapesBlocks(input: String, esesRegex: String, endBlockRegex: String): String = {
    val removeEscapes = this.removeEscapesBlocksRegex(esesRegex, endBlockRegex)
    input.replaceAll(removeEscapes, "")
  }

}
