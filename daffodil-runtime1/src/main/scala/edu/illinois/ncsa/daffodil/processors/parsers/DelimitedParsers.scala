/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

import java.nio.charset.Charset
import java.nio.charset.MalformedInputException
import edu.illinois.ncsa.daffodil.dsom.ListOfStringValueAsLiteral
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeParserHelper
import edu.illinois.ncsa.daffodil.processors.FieldFactoryBase
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.ParseError
import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.processors.dfa
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
import edu.illinois.ncsa.daffodil.processors.dfa.DFAField
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParser
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParserWithEscapeBlock
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParserFactory
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeBlockParserHelper
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeCharParserHelper
import java.nio.charset.CharsetDecoder
import edu.illinois.ncsa.daffodil.processors.TextParserRuntimeMixin
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.equality._
import java.nio.charset.StandardCharsets

class StringDelimitedParser(
  erd: ElementRuntimeData,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  ff: FieldFactoryBase,
  pf: TextDelimitedParserFactory,
  isDelimRequired: Boolean)
  extends PrimParser(erd) {

  def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): Unit = {

    if (!parseResult.isDefined) this.PE(state, "%s - %s - Parse failed.", this.toString(), erd.prettyName)
    else {
      val result = parseResult.get
      val field = result.field.getOrElse("")
      state.simpleElement.setDataValue(field)
      if (result.matchedDelimiterValue.isDefined)
        state.withDelimitedText(result.matchedDelimiterValue.get, result.originalDelimiterRep)
    }

  }

  def parse(start: PState): Unit = withParseErrorThrowing(start) {

    // TODO: DFDL-451 - Has been put on the backburner until we can figure out the appropriate behavior
    //
    //      gram.checkDelimiterDistinctness(esObj.escapeSchemeKind, optPadChar, finalOptEscChar,
    //        finalOptEscEscChar, optEscBlkStart, optEscBlkEnd, delimsCooked, postEscapeSchemeEvalState)

    val (textParser, delims, delimsCooked, fieldDFA, scheme) = pf.getParser(start)

    val bytePos = (start.bitPos >> 3).toInt

    val hasDelim = delimsCooked.length > 0

    start.clearDelimitedText

    val result = try {
      if (scheme.isDefined) {
        scheme.get match {
          case s: EscapeSchemeBlockParserHelper =>
            textParser.asInstanceOf[TextDelimitedParserWithEscapeBlock].parse(start.dataInputStream, fieldDFA, s.fieldEscDFA, s.blockStartDFA, s.blockEndDFA, delims, isDelimRequired)
          case s: EscapeSchemeCharParserHelper =>
            textParser.parse(start.dataInputStream, fieldDFA, delims, isDelimRequired)
        }
      } else textParser.parse(start.dataInputStream, fieldDFA, delims, isDelimRequired)
    } catch {
      case mie: MalformedInputException =>
        throw new ParseError(One(erd.schemaFileLocation), One(start.currentLocation), "Malformed input, length: %s", mie.getInputLength())
    }
    processResult(result, start)
  }
}

class LiteralNilDelimitedEndOfDataParser(
  erd: ElementRuntimeData,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  ff: FieldFactoryBase,
  pf: TextDelimitedParserFactory,
  override val cookedNilValuesForParse: List[String],
  rawNilValuesForParse: List[String])
  extends StringDelimitedParser(erd, justificationTrim, pad, ff, pf, false)
  with NilMatcherMixin {

  val isDelimRequired: Boolean = false

  private def doPE(state: PState) = {
    val nilValuesDescription =
      if (rawNilValuesForParse.length > 1)
        "(one of) '" + rawNilValuesForParse.mkString(" ") + "'."
      else
        "'" + rawNilValuesForParse.head + "'"
    this.PE(state, "%s - %s - Parse failed. Does not contain a nil literal matching %s", this.toString(), erd.prettyName, nilValuesDescription)
  }

  override def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): Unit = {
    if (!parseResult.isDefined) doPE(state)
    else {
      val result = parseResult.get
      // We have a field, is it empty?
      val field = result.field.getOrElse("")
      val isFieldEmpty = field.length() == 0 // Note: field has been stripped of padChars

      lazy val isNilLiteral = isFieldNilLiteral(field)
      if (isFieldEmpty && !isEmptyAllowed && !isNilLiteral) {
        doPE(state)
        return
      } else if ((isFieldEmpty && isEmptyAllowed) || // Empty, but must advance past padChars if there were any. 
        isNilLiteral) { // Not empty, but matches.
        // Contains a nilValue, Success!
        state.thisElement.setNilled()
        if (result.matchedDelimiterValue.isDefined) state.withDelimitedText(result.matchedDelimiterValue.get, result.originalDelimiterRep)
        return
      } else {
        doPE(state)
        return
      }
    }
  }
}

class HexBinaryDelimitedParser(
  erd: ElementRuntimeData,
  ff: FieldFactoryBase,
  pf: TextDelimitedParserFactory,
  isDelimRequired: Boolean)
  extends StringDelimitedParser(erd, TextJustificationType.None, Nope, ff, pf, isDelimRequired) {

  /**
   * HexBinary is just a string in iso-8859-1 encoding.
   *
   * This works because java/scala's decoder for iso-8859-1 does not implement any
   * unmapping error detection. The official definition of iso-8859-1 has a few unmapped
   * characters, but most interpretations of iso-8859-1 implement these code points anyway, with
   * their unicode code points being exactly the byte values (interpreted unsigned).
   *
   * So, in scala/java anyway, it appears one can use iso-8859-1 as characters corresponding to
   * raw byte values.
   */

  override def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): Unit = {
    //Assert.invariant(erd.encodingInfo.isKnownEncoding && erd.encodingInfo.knownEncodingCharset.charset =:= StandardCharsets.ISO_8859_1)

    if (!parseResult.isDefined) this.PE(state, "%s - %s - Parse failed.", this.toString(), erd.prettyName)
    else {
      val result = parseResult.get
      val field = result.field.getOrElse("")
      val numBits = field.length * 8 // hexBinary each byte is a iso-8859-1 character
      val endBitPos = state.bitPos + numBits
      val hexStr = field.map(c => c.toByte.formatted("%02X")).mkString
      state.simpleElement.setDataValue(hexStr)
      if (result.matchedDelimiterValue.isDefined) state.withDelimitedText(result.matchedDelimiterValue.get, result.originalDelimiterRep)
      return
    }
  }
}
