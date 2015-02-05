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
import edu.illinois.ncsa.daffodil.processors.DFDLCharReader
import edu.illinois.ncsa.daffodil.processors.DFDLDelimParser
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.EscapeScheme
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeBlock
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeChar
import edu.illinois.ncsa.daffodil.processors.FieldFactoryBase
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.ParseError
import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.processors.TextReader
import edu.illinois.ncsa.daffodil.processors.dfa
import edu.illinois.ncsa.daffodil.processors.dfa.DFADelimiter
import edu.illinois.ncsa.daffodil.processors.dfa.DFAField
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParser
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParserWithEscapeBlock
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.util.Maybe.toMaybe
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParserFactory
import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.dsom.RuntimeEncodingMixin
import edu.illinois.ncsa.daffodil.processors.EncodingInfo

class StringDelimitedParser(
  erd: ElementRuntimeData,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  ff: FieldFactoryBase,
  pf: TextDelimitedParserFactory,
  isDelimRequired: Boolean,
  override val encodingInfo: EncodingInfo)
  extends PrimParser(erd)
  with TextReader
  with RuntimeEncodingMixin {

  val dp = new DFDLDelimParser(erd, encodingInfo)

  def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): PState = {
    val res = {
      if (!parseResult.isDefined) this.PE(state, "%s - %s - Parse failed.", this.toString(), erd.prettyName)
      else {
        val result = parseResult.get
        val field = result.field.getOrElse("")
        val numBits = result.numBits
        val endCharPos = if (state.charPos == -1) result.numCharsRead else state.charPos + result.numCharsRead
        val endBitPos = state.bitPos + numBits
        state.simpleElement.setDataValue(field)
        val stateWithPos = state.withPos(endBitPos, endCharPos, One(result.next))
        if (result.matchedDelimiterValue.isDefined) stateWithPos.mpstate.withDelimitedText(result.matchedDelimiterValue.get, result.originalDelimiterRep)
        return stateWithPos
      }
    }
    res
  }

  def parse(start: PState): PState = withParseErrorThrowing(start) {

    // TODO: DFDL-451 - Has been put on the backburner until we can figure out the appropriate behavior
    //
    //      gram.checkDelimiterDistinctness(esObj.escapeSchemeKind, optPadChar, finalOptEscChar,
    //        finalOptEscEscChar, optEscBlkStart, optEscBlkEnd, delimsCooked, postEscapeSchemeEvalState)

    val (postEvalState, textParser, delims, delimsCooked, fieldDFA, scheme) = pf.getParser(start)

    val bytePos = (postEvalState.bitPos >> 3).toInt

    val reader = getReader(dcharset.charset, postEvalState.bitPos, postEvalState)
    val hasDelim = delimsCooked.length > 0

    start.mpstate.clearDelimitedText

    val result = try {
      if (scheme.isDefined) {
        scheme.get match {
          case s: EscapeSchemeBlock => textParser.asInstanceOf[TextDelimitedParserWithEscapeBlock].parse(reader, fieldDFA, s.fieldEscDFA, s.blockStartDFA, s.blockEndDFA, delims, isDelimRequired)
          case s: EscapeSchemeChar => textParser.parse(reader, fieldDFA, delims, isDelimRequired)
        }
      } else textParser.parse(reader, fieldDFA, delims, isDelimRequired)
    } catch {
      case mie: MalformedInputException =>
        throw new ParseError(One(erd.schemaFileLocation), Some(postEvalState), "Malformed input, length: %s", mie.getInputLength())
    }
    processResult(result, postEvalState)
  }
}

class LiteralNilDelimitedEndOfDataParser(
  erd: ElementRuntimeData,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  ff: FieldFactoryBase,
  pf: TextDelimitedParserFactory,
  nilValues: Seq[String],
  encInfo: EncodingInfo)
  extends StringDelimitedParser(erd, justificationTrim, pad, ff, pf, false, encInfo) {

  val isEmptyAllowed = nilValues.contains("%ES;") // TODO: move outside parser

  val isDelimRequired: Boolean = false

  override def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): PState = {
    val res = {
      if (!parseResult.isDefined) this.PE(state, "%s - %s - Parse failed.", this.toString(), erd.prettyName)
      else {
        val result = parseResult.get
        // We have a field, is it empty?
        val field = result.field.getOrElse("")
        val isFieldEmpty = field.length() == 0 // Note: field has been stripped of padChars

        if (isFieldEmpty && !isEmptyAllowed) {
          return this.PE(state, "%s - %s - Parse failed.", this.toString(), erd.prettyName)
        } else if ((isFieldEmpty && isEmptyAllowed) || // Empty, but must advance past padChars if there were any. 
          dp.isFieldDfdlLiteral(field, nilValues.toSet)) { // Not empty, but matches.
          // Contains a nilValue, Success!
          state.thisElement.setNilled()

          val numBits = result.numBits
          val endCharPos = if (state.charPos == -1) result.numCharsRead else state.charPos + result.numCharsRead
          val endBitPos = numBits + state.bitPos

          val stateWithPos = state.withPos(endBitPos, endCharPos, One(result.next))
          if (result.matchedDelimiterValue.isDefined) stateWithPos.mpstate.withDelimitedText(result.matchedDelimiterValue.get, result.originalDelimiterRep)
          return stateWithPos
        } else {
          // Fail!
          return this.PE(state, "%s - Does not contain a nil literal!", erd.prettyName)
        }
      }
    }
    res
  }
}

class HexBinaryDelimitedParser(
  erd: ElementRuntimeData,
  justificationTrim: TextJustificationType.Type,
  pad: Maybe[Char],
  ff: FieldFactoryBase,
  pf: TextDelimitedParserFactory,
  isDelimRequired: Boolean,
  encInfo: EncodingInfo)
  extends StringDelimitedParser(erd, justificationTrim, pad, ff, pf, isDelimRequired, encInfo) {

  override lazy val dcharset = new DFDLCharset("ISO-8859-1")

  override def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): PState = {
    val res = {
      if (!parseResult.isDefined) this.PE(state, "%s - %s - Parse failed.", this.toString(), erd.prettyName)
      else {
        val result = parseResult.get
        val field = result.field.getOrElse("")
        val numBits = knownEncodingStringBitLength(field)
        val endCharPos = if (state.charPos == -1) result.numCharsRead else state.charPos + result.numCharsRead
        val endBitPos = state.bitPos + numBits
        val hexStr = field.map(c => c.toByte.formatted("%02X")).mkString
        state.simpleElement.setDataValue(hexStr)
        val stateWithPos = state.withPos(endBitPos, endCharPos, One(result.next))
        if (result.matchedDelimiterValue.isDefined) stateWithPos.mpstate.withDelimitedText(result.matchedDelimiterValue.get, result.originalDelimiterRep)
        return stateWithPos
      }
    }
    res
  }
}
