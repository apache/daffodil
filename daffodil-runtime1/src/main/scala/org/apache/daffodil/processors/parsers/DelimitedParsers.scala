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

import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.processors.FieldDFAParseEv
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeBlockParserHelper
import edu.illinois.ncsa.daffodil.processors.dfa
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParserBase
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParserWithEscapeBlock
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.equality._; object ENoWarn { EqualitySuppressUnusedImportWarning() }
import java.nio.charset.StandardCharsets
import edu.illinois.ncsa.daffodil.util.MaybeChar
import edu.illinois.ncsa.daffodil.processors.AllTerminatingMarkupDelimiterIterator
import passera.unsigned.ULong

class StringDelimitedParser(
  erd: ElementRuntimeData,
  justificationTrim: TextJustificationType.Type,
  pad: MaybeChar,
  textParser: TextDelimitedParserBase,
  fieldDFAEv: FieldDFAParseEv,
  isDelimRequired: Boolean)
  extends PrimParserObject(erd)
  with CaptureParsingValueLength {

  override val runtimeDependencies = List(fieldDFAEv, erd.encInfo.charsetEv)

  override val charsetEv = erd.encInfo.charsetEv

  def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): Unit = {

    if (!parseResult.isDefined) this.PE(state, "%s - %s - Parse failed.", this.toString(), erd.diagnosticDebugName)
    else {
      val result = parseResult.get
      val field = if (result.field.isDefined) result.field.get else ""
      state.simpleElement.setDataValue(field)
      captureValueLengthOfString(state, field)
      if (result.matchedDelimiterValue.isDefined)
        state.saveDelimitedParseResult(parseResult)
    }

  }

  def parse(start: PState): Unit = {

    // TODO: DFDL-451 - Has been put on the backburner until we can figure out the appropriate behavior
    //
    //      gram.checkDelimiterDistinctness(esObj.escapeSchemeKind, optPadChar, finalOptEscChar,
    //        finalOptEscEscChar, optEscBlkStart, optEscBlkEnd, delimsCooked, postEscapeSchemeEvalState)

    val delimIter = new AllTerminatingMarkupDelimiterIterator(start.mpstate.delimiters)
    val fieldDFA = fieldDFAEv.evaluate(start)

    start.clearDelimitedParseResult

    val result = {
      if (textParser.isInstanceOf[TextDelimitedParserWithEscapeBlock]) {
        val s = fieldDFAEv.escapeSchemeEv.get.evaluate(start).asInstanceOf[EscapeSchemeBlockParserHelper]
        textParser.asInstanceOf[TextDelimitedParserWithEscapeBlock].parse(start.dataInputStream, fieldDFA, s.fieldEscDFA, s.blockStartDFA, s.blockEndDFA, delimIter, isDelimRequired)
      } else {
        textParser.parse(start.dataInputStream, fieldDFA, delimIter, isDelimRequired)
      }
    }
    processResult(result, start)
  }
}

class LiteralNilDelimitedEndOfDataParser(
  erd: ElementRuntimeData,
  justificationTrim: TextJustificationType.Type,
  pad: MaybeChar,
  textParser: TextDelimitedParserBase,
  fieldDFAEv: FieldDFAParseEv,
  override val cookedNilValuesForParse: List[String],
  rawNilValuesForParse: List[String],
  override val ignoreCase: Boolean)
  extends StringDelimitedParser(erd, justificationTrim, pad, textParser, fieldDFAEv, false)
  with NilMatcherMixin {

  val isDelimRequired: Boolean = false

  private def doPE(state: PState) = {
    val nilValuesDescription =
      if (rawNilValuesForParse.length > 1)
        "(one of) '" + rawNilValuesForParse.mkString(" ") + "'."
      else
        "'" + rawNilValuesForParse.head + "'"
    this.PE(state, "%s - %s - Parse failed. Does not contain a nil literal matching %s", this.toString(), erd.diagnosticDebugName, nilValuesDescription)
  }

  override def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): Unit = {
    if (!parseResult.isDefined) doPE(state)
    else {
      val result = parseResult.get
      // We have a field, is it empty?
      val field = if (result.field.isDefined) result.field.get else ""
      val isFieldEmpty = field.length() == 0 // Note: field has been stripped of padChars

      lazy val isNilLiteral = isFieldNilLiteralValue(field)
      if (isFieldEmpty && !isEmptyAllowed && !isNilLiteral) {
        doPE(state)
        return
      } else if ((isFieldEmpty && isEmptyAllowed) || // Empty, but must advance past padChars if there were any.
        isNilLiteral) { // Not empty, but matches.
        // Contains a nilValue, Success!
        state.thisElement.setNilled()
        captureValueLengthOfString(state, field)
        if (result.matchedDelimiterValue.isDefined) state.saveDelimitedParseResult(parseResult)
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
  textParser: TextDelimitedParserBase,
  fieldDFAEv: FieldDFAParseEv,
  isDelimRequired: Boolean)
  extends StringDelimitedParser(erd, TextJustificationType.None, MaybeChar.Nope, textParser, fieldDFAEv, isDelimRequired) {

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
    Assert.invariant(erd.encodingInfo.isKnownEncoding && erd.encodingInfo.knownEncodingCharset.charset =:= StandardCharsets.ISO_8859_1)

    if (!parseResult.isDefined) this.PE(state, "%s - %s - Parse failed.", this.toString(), erd.diagnosticDebugName)
    else {
      val result = parseResult.get
      val field = if (result.field.isDefined) result.field.get else ""
      val fieldBytes = field.getBytes(StandardCharsets.ISO_8859_1)
      state.simpleElement.setDataValue(fieldBytes)
      captureValueLength(state, ULong(0), ULong(fieldBytes.length * 8))
      if (result.matchedDelimiterValue.isDefined) state.saveDelimitedParseResult(parseResult)
      return
    }
  }
}
