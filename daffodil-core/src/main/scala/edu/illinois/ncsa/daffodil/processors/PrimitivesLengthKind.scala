/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
import edu.illinois.ncsa.daffodil.processors.unparsers.{ Unparser => DaffodilUnparser }

import edu.illinois.ncsa.daffodil.util.LogLevel

import edu.illinois.ncsa.daffodil.processors.dfa.TextPaddingParser
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EscapeKind
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.processors.parsers.HexBinaryVariableLengthInBytesParser
import edu.illinois.ncsa.daffodil.processors.unparsers.HexBinaryVariableLengthInBytesUnparser
import edu.illinois.ncsa.daffodil.processors.parsers.LiteralNilDelimitedEndOfDataParser
import edu.illinois.ncsa.daffodil.processors.parsers.StringDelimitedParser
import edu.illinois.ncsa.daffodil.processors.parsers.HexBinaryFixedLengthInBytesParser
import edu.illinois.ncsa.daffodil.processors.unparsers.HexBinaryFixedLengthInBytesUnparser
import edu.illinois.ncsa.daffodil.processors.parsers.HexBinaryDelimitedParser
import edu.illinois.ncsa.daffodil.dsom.Term
import edu.illinois.ncsa.daffodil.processors.parsers.OptionalInfixSepParser
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParser
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedParserWithEscapeBlock
import edu.illinois.ncsa.daffodil.processors.unparsers.LiteralNilDelimitedEndOfDataUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.StringDelimitedUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.OptionalInfixSepUnparser
import edu.illinois.ncsa.daffodil.processors.parsers.StringOfSpecifiedLengthParser
import edu.illinois.ncsa.daffodil.processors.unparsers.Unparser
import edu.illinois.ncsa.daffodil.processors.unparsers.StringOfSpecifiedLengthUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.StringLiteralForUnparser
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.unparsers.HexBinaryDelimitedMinLengthInBytesUnparser

abstract class HexBinaryLengthInBytes(e: ElementBase)
  extends Terminal(e, true) {
  // nothing here
}

case class HexBinaryFixedLengthInBytes(e: ElementBase, nBytes: Long)
  extends HexBinaryLengthInBytes(e) {

  lazy val parserName = "HexBinaryFixedLengthInBytes"
  lazy val lengthText = nBytes.toString

  def getLength(pstate: PState): Long = {
    nBytes
  }

  override lazy val parser: DaffodilParser = new HexBinaryFixedLengthInBytesParser(nBytes,
    e.elementRuntimeData)

  override lazy val unparser: DaffodilUnparser = new HexBinaryFixedLengthInBytesUnparser(nBytes,
    e.elementRuntimeData)
}

case class HexBinaryVariableLengthInBytes(e: ElementBase)
  extends HexBinaryLengthInBytes(e) {

  lazy val parserName = "HexBinaryVariableLengthInBytes"
  lazy val lengthText = e.lengthEv.toString()

  override lazy val parser: DaffodilParser = new HexBinaryVariableLengthInBytesParser(e.elementRuntimeData,
    e.lengthEv)

  override lazy val unparser: DaffodilUnparser = new HexBinaryVariableLengthInBytesUnparser(e.elementRuntimeData,
    e.lengthEv)
}

case class StringOfSpecifiedLength(e: ElementBase) extends Terminal(e, true) with Padded {

  override lazy val parser: DaffodilParser =
    new StringOfSpecifiedLengthParser(parsingPadChar,
      justificationTrim,
      e.elementRuntimeData)

  // TODO: PERFORMANCE The unparser below currently uses the worst-case algorithm
  // which is what is needed for utf-8 where we don't know the length in bytes/bits until we
  // actually convert the utf-8 to bytes.
  // However, it does this even if the string is ascii, i.e., known fixed width.
  //
  // Either here or in the grammar, we should be choosing an algorithm that
  // if the encoding is known, if it is fixed width then we just calculate the fixed length in bytes/bits
  // directly rather than calling a method that outputs the string to bytes in order to measure it.

  override lazy val unparser: Unparser =
    new StringOfSpecifiedLengthUnparser(unparsingPadChar,
      justificationPad,
      e.elementRuntimeData, true)

}

abstract class StringDelimited(e: ElementBase)
  extends DelimParserBase(e, true)
  with Padded {

  // TODO: DFDL-451 - Has been placed on the backburner until we can figure out the appropriate behavior
  //
  //  requiredEvaluations(gram.checkDelimiterDistinctness(esObj.escapeSchemeKind, optPadChar, optEscChar,
  //    optEscEscChar, optEscBlkStart, optEscBlkEnd, staticDelimsCooked, elemBase))

  def isDelimRequired: Boolean

  lazy val es = e.optionEscapeScheme

  lazy val cname = toString

  lazy val eName = e.toString()

  lazy val leftPaddingOpt: Option[TextPaddingParser] = {
    if (!parsingPadChar.isDefined) None
    else Some(new TextPaddingParser(parsingPadChar.get, e.elementRuntimeData))
  }

  // TODO: move out of parser and into the dsom
  lazy val escapeSchemeParseEvOpt = if (es.isDefined) One(es.get.escapeSchemeParseEv) else Nope
  lazy val escapeSchemeUnparseEvOpt = if (es.isDefined) One(es.get.escapeSchemeUnparseEv) else Nope

  // TODO: move out of parser and into the dsom
  lazy val fieldDFAParseEv = {
    val ev = new FieldDFAParseEv(escapeSchemeParseEvOpt, context.runtimeData)
    ev.compile()
    ev
  }

  val textDelimitedParser = {
    val isBlockEscapeScheme = es.isDefined && es.get.escapeKind == EscapeKind.EscapeBlock
    if (isBlockEscapeScheme) {
      new TextDelimitedParserWithEscapeBlock(justificationTrim, parsingPadChar, e.elementRuntimeData)
    } else {
      new TextDelimitedParser(justificationTrim, parsingPadChar, e.elementRuntimeData)
    }
  }

  /**
   * Called at compile time in static case, at runtime for dynamic case.
   */
  def errorIfDelimsHaveWSPStar(delims: List[String]): Unit = {
    if (delims.filter(x => x == "%WSP*;").length > 0) {
      // We cannot detect this error until expressions have been evaluated!
      log(LogLevel.Debug, "%s - Failed due to WSP* detected as a delimiter for lengthKind=delimited.", eName)
      context.schemaDefinitionError("WSP* cannot be used as a delimiter when lengthKind=delimited.")
    }
  }

  override lazy val parser: DaffodilParser = new StringDelimitedParser(
    e.elementRuntimeData,
    justificationTrim,
    parsingPadChar,
    textDelimitedParser,
    fieldDFAParseEv,
    isDelimRequired)
  override lazy val unparser: DaffodilUnparser = new StringDelimitedUnparser(e.elementRuntimeData, justificationPad, parsingPadChar, escapeSchemeUnparseEvOpt, isDelimRequired)

}

case class StringDelimitedEndOfData(e: ElementBase)
  extends StringDelimited(e) {
  val isDelimRequired: Boolean = false
}

abstract class HexBinaryDelimited(e: ElementBase)
  extends StringDelimited(e) {

  override lazy val parser: DaffodilParser = new HexBinaryDelimitedParser(
    e.elementRuntimeData,
    textDelimitedParser,
    fieldDFAParseEv,
    isDelimRequired)

  override lazy val unparser: DaffodilUnparser = new HexBinaryDelimitedMinLengthInBytesUnparser(
    e.minLength.longValue,
    e.elementRuntimeData)
}

case class HexBinaryDelimitedEndOfData(e: ElementBase)
  extends HexBinaryDelimited(e) {
  val isDelimRequired: Boolean = false
}

case class LiteralNilDelimitedEndOfData(eb: ElementBase)
  extends StringDelimited(eb) {

  lazy val isDelimRequired: Boolean = false

  override lazy val parser: DaffodilParser =
    new LiteralNilDelimitedEndOfDataParser(
      eb.elementRuntimeData,
      justificationTrim,
      parsingPadChar,
      textDelimitedParser,
      fieldDFAParseEv,
      eb.cookedNilValuesForParse,
      eb.rawNilValuesForParse)

  private lazy val slNilValue =
    StringLiteralForUnparser(eb.elementRuntimeData, eb.outputNewLineEv, eb.rawNilValuesForUnparse.head)

  override lazy val unparser: DaffodilUnparser =
    new LiteralNilDelimitedEndOfDataUnparser(eb.elementRuntimeData, slNilValue, justificationPad, parsingPadChar, isDelimRequired)

}

case class PrefixLength(e: ElementBase) extends UnimplementedPrimitive(e, e.lengthKind == LengthKind.Prefixed)

class OptionalInfixSep(term: Term, sep: => Gram, guard: Boolean = true) extends Terminal(term, guard) {

  lazy val parser: DaffodilParser = new OptionalInfixSepParser(term.runtimeData, sep.parser)

  override lazy val unparser: DaffodilUnparser = new OptionalInfixSepUnparser(term.runtimeData, sep.unparser)
}
