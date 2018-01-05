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

package org.apache.daffodil.grammar.primitives

import scala.Boolean
import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.dsom.Term
import org.apache.daffodil.grammar.Gram
import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.processors.dfa.TextDelimitedParser
import org.apache.daffodil.processors.dfa.TextDelimitedParserWithEscapeBlock
import org.apache.daffodil.processors.dfa.TextPaddingParser
import org.apache.daffodil.processors.parsers.HexBinaryDelimitedParser
import org.apache.daffodil.processors.parsers.HexBinaryEndOfBitLimitParser
import org.apache.daffodil.processors.parsers.HexBinarySpecifiedLengthParser
import org.apache.daffodil.processors.parsers.PackedIntegerDelimitedParser
import org.apache.daffodil.processors.parsers.PackedDecimalDelimitedParser
import org.apache.daffodil.processors.parsers.BCDIntegerDelimitedParser
import org.apache.daffodil.processors.parsers.BCDDecimalDelimitedParser
import org.apache.daffodil.processors.parsers.IBM4690PackedIntegerDelimitedParser
import org.apache.daffodil.processors.parsers.IBM4690PackedDecimalDelimitedParser
import org.apache.daffodil.processors.parsers.LiteralNilDelimitedEndOfDataParser
import org.apache.daffodil.processors.parsers.OptionalInfixSepParser
import org.apache.daffodil.processors.parsers.StringDelimitedParser
import org.apache.daffodil.processors.parsers.StringOfSpecifiedLengthParser
import org.apache.daffodil.processors.parsers.{ Parser => DaffodilParser }
import org.apache.daffodil.processors.unparsers.HexBinaryMinLengthInBytesUnparser
import org.apache.daffodil.processors.unparsers.PackedIntegerDelimitedUnparser
import org.apache.daffodil.processors.unparsers.PackedDecimalDelimitedUnparser
import org.apache.daffodil.processors.unparsers.BCDIntegerDelimitedUnparser
import org.apache.daffodil.processors.unparsers.BCDDecimalDelimitedUnparser
import org.apache.daffodil.processors.unparsers.IBM4690PackedIntegerDelimitedUnparser
import org.apache.daffodil.processors.unparsers.IBM4690PackedDecimalDelimitedUnparser
import org.apache.daffodil.processors.unparsers.HexBinarySpecifiedLengthUnparser
import org.apache.daffodil.processors.unparsers.LiteralNilDelimitedEndOfDataUnparser
import org.apache.daffodil.processors.unparsers.OptionalInfixSepUnparser
import org.apache.daffodil.processors.unparsers.StringDelimitedUnparser
import org.apache.daffodil.processors.unparsers.StringMaybeTruncateBitsUnparser
import org.apache.daffodil.processors.unparsers.StringMaybeTruncateCharactersUnparser
import org.apache.daffodil.processors.unparsers.StringNoTruncateUnparser
import org.apache.daffodil.processors.unparsers.{ Unparser => DaffodilUnparser }
import org.apache.daffodil.schema.annotation.props.gen.EscapeKind
import org.apache.daffodil.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.util.Maybe.Nope
import org.apache.daffodil.util.Maybe.One
import org.apache.daffodil.util.PackedSignCodes
import org.apache.daffodil.processors.FieldDFAParseEv
import org.apache.daffodil.processors.TextTruncationType

case class HexBinarySpecifiedLength(e: ElementBase) extends Terminal(e, true) {

  override lazy val parser: DaffodilParser = new HexBinarySpecifiedLengthParser(e.elementRuntimeData, e.elementLengthInBitsEv)

  override lazy val unparser: DaffodilUnparser = new HexBinarySpecifiedLengthUnparser(e.elementRuntimeData, e.unparseTargetLengthInBitsEv)
}

case class StringOfSpecifiedLength(e: ElementBase) extends Terminal(e, true) with Padded {

  private def erd = e.elementRuntimeData

  override lazy val parser: DaffodilParser =
    new StringOfSpecifiedLengthParser(parsingPadChar,
      justificationTrim,
      erd)

  override lazy val unparser: DaffodilUnparser = {
    (e.stringTruncationType, e.lengthUnits) match {
      case (TextTruncationType.None, _) => new StringNoTruncateUnparser(erd)
      case (stt, LengthUnits.Characters) =>
        new StringMaybeTruncateCharactersUnparser(e.lengthEv, stt, erd)
      case (stt, lu) =>
        new StringMaybeTruncateBitsUnparser(
          e.unparseTargetLengthInBitsEv, stt, erd, e.charsetEv)
    }
  }
}

abstract class StringDelimited(e: ElementBase)
  extends StringDelimBase(e, true)
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

  //  /**
  //   * Called at compile time in static case, at runtime for dynamic case.
  //   */
  //  def errorIfDelimsHaveWSPStar(delims: List[String]): Unit = {
  //    if (delims.filter(x => x == "%WSP*;").length > 0) {
  //      // We cannot detect this error until expressions have been evaluated!
  //      log(LogLevel.Debug, "%s - Failed due to WSP* detected as a delimiter for lengthKind=delimited.", eName)
  //      context.schemaDefinitionError("WSP* cannot be used as a delimiter when lengthKind=delimited.")
  //    }
  //  }

  override lazy val parser: DaffodilParser = new StringDelimitedParser(
    e.elementRuntimeData,
    justificationTrim,
    parsingPadChar,
    textDelimitedParser,
    fieldDFAParseEv,
    isDelimRequired)

  override lazy val unparser: DaffodilUnparser =
    new StringDelimitedUnparser(e.elementRuntimeData,
      escapeSchemeUnparseEvOpt,
      isDelimRequired)

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

  override lazy val unparser: DaffodilUnparser = new HexBinaryMinLengthInBytesUnparser(
    e.minLength.longValue,
    e.elementRuntimeData)
}

case class HexBinaryDelimitedEndOfData(e: ElementBase)
  extends HexBinaryDelimited(e) {
  val isDelimRequired: Boolean = false
}

case class HexBinaryEndOfBitLimit(e: ElementBase) extends Terminal(e, true) {

  override lazy val parser: DaffodilParser = new HexBinaryEndOfBitLimitParser(e.elementRuntimeData)

  override lazy val unparser: DaffodilUnparser = new HexBinaryMinLengthInBytesUnparser(
    e.minLength.longValue,
    e.elementRuntimeData)
}

abstract class PackedIntegerDelimited(e: ElementBase, signed: Boolean, packedSignCodes: PackedSignCodes)
  extends StringDelimited(e) {

  override lazy val parser: DaffodilParser = new PackedIntegerDelimitedParser(
    e.elementRuntimeData,
    textDelimitedParser,
    fieldDFAParseEv,
    isDelimRequired,
    packedSignCodes)

  override lazy val unparser: DaffodilUnparser = new PackedIntegerDelimitedUnparser(
    e.elementRuntimeData,
    packedSignCodes)
}

case class PackedIntegerDelimitedEndOfData(e: ElementBase, signed: Boolean, packedSignCodes: PackedSignCodes)
  extends PackedIntegerDelimited(e, signed, packedSignCodes) {
  val isDelimRequired: Boolean = false
}

abstract class PackedDecimalDelimited(e: ElementBase, packedSignCodes: PackedSignCodes)
  extends StringDelimited(e) {

  override lazy val parser: DaffodilParser = new PackedDecimalDelimitedParser(
    e.elementRuntimeData,
    textDelimitedParser,
    fieldDFAParseEv,
    isDelimRequired,
    e.binaryDecimalVirtualPoint,
    packedSignCodes)

  override lazy val unparser: DaffodilUnparser = new PackedDecimalDelimitedUnparser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint,
    packedSignCodes)
}

case class PackedDecimalDelimitedEndOfData(e: ElementBase, packedSignCodes: PackedSignCodes)
  extends PackedDecimalDelimited(e, packedSignCodes) {
  val isDelimRequired: Boolean = false
}

abstract class BCDIntegerDelimited(e: ElementBase)
  extends StringDelimited(e) {

  override lazy val parser: DaffodilParser = new BCDIntegerDelimitedParser(
    e.elementRuntimeData,
    textDelimitedParser,
    fieldDFAParseEv,
    isDelimRequired)

  override lazy val unparser: DaffodilUnparser = new BCDIntegerDelimitedUnparser(
    e.elementRuntimeData)
}

case class BCDIntegerDelimitedEndOfData(e: ElementBase)
  extends BCDIntegerDelimited(e) {
  val isDelimRequired: Boolean = false
}

abstract class BCDDecimalDelimited(e: ElementBase)
  extends StringDelimited(e) {

  override lazy val parser: DaffodilParser = new BCDDecimalDelimitedParser(
    e.elementRuntimeData,
    textDelimitedParser,
    fieldDFAParseEv,
    isDelimRequired,
    e.binaryDecimalVirtualPoint)

  override lazy val unparser: DaffodilUnparser = new BCDDecimalDelimitedUnparser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint)
}

case class BCDDecimalDelimitedEndOfData(e: ElementBase)
  extends BCDDecimalDelimited(e) {
  val isDelimRequired: Boolean = false
}

abstract class IBM4690PackedIntegerDelimited(e: ElementBase, signed: Boolean)
  extends StringDelimited(e) {

  override lazy val parser: DaffodilParser = new IBM4690PackedIntegerDelimitedParser(
    e.elementRuntimeData,
    textDelimitedParser,
    fieldDFAParseEv,
    isDelimRequired)

  override lazy val unparser: DaffodilUnparser = new IBM4690PackedIntegerDelimitedUnparser(
    e.elementRuntimeData)
}

case class IBM4690PackedIntegerDelimitedEndOfData(e: ElementBase, signed: Boolean)
  extends IBM4690PackedIntegerDelimited(e, signed) {
  val isDelimRequired: Boolean = false
}

abstract class IBM4690PackedDecimalDelimited(e: ElementBase)
  extends StringDelimited(e) {

  override lazy val parser: DaffodilParser = new IBM4690PackedDecimalDelimitedParser(
    e.elementRuntimeData,
    textDelimitedParser,
    fieldDFAParseEv,
    isDelimRequired,
    e.binaryDecimalVirtualPoint)

  override lazy val unparser: DaffodilUnparser = new IBM4690PackedDecimalDelimitedUnparser(
    e.elementRuntimeData,
    e.binaryDecimalVirtualPoint)
}

case class IBM4690PackedDecimalDelimitedEndOfData(e: ElementBase)
  extends IBM4690PackedDecimalDelimited(e) {
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
      eb.rawNilValuesForParse,
      eb.ignoreCaseBool)

  override lazy val unparser: DaffodilUnparser =
    new LiteralNilDelimitedEndOfDataUnparser(eb.elementRuntimeData,
      eb.nilStringLiteralForUnparserEv,
      isDelimRequired)

}

case class PrefixLength(e: ElementBase) extends UnimplementedPrimitive(e, e.lengthKind == LengthKind.Prefixed)

class OptionalInfixSep(term: Term, sep: => Gram, guard: Boolean = true) extends Terminal(term, guard) {

  lazy val parser: DaffodilParser = new OptionalInfixSepParser(term.termRuntimeData, sep.parser)

  override lazy val unparser: DaffodilUnparser = new OptionalInfixSepUnparser(term.termRuntimeData, sep.unparser)
}
