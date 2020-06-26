/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.grammar.primitives

import scala.Boolean

import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.processors.FieldDFAParseEv
import org.apache.daffodil.processors.TextTruncationType
import org.apache.daffodil.processors.dfa.TextDelimitedParser
import org.apache.daffodil.processors.dfa.TextDelimitedParserWithEscapeBlock
import org.apache.daffodil.processors.dfa.TextPaddingParser
import org.apache.daffodil.processors.parsers.BCDDecimalDelimitedParser
import org.apache.daffodil.processors.parsers.BCDIntegerDelimitedParser
import org.apache.daffodil.processors.parsers.BlobSpecifiedLengthParser
import org.apache.daffodil.processors.parsers.HexBinaryDelimitedParser
import org.apache.daffodil.processors.parsers.HexBinaryEndOfBitLimitParser
import org.apache.daffodil.processors.parsers.HexBinaryLengthPrefixedParser
import org.apache.daffodil.processors.parsers.HexBinarySpecifiedLengthParser
import org.apache.daffodil.processors.parsers.IBM4690PackedDecimalDelimitedParser
import org.apache.daffodil.processors.parsers.IBM4690PackedIntegerDelimitedParser
import org.apache.daffodil.processors.parsers.LiteralNilDelimitedEndOfDataParser
import org.apache.daffodil.processors.parsers.PackedDecimalDelimitedParser
import org.apache.daffodil.processors.parsers.PackedIntegerDelimitedParser
import org.apache.daffodil.processors.parsers.StringDelimitedParser
import org.apache.daffodil.processors.parsers.StringOfSpecifiedLengthParser
import org.apache.daffodil.processors.parsers.{ Parser => DaffodilParser }
import org.apache.daffodil.processors.unparsers.BCDDecimalDelimitedUnparser
import org.apache.daffodil.processors.unparsers.BCDIntegerDelimitedUnparser
import org.apache.daffodil.processors.unparsers.BlobSpecifiedLengthUnparser
import org.apache.daffodil.processors.unparsers.HexBinaryLengthPrefixedUnparser
import org.apache.daffodil.processors.unparsers.HexBinaryMinLengthInBytesUnparser
import org.apache.daffodil.processors.unparsers.HexBinarySpecifiedLengthUnparser
import org.apache.daffodil.processors.unparsers.IBM4690PackedDecimalDelimitedUnparser
import org.apache.daffodil.processors.unparsers.IBM4690PackedIntegerDelimitedUnparser
import org.apache.daffodil.processors.unparsers.LiteralNilDelimitedEndOfDataUnparser
import org.apache.daffodil.processors.unparsers.PackedDecimalDelimitedUnparser
import org.apache.daffodil.processors.unparsers.PackedIntegerDelimitedUnparser
import org.apache.daffodil.processors.unparsers.StringDelimitedUnparser
import org.apache.daffodil.processors.unparsers.StringMaybeTruncateBitsUnparser
import org.apache.daffodil.processors.unparsers.StringMaybeTruncateCharactersUnparser
import org.apache.daffodil.processors.unparsers.StringNoTruncateUnparser
import org.apache.daffodil.processors.unparsers.{ Unparser => DaffodilUnparser }
import org.apache.daffodil.schema.annotation.props.gen.EscapeKind
import org.apache.daffodil.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.util.Maybe.Nope
import org.apache.daffodil.util.Maybe.One
import org.apache.daffodil.util.PackedSignCodes

case class HexBinarySpecifiedLength(e: ElementBase) extends Terminal(e, true) {

  override lazy val parser: DaffodilParser = new HexBinarySpecifiedLengthParser(e.elementRuntimeData, e.elementLengthInBitsEv)

  override lazy val unparser: DaffodilUnparser = new HexBinarySpecifiedLengthUnparser(e.elementRuntimeData, e.unparseTargetLengthInBitsEv)
}

case class BlobSpecifiedLength(e: ElementBase) extends Terminal(e, true) {

  override lazy val parser = new BlobSpecifiedLengthParser(e.elementRuntimeData, e.elementLengthInBitsEv)

  override lazy val unparser = new BlobSpecifiedLengthUnparser(e.elementRuntimeData, e.unparseTargetLengthInBitsEv)
}

case class StringOfSpecifiedLength(e: ElementBase) extends Terminal(e, true) with Padded {

  private def erd = e.elementRuntimeData

  override lazy val parser: DaffodilParser =
    new StringOfSpecifiedLengthParser(
      parsingPadChar,
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
    val ev = new FieldDFAParseEv(escapeSchemeParseEvOpt, context.dpathCompileInfo)
    ev.compile(context.tunable)
    ev
  }

  val textDelimitedParser = {
    val isBlockEscapeScheme = es.isDefined && es.get.escapeKind == EscapeKind.EscapeBlock
    if (isBlockEscapeScheme) {
      new TextDelimitedParserWithEscapeBlock(justificationTrim, parsingPadChar, e.erd)
    } else {
      new TextDelimitedParser(justificationTrim, parsingPadChar, e.erd)
    }
  }

  override lazy val parser: DaffodilParser = new StringDelimitedParser(
    e.elementRuntimeData,
    justificationTrim,
    parsingPadChar,
    textDelimitedParser,
    fieldDFAParseEv,
    isDelimRequired)

  override lazy val unparser: DaffodilUnparser =
    new StringDelimitedUnparser(
      e.erd,
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

case class HexBinaryLengthPrefixed(e: ElementBase) extends Terminal(e, true) {

  override lazy val parser: DaffodilParser = new HexBinaryLengthPrefixedParser(
    e.elementRuntimeData,
    e.prefixedLengthBody.parser,
    e.prefixedLengthElementDecl.elementRuntimeData,
    e.lengthUnits,
    e.prefixedLengthAdjustmentInUnits)

  override lazy val unparser: DaffodilUnparser = new HexBinaryLengthPrefixedUnparser(
    e.elementRuntimeData,
    e.prefixedLengthBody.unparser,
    e.prefixedLengthElementDecl.elementRuntimeData,
    e.minLength.longValue,
    e.lengthUnits,
    e.prefixedLengthAdjustmentInUnits)
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
    new LiteralNilDelimitedEndOfDataUnparser(
      eb.elementRuntimeData,
      eb.nilStringLiteralForUnparserEv,
      isDelimRequired)

}
