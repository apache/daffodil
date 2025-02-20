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

package org.apache.daffodil.runtime1.processors.parsers

import org.apache.daffodil.lib.equality._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.PackedSignCodes
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.EscapeSchemeBlockParserHelper
import org.apache.daffodil.runtime1.processors.FieldDFAParseEv
import org.apache.daffodil.runtime1.processors.TextJustificationType
import org.apache.daffodil.runtime1.processors.dfa
import org.apache.daffodil.runtime1.processors.dfa.TextDelimitedParserBase
import org.apache.daffodil.runtime1.processors.dfa.TextDelimitedParserWithEscapeBlock;
object ENoWarn { EqualitySuppressUnusedImportWarning() }
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInteger }
import java.nio.charset.StandardCharsets

import org.apache.daffodil.io.processors.charset.StandardBitsCharsets
import org.apache.daffodil.lib.util.DecimalUtils
import org.apache.daffodil.lib.util.MaybeChar
import org.apache.daffodil.runtime1.processors.AllTerminatingMarkupDelimiterIterator

import passera.unsigned.ULong

class StringDelimitedParser(
  override val context: ElementRuntimeData,
  justificationTrim: TextJustificationType.Type,
  pad: MaybeChar,
  textParser: TextDelimitedParserBase,
  fieldDFAEv: FieldDFAParseEv,
  isDelimRequired: Boolean
) extends TextPrimParser
  with CaptureParsingValueLength {

  override def runtimeDependencies = Vector(fieldDFAEv, context.encInfo.charsetEv)

  override val charsetEv = context.encInfo.charsetEv

  def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): Unit = {

    if (!parseResult.isDefined)
      this.PE(state, "%s - %s - Parse failed.", this.toString(), context.diagnosticDebugName)
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

    val delimIter = new AllTerminatingMarkupDelimiterIterator(
      start.mpstate.delimiters,
      start.mpstate.delimitersLocalIndexStack.top
    )
    val fieldDFA = fieldDFAEv.evaluate(start)

    start.clearDelimitedParseResult()

    val result = {
      if (textParser.isInstanceOf[TextDelimitedParserWithEscapeBlock]) {
        val s = fieldDFAEv.escapeSchemeEv.get
          .evaluate(start)
          .asInstanceOf[EscapeSchemeBlockParserHelper]
        textParser
          .asInstanceOf[TextDelimitedParserWithEscapeBlock]
          .parse(
            start,
            start.dataInputStream,
            fieldDFA,
            s.fieldEscDFA,
            s.blockStartDFA,
            s.blockEndDFA,
            delimIter,
            isDelimRequired
          )
      } else {
        textParser.parse(start, start.dataInputStream, fieldDFA, delimIter, isDelimRequired)
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
  override val ignoreCase: Boolean
) extends StringDelimitedParser(erd, justificationTrim, pad, textParser, fieldDFAEv, false)
  with NilMatcherMixin {

  val isDelimRequired: Boolean = false

  private def doPE(state: PState) = {
    val nilValuesDescription =
      if (rawNilValuesForParse.length > 1)
        "(one of) '" + rawNilValuesForParse.mkString(" ") + "'."
      else
        "'" + rawNilValuesForParse.head + "'"
    this.PE(
      state,
      "%s - %s - Parse failed. Does not contain a nil literal matching %s",
      this.toString(),
      erd.diagnosticDebugName,
      nilValuesDescription
    )
  }

  override def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): Unit = {
    if (!parseResult.isDefined) doPE(state)
    else {
      val result = parseResult.get
      // We have a field, is it empty?
      val field = if (result.field.isDefined) result.field.get else ""
      val isFieldEmpty = field.length() == 0 // Note: field has been stripped of padChars

      lazy val isNilLiteral = isFieldNilLiteralValue(field)
      if (
        (isFieldEmpty && isEmptyAllowed) || // Empty, but must advance past padChars if there were any.
        isNilLiteral
      ) { // Not empty, but matches.
        // Contains a nilValue, Succes ParseResult indiciates nilled
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
  isDelimRequired: Boolean
) extends StringDelimitedParser(
    erd,
    TextJustificationType.None,
    MaybeChar.Nope,
    textParser,
    fieldDFAEv,
    isDelimRequired
  ) {

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
    Assert.invariant(
      erd.encodingInfo.isKnownEncoding && erd.encodingInfo.knownEncodingCharset =:= StandardBitsCharsets.ISO_8859_1
    )

    if (!parseResult.isDefined)
      this.PE(state, "%s - %s - Parse failed.", this.toString(), erd.diagnosticDebugName)
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

class PackedIntegerDelimitedParser(
  erd: ElementRuntimeData,
  textParser: TextDelimitedParserBase,
  fieldDFAEv: FieldDFAParseEv,
  isDelimRequired: Boolean,
  packedSignCodes: PackedSignCodes
) extends PackedBinaryIntegerDelimitedBaseParser(erd, textParser, fieldDFAEv, isDelimRequired) {

  override def toNumber(num: Array[Byte]): JBigInteger =
    DecimalUtils.packedToBigInteger(num, packedSignCodes)

}

class PackedDecimalDelimitedParser(
  erd: ElementRuntimeData,
  textParser: TextDelimitedParserBase,
  fieldDFAEv: FieldDFAParseEv,
  isDelimRequired: Boolean,
  binaryDecimalVirtualPoint: Int,
  packedSignCodes: PackedSignCodes
) extends PackedBinaryDecimalDelimitedBaseParser(
    erd,
    textParser,
    fieldDFAEv,
    isDelimRequired,
    binaryDecimalVirtualPoint
  ) {

  override def toNumber(num: Array[Byte]): JBigDecimal =
    DecimalUtils.packedToBigDecimal(num, binaryDecimalVirtualPoint, packedSignCodes)

}

class BCDIntegerDelimitedParser(
  erd: ElementRuntimeData,
  textParser: TextDelimitedParserBase,
  fieldDFAEv: FieldDFAParseEv,
  isDelimRequired: Boolean
) extends PackedBinaryIntegerDelimitedBaseParser(erd, textParser, fieldDFAEv, isDelimRequired) {

  override def toNumber(num: Array[Byte]): JBigInteger = DecimalUtils.bcdToBigInteger(num)

}

class BCDDecimalDelimitedParser(
  erd: ElementRuntimeData,
  textParser: TextDelimitedParserBase,
  fieldDFAEv: FieldDFAParseEv,
  isDelimRequired: Boolean,
  binaryDecimalVirtualPoint: Int
) extends PackedBinaryDecimalDelimitedBaseParser(
    erd,
    textParser,
    fieldDFAEv,
    isDelimRequired,
    binaryDecimalVirtualPoint
  ) {

  override def toNumber(num: Array[Byte]): JBigDecimal =
    DecimalUtils.bcdToBigDecimal(num, binaryDecimalVirtualPoint)

}

class IBM4690PackedIntegerDelimitedParser(
  erd: ElementRuntimeData,
  textParser: TextDelimitedParserBase,
  fieldDFAEv: FieldDFAParseEv,
  isDelimRequired: Boolean
) extends PackedBinaryIntegerDelimitedBaseParser(erd, textParser, fieldDFAEv, isDelimRequired) {

  override def toNumber(num: Array[Byte]): JBigInteger =
    DecimalUtils.ibm4690ToBigInteger(num)

}

class IBM4690PackedDecimalDelimitedParser(
  erd: ElementRuntimeData,
  textParser: TextDelimitedParserBase,
  fieldDFAEv: FieldDFAParseEv,
  isDelimRequired: Boolean,
  binaryDecimalVirtualPoint: Int
) extends PackedBinaryDecimalDelimitedBaseParser(
    erd,
    textParser,
    fieldDFAEv,
    isDelimRequired,
    binaryDecimalVirtualPoint
  ) {

  override def toNumber(num: Array[Byte]): JBigDecimal =
    DecimalUtils.ibm4690ToBigDecimal(num, binaryDecimalVirtualPoint)

}
