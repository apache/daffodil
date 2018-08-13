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

package org.apache.daffodil.processors.parsers

import java.math.{ BigDecimal => JBigDecimal }
import java.math.{ BigInteger => JBigInteger }
import java.nio.charset.StandardCharsets

import org.apache.daffodil.equality.TypeEqual
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.FieldDFAParseEv
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.processors.TextJustificationType
import org.apache.daffodil.processors.charset.StandardBitsCharsets
import org.apache.daffodil.processors.dfa
import org.apache.daffodil.processors.dfa.TextDelimitedParserBase
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.util.MaybeChar
import passera.unsigned.ULong

trait PackedBinaryConversion {
  def toBigInteger(num: Array[Byte]): JBigInteger
  def toBigDecimal(num: Array[Byte], scale: Int): JBigDecimal
}

abstract class PackedBinaryDecimalBaseParser(
  override val context: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int)
  extends PrimParser
  with PackedBinaryConversion {
  override lazy val runtimeDependencies = Vector()

  protected def getBitLength(s: ParseOrUnparseState): Int

  def parse(start: PState): Unit = {
    val nBits = getBitLength(start)
    if (nBits == 0) return // zero length is used for outputValueCalc often.
    val dis = start.dataInputStream

    if (!dis.isDefinedForLength(nBits)) {
      PENotEnoughBits(start, nBits, dis.remainingBits)
      return
    }

    try {
      val bigDec = toBigDecimal(dis.getByteArray(nBits, start), binaryDecimalVirtualPoint)
      start.simpleElement.overwriteDataValue(bigDec)
    } catch {
      case n: NumberFormatException => PE(start, "Error in packed data: \n%s", n.getMessage())
    }
  }
}

abstract class PackedBinaryIntegerBaseParser(
  override val context: ElementRuntimeData,
  signed: Boolean = false)
  extends PrimParser
  with PackedBinaryConversion {
  override lazy val runtimeDependencies = Vector()

  protected def getBitLength(s: ParseOrUnparseState): Int

  def parse(start: PState): Unit = {
    val nBits = getBitLength(start)
    if (nBits == 0) return // zero length is used for outputValueCalc often.
    val dis = start.dataInputStream

    if (!dis.isDefinedForLength(nBits)) {
      PENotEnoughBits(start, nBits, dis.remainingBits)
      return
    }

    try {
      val int = toBigInteger(dis.getByteArray(nBits, start))
      if (!signed && (int.signum != 1))
        PE(start, "Expected unsigned data but parsed a negative number")
      else
        start.simpleElement.overwriteDataValue(int)
    } catch {
      case n: NumberFormatException => PE(start, "Error in packed data: \n%s", n.getMessage())
    }
  }
}

abstract class PackedBinaryIntegerDelimitedBaseParser(
  e: ElementRuntimeData,
  textParser: TextDelimitedParserBase,
  fieldDFAEv: FieldDFAParseEv,
  isDelimRequired: Boolean)
  extends StringDelimitedParser(e, TextJustificationType.None, MaybeChar.Nope, textParser, fieldDFAEv, isDelimRequired)
  with PackedBinaryConversion {

  override def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): Unit = {
    Assert.invariant(e.encodingInfo.isKnownEncoding && e.encodingInfo.knownEncodingCharset =:= StandardBitsCharsets.ISO_8859_1)

    if (!parseResult.isDefined) this.PE(state, "%s - %s - Parse failed.", this.toString(), e.diagnosticDebugName)
    else {
      val result = parseResult.get
      val field = if (result.field.isDefined) result.field.get else ""
      val fieldBytes = field.getBytes(StandardCharsets.ISO_8859_1)
      captureValueLength(state, ULong(0), ULong(fieldBytes.length * 8))
      if (field == "") {
        this.PE(state, "%s - %s - Parse failed.", this.toString(), e.diagnosticDebugName)
        return
      } else {
        try {
          val num = toBigInteger(fieldBytes)
          state.simpleElement.setDataValue(num)
        } catch {
          case n: NumberFormatException => PE(state, "Error in packed data: \n%s", n.getMessage())
        }

        if (result.matchedDelimiterValue.isDefined) state.saveDelimitedParseResult(parseResult)
      }
      return
    }
  }
}

abstract class PackedBinaryDecimalDelimitedBaseParser(
  e: ElementRuntimeData,
  textParser: TextDelimitedParserBase,
  fieldDFAEv: FieldDFAParseEv,
  isDelimRequired: Boolean,
  binaryDecimalVirtualPoint: Int)
  extends StringDelimitedParser(e, TextJustificationType.None, MaybeChar.Nope, textParser, fieldDFAEv, isDelimRequired)
  with PackedBinaryConversion {

  /**
   * We are treating packed binary formats as just a string in iso-8859-1 encoding.
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
    Assert.invariant(e.encodingInfo.isKnownEncoding && e.encodingInfo.knownEncodingCharset =:= StandardBitsCharsets.ISO_8859_1)

    if (!parseResult.isDefined) this.PE(state, "%s - %s - Parse failed.", this.toString(), e.diagnosticDebugName)
    else {
      val result = parseResult.get
      val field = if (result.field.isDefined) result.field.get else ""
      val fieldBytes = field.getBytes(StandardCharsets.ISO_8859_1)
      captureValueLength(state, ULong(0), ULong(fieldBytes.length * 8))
      if (field == "") {
        this.PE(state, "%s - %s - Parse failed.", this.toString(), e.diagnosticDebugName)
        return
      } else {
        try {
          val num = toBigDecimal(fieldBytes, binaryDecimalVirtualPoint)
          state.simpleElement.setDataValue(num)
        } catch {
          case n: NumberFormatException => PE(state, "Error in packed data: \n%s", n.getMessage())
        }

        if (result.matchedDelimiterValue.isDefined) state.saveDelimitedParseResult(parseResult)
      }
      return
    }
  }
}
