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

import java.math.{ BigDecimal => JBigDecimal }
import java.math.{ BigInteger => JBigInteger }
import java.nio.charset.StandardCharsets

import org.apache.daffodil.io.processors.charset.StandardBitsCharsets
import org.apache.daffodil.lib.equality.TypeEqual
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.MaybeChar
import org.apache.daffodil.runtime1.dpath.InvalidPrimitiveDataException
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType
import org.apache.daffodil.runtime1.infoset.DataValue.DataValueNumber
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.FieldDFAParseEv
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.TextJustificationType
import org.apache.daffodil.runtime1.processors.dfa
import org.apache.daffodil.runtime1.processors.dfa.TextDelimitedParserBase

import passera.unsigned.ULong

trait PackedBinaryConversion {
  def toBigInteger(num: Array[Byte]): JBigInteger
  def toBigDecimal(num: Array[Byte], scale: Int): JBigDecimal

  def toPrimitiveType(context: ElementRuntimeData, num: Array[Byte]): DataValueNumber = {
    context.optPrimType.get match {
      case pn: PrimType.PrimNumeric => pn.fromNumber(toBigInteger(num))
      // Non-numeric types such as Time can still use these funcitons and
      // expect BigIntegers as the output of the conversion
      case _ => toBigInteger(num)
    }
  }

  def toPrimitiveType(context: ElementRuntimeData, num: Array[Byte], scale: Int): DataValueNumber = {
    context.optPrimType.get match {
      case pn: PrimType.PrimNumeric => pn.fromNumber(toBigDecimal(num, scale))
      // Non-numeric types such as Time can still use these funcitons and
      // expect BigDecimal as the output of the conversion
      case _ => toBigDecimal(num, scale)
    }
  }
}

trait PackedBinaryLengthCheck {
  def PE(state: PState, str: String, args: Any*): Unit
  def checkLengthNotEqualToZero(nBits: Int, start: PState, packedType: String): Boolean = {
    if (nBits == 0) {
      PE(
        start,
        s"Number of bits %d out of range for a packed $packedType.",
        nBits
      )
      false
    } else {
      true
    }
  }

  def checkLengthIsMultipleOf4(nBits: Int, start: PState): Boolean = {
    if ((nBits % 4) != 0) {
      PE(
        start,
        "The given length (%s bits) must be a multiple of 4 when using packed binary formats",
        nBits
      )
      false
    } else {
      true
    }
  }
}

abstract class PackedBinaryDecimalBaseParser(
  override val context: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int
) extends PrimParser
  with PackedBinaryConversion
  with PackedBinaryLengthCheck {
  override lazy val runtimeDependencies = Vector()

  protected def getBitLength(s: ParseOrUnparseState): Int

  def parse(start: PState): Unit = {
    val nBits = getBitLength(start)
    val lengthEqualsZero = !checkLengthNotEqualToZero(nBits, start, packedType = "decimal")
    if (lengthEqualsZero) return
    val lengthNotMultipleOf4 = !checkLengthIsMultipleOf4(nBits, start)
    if (lengthNotMultipleOf4) return

    val dis = start.dataInputStream

    if (!dis.isDefinedForLength(nBits)) {
      PENotEnoughBits(start, nBits, dis)
      return
    }

    try {
      val dec = toPrimitiveType(context, dis.getByteArray(nBits, start), binaryDecimalVirtualPoint)
      start.simpleElement.setDataValue(dec)
    } catch {
      case n: NumberFormatException => PE(start, "Error in packed data: \n%s", n.getMessage())
      case i: InvalidPrimitiveDataException => PE(start, "Error in packed data: \n%s", i.getMessage())
    }
  }
}

abstract class PackedBinaryIntegerBaseParser(
  override val context: ElementRuntimeData
) extends PrimParser
  with PackedBinaryConversion
  with PackedBinaryLengthCheck {
  override lazy val runtimeDependencies = Vector()

  val signed = {
    context.optPrimType.get match {
      case n: PrimType.PrimNumeric => n.isSigned
      // context.optPrimType can be of type date/time via ConvertZonedCombinator
      case _ => false
    }
  }
  protected def getBitLength(s: ParseOrUnparseState): Int

  def parse(start: PState): Unit = {
    val nBits = getBitLength(start)
    val lengthEqualsZero = !checkLengthNotEqualToZero(nBits, start, packedType = "integer")
    if (lengthEqualsZero) return
    val lengthNotMultipleOf4 = !checkLengthIsMultipleOf4(nBits, start)
    if (lengthNotMultipleOf4) return

    val dis = start.dataInputStream

    if (!dis.isDefinedForLength(nBits)) {
      PENotEnoughBits(start, nBits, dis)
      return
    }

    try {
      val int = toPrimitiveType(context, dis.getByteArray(nBits, start))
      start.simpleElement.setDataValue(int)
    } catch {
      case n: NumberFormatException => PE(start, "Error in packed data: \n%s", n.getMessage())
      case i: InvalidPrimitiveDataException => PE(start, "Error in packed data: \n%s", i.getMessage())
    }
  }
}

abstract class PackedBinaryIntegerDelimitedBaseParser(
  e: ElementRuntimeData,
  textParser: TextDelimitedParserBase,
  fieldDFAEv: FieldDFAParseEv,
  isDelimRequired: Boolean
) extends StringDelimitedParser(
    e,
    TextJustificationType.None,
    MaybeChar.Nope,
    textParser,
    fieldDFAEv,
    isDelimRequired
  )
  with PackedBinaryConversion {

  override def processResult(parseResult: Maybe[dfa.ParseResult], state: PState): Unit = {
    Assert.invariant(
      e.encodingInfo.isKnownEncoding && e.encodingInfo.knownEncodingCharset =:= StandardBitsCharsets.ISO_8859_1
    )

    if (!parseResult.isDefined)
      this.PE(state, "%s - %s - Parse failed.", this.toString(), e.diagnosticDebugName)
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
          val num = toPrimitiveType(context, fieldBytes)
          state.simpleElement.setDataValue(num)
        } catch {
          case n: NumberFormatException =>
            PE(state, "Error in packed data: \n%s", n.getMessage())
          case i: InvalidPrimitiveDataException => PE(state, "Error in packed data: \n%s", i.getMessage())
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
  binaryDecimalVirtualPoint: Int
) extends StringDelimitedParser(
    e,
    TextJustificationType.None,
    MaybeChar.Nope,
    textParser,
    fieldDFAEv,
    isDelimRequired
  )
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
    Assert.invariant(
      e.encodingInfo.isKnownEncoding && e.encodingInfo.knownEncodingCharset =:= StandardBitsCharsets.ISO_8859_1
    )

    if (!parseResult.isDefined)
      this.PE(state, "%s - %s - Parse failed.", this.toString(), e.diagnosticDebugName)
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
          val num = toPrimitiveType(e, fieldBytes, binaryDecimalVirtualPoint)
          state.simpleElement.setDataValue(num)
        } catch {
          case n: NumberFormatException =>
            PE(state, "Error in packed data: \n%s", n.getMessage())
          case i: InvalidPrimitiveDataException => PE(state, "Error in packed data: \n%s", i.getMessage())
        }

        if (result.matchedDelimiterValue.isDefined) state.saveDelimitedParseResult(parseResult)
      }
      return
    }
  }
}
