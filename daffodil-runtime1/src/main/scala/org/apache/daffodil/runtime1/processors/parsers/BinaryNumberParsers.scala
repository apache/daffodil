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

import java.lang.{ Double => JDouble, Float => JFloat, Long => JLong, Number => JNumber }
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInt }

import org.apache.daffodil.lib.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.lib.schema.annotation.props.gen.YesNo
import org.apache.daffodil.lib.schema.annotation.props.gen.YesNo.Yes
import org.apache.daffodil.runtime1.dpath.InvalidPrimitiveDataException
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Evaluatable
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.unparsers.UState

class BinaryFloatParser(override val context: ElementRuntimeData) extends PrimParser {
  override def runtimeDependencies = Vector()

  def parse(start: PState): Unit = {
    val dis = start.dataInputStream

    if (!dis.isDefinedForLength(32)) {
      PENotEnoughBits(start, 32, dis)
      return
    }

    val f: JFloat = dis.getBinaryFloat(start)
    start.simpleElement.overwriteDataValue(f)
  }
}

class BinaryDoubleParser(override val context: ElementRuntimeData) extends PrimParser {
  override def runtimeDependencies = Vector()

  def parse(start: PState): Unit = {
    val dis = start.dataInputStream

    if (!dis.isDefinedForLength(64)) {
      PENotEnoughBits(start, 64, dis)
      return
    }

    val d: JDouble = dis.getBinaryDouble(start)
    start.simpleElement.overwriteDataValue(d)
  }
}

class BinaryDecimalKnownLengthParser(
  e: ElementRuntimeData,
  signed: YesNo,
  binaryDecimalVirtualPoint: Int,
  val lengthInBits: Int
) extends BinaryDecimalParserBase(e, signed, binaryDecimalVirtualPoint)
  with HasKnownLengthInBits {}

class BinaryDecimalRuntimeLengthParser(
  val e: ElementRuntimeData,
  signed: YesNo,
  binaryDecimalVirtualPoint: Int,
  val lengthEv: Evaluatable[JLong],
  val lengthUnits: LengthUnits
) extends BinaryDecimalParserBase(e, signed, binaryDecimalVirtualPoint)
  with HasRuntimeExplicitLength {}

class BinaryDecimalBitLimitLengthParser(
  e: ElementRuntimeData,
  signed: YesNo,
  binaryDecimalVirtualPoint: Int
) extends BinaryDecimalParserBase(e, signed, binaryDecimalVirtualPoint)
  with BitLengthFromBitLimitMixin

abstract class BinaryDecimalParserBase(
  override val context: ElementRuntimeData,
  signed: YesNo,
  binaryDecimalVirtualPoint: Int
) extends PrimParser
  with BinaryNumberCheckWidth {
  override def runtimeDependencies = Vector()

  protected def getBitLength(s: ParseOrUnparseState): Int

  def parse(start: PState): Unit = {
    val nBits = getBitLength(start)
    val isSigned = signed == Yes
    val minWidth = if (isSigned) 2 else 1
    val res = checkMinWidth(start, isSigned, nBits, minWidth)
    if (!res) return
    val dis = start.dataInputStream
    if (!dis.isDefinedForLength(nBits)) {
      PENotEnoughBits(start, nBits, dis)
      return
    }

    val bigInt: JBigInt =
      if (signed == YesNo.Yes) { dis.getSignedBigInt(nBits, start) }
      else { dis.getUnsignedBigInt(nBits, start) }

    val bigDec = new JBigDecimal(bigInt, binaryDecimalVirtualPoint)
    start.simpleElement.overwriteDataValue(bigDec)
  }
}

class BinaryIntegerRuntimeLengthParser(
  val e: ElementRuntimeData,
  val lengthEv: Evaluatable[JLong],
  val lengthUnits: LengthUnits
) extends BinaryIntegerBaseParser(e)
  with HasRuntimeExplicitLength {}

class BinaryIntegerKnownLengthParser(
  e: ElementRuntimeData,
  val lengthInBits: Int
) extends BinaryIntegerBaseParser(e)
  with HasKnownLengthInBits {}

class BinaryIntegerBitLimitLengthParser(e: ElementRuntimeData)
  extends BinaryIntegerBaseParser(e)
  with BitLengthFromBitLimitMixin

abstract class BinaryIntegerBaseParser(
  override val context: ElementRuntimeData
) extends PrimParser
  with BinaryNumberCheckWidth {
  override def runtimeDependencies = Vector()

  protected def getBitLength(s: ParseOrUnparseState): Int

  private val primNumeric = context.optPrimType.get.asInstanceOf[NodeInfo.PrimType.PrimNumeric]

  def parse(start: PState): Unit = {
    val nBits = getBitLength(start)
    if (primNumeric.minWidth.isDefined) {
      val minWidth = primNumeric.minWidth.get
      val isSigned: Boolean = primNumeric.isSigned
      val res = checkMinWidth(start, isSigned, nBits, minWidth)
      if (!res) return
    }
    if (primNumeric.maxWidth.isDefined) {
      val maxWidth = primNumeric.maxWidth.get
      val res = checkMaxWidth(start, nBits, maxWidth)
      if (!res) return
    }
    val dis = start.dataInputStream
    if (!dis.isDefinedForLength(nBits)) {
      PENotEnoughBits(start, nBits, dis)
      return
    }

    val num: JNumber =
      if (primNumeric.isSigned) {
        if (nBits > 64) { dis.getSignedBigInt(nBits, start) }
        else { dis.getSignedLong(nBits, start) }
      } else {
        if (nBits >= 64) { dis.getUnsignedBigInt(nBits, start) }
        else { dis.getUnsignedLong(nBits, start).toLong }
      }

    val res =
      try {
        primNumeric.fromNumber(num)
      } catch {
        case e: InvalidPrimitiveDataException => {
          PE(start, "%s", e.getMessage)
          return
        }
      }

    start.simpleElement.overwriteDataValue(res)
  }
}

trait BinaryNumberCheckWidth {
  def checkMinWidth(
    state: ParseOrUnparseState,
    isSigned: Boolean,
    nBits: Int,
    minWidth: Int
  ): Boolean = {
    if (
      nBits < minWidth && !(isSigned && state.tunable.allowSignedIntegerLength1Bit && nBits == 1)
    ) {
      val signedStr = if (isSigned) "a signed" else "an unsigned"
      val outOfRangeStr =
        s"Minimum length for $signedStr binary number is $minWidth bit(s), number of bits $nBits out of range. " +
          "An unsigned number with length 1 bit could be used instead."
      val procErr = state.toProcessingError(outOfRangeStr)
      state match {
        case s: PState =>
          s.setFailed(procErr)
          return false
        case s: UState =>
          s.toss(procErr)
      }
    }
    true
  }

  def checkMaxWidth(state: ParseOrUnparseState, nBits: Int, maxWidth: Int): Boolean = {
    if (nBits > maxWidth) {
      val procErr = state.toProcessingError(
        s"Number of bits $nBits out of range, must be between 1 and $maxWidth bits."
      )
      state match {
        case s: PState =>
          s.setFailed(procErr)
          return false
        case s: UState =>
          s.toss(procErr)
      }
    }
    true
  }
}
