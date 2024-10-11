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

package org.apache.daffodil.unparsers.runtime1

import java.lang.{ Long => JLong, Number => JNumber }

import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.lib.api.WarnID
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.lib.schema.annotation.props.gen.YesNo
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.lib.util.MaybeInt
import org.apache.daffodil.lib.util.Numbers._
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Evaluatable
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.Processor
import org.apache.daffodil.runtime1.processors.parsers.HasKnownLengthInBits
import org.apache.daffodil.runtime1.processors.parsers.HasRuntimeExplicitLength
import org.apache.daffodil.runtime1.processors.unparsers._

abstract class BinaryNumberBaseUnparser(override val context: ElementRuntimeData)
  extends PrimUnparser {

  protected def getNumberToPut(state: UState): JNumber = {
    val node = state.currentInfosetNode.asSimple
    node.dataValue.getNumber
  }

  protected def getBitLength(s: ParseOrUnparseState): Int

  protected def putNumber(
    dos: DataOutputStream,
    number: JNumber,
    nBits: Int,
    finfo: FormatInfo
  ): Boolean

  def unparse(state: UState): Unit = {
    val nBits = getBitLength(state)
    val value = getNumberToPut(state)
    val dos = state.dataOutputStream
    this match {
      case unparser: BinaryIntegerBaseUnparser => {
        val primNumeric = context.optPrimType.get.asInstanceOf[NodeInfo.PrimType.PrimNumeric]
        // minimum length for a signed binary integer is 2 bits, for unsigned it is 1 bit
        if (unparser.signed && nBits < 2) {
          val outOfRangeStr =
            "Minimum length for a signed binary integer is 2 bits, number of bits %d out of range. " +
              "An unsigned integer with length 1 bit could be used instead."
          if (state.tunable.allowSignedIntegerLength1Bit) {
            state.SDW(
              WarnID.SignedBinaryIntegerLength1Bit,
              outOfRangeStr,
              nBits
            )
          } else {
            UE(
              state,
              outOfRangeStr,
              nBits
            )
            return
          }
        } else if (!unparser.signed && nBits < 1) {
          UE(
            state,
            "Minimum length for an unsigned binary integer is 1 bit, number of bits %d out of range.",
            nBits
          )
          return
        }
        if (primNumeric.width.isDefined) {
          val width = primNumeric.width.get
          if (nBits > width) {
            UE(
              state,
              "Number of bits %d out of range, must be between 1 and %d bits.",
              nBits,
              width
            )
            return
          }
        }
      }
      case _ => // do nothing
    }
    val res =
      if (nBits > 0) {
        putNumber(dos, value, nBits, state)
      } else {
        true
      }

    if (!res) {
      Assert.invariant(dos.maybeRelBitLimit0b.isDefined)
      UnparseError(
        One(state.schemaFileLocation),
        One(state.currentLocation),
        "Insufficient space to unparse element %s, required %s bits, but only %s were available.",
        context.dpathElementCompileInfo.namedQName.toPrettyString,
        nBits,
        dos.maybeRelBitLimit0b.get
      )
    }
  }

}

abstract class BinaryIntegerBaseUnparser(e: ElementRuntimeData, val signed: Boolean)
  extends BinaryNumberBaseUnparser(e) {

  override def putNumber(
    dos: DataOutputStream,
    value: JNumber,
    nBits: Int,
    finfo: FormatInfo
  ): Boolean = {
    if (nBits > 64) {
      dos.putBigInt(asBigInt(value), nBits, signed, finfo)
    } else {
      dos.putLong(asLong(value), nBits, finfo)
    }
  }
}

class BinaryIntegerKnownLengthUnparser(
  e: ElementRuntimeData,
  signed: Boolean,
  override val lengthInBits: Int
) extends BinaryIntegerBaseUnparser(e, signed)
  with HasKnownLengthInBits {

  override lazy val runtimeDependencies = Vector()

}

class BinaryIntegerRuntimeLengthUnparser(
  val e: ElementRuntimeData,
  signed: Boolean,
  val lengthEv: Evaluatable[JLong],
  val lengthUnits: LengthUnits
) extends BinaryIntegerBaseUnparser(e, signed)
  with HasRuntimeExplicitLength {

  override val runtimeDependencies = Vector(lengthEv)
}

class BinaryIntegerPrefixedLengthUnparser(
  e: ElementRuntimeData,
  override val prefixedLengthUnparser: Unparser,
  override val prefixedLengthERD: ElementRuntimeData,
  maybeNBits: MaybeInt,
  signed: Boolean,
  override val lengthUnits: LengthUnits,
  override val prefixedLengthAdjustmentInUnits: Long
) extends BinaryIntegerBaseUnparser(e: ElementRuntimeData, signed: Boolean)
  with KnownPrefixedLengthUnparserMixin {

  override def childProcessors: Vector[Processor] = Vector(prefixedLengthUnparser)
  override lazy val runtimeDependencies = Vector()

  override def getBitLength(s: ParseOrUnparseState): Int = {
    if (maybeNBits.isDefined) {
      // not xs:integer, so is a fixed number of bits based on the prim type
      maybeNBits.get
    } else {
      // type is xs:integer, the length is determined by the minimum number of
      // bytes needed to represent the number
      val value = getNumberToPut(s.asInstanceOf[UState])
      val len = Math.max(asBigInt(value).bitLength, 1)
      val signedLen = if (signed) len + 1 else len
      (signedLen + 7) & ~0x7 // round up to nearest multilpe of 8
    }
  }

  override def unparse(state: UState): Unit = {
    unparsePrefixedLength(state)
    super.unparse(state)
  }
}

class BinaryFloatUnparser(e: ElementRuntimeData) extends BinaryNumberBaseUnparser(e) {

  override lazy val runtimeDependencies = Vector()

  override def getBitLength(s: ParseOrUnparseState) = 32

  override def putNumber(
    dos: DataOutputStream,
    value: JNumber,
    nBits: Int,
    finfo: FormatInfo
  ): Boolean = {
    dos.putBinaryFloat(asFloat(value), finfo)
  }

}

class BinaryDoubleUnparser(e: ElementRuntimeData) extends BinaryNumberBaseUnparser(e) {

  override lazy val runtimeDependencies = Vector()

  override def getBitLength(s: ParseOrUnparseState) = 64

  override def putNumber(
    dos: DataOutputStream,
    value: JNumber,
    nBits: Int,
    finfo: FormatInfo
  ): Boolean = {
    dos.putBinaryDouble(asDouble(value), finfo)
  }
}

class BinaryDecimalKnownLengthUnparser(
  e: ElementRuntimeData,
  signed: YesNo,
  binaryDecimalVirtualPoint: Int,
  val lengthInBits: Int
) extends BinaryDecimalUnparserBase(e, signed, binaryDecimalVirtualPoint)
  with HasKnownLengthInBits {

  override lazy val runtimeDependencies = Vector()

}

class BinaryDecimalRuntimeLengthUnparser(
  val e: ElementRuntimeData,
  signed: YesNo,
  binaryDecimalVirtualPoint: Int,
  val lengthEv: Evaluatable[JLong],
  val lengthUnits: LengthUnits
) extends BinaryDecimalUnparserBase(e, signed, binaryDecimalVirtualPoint)
  with HasRuntimeExplicitLength {

  override val runtimeDependencies = Vector(lengthEv)
}

class BinaryDecimalPrefixedLengthUnparser(
  e: ElementRuntimeData,
  override val prefixedLengthUnparser: Unparser,
  override val prefixedLengthERD: ElementRuntimeData,
  signed: YesNo,
  binaryDecimalVirtualPoint: Int,
  override val lengthUnits: LengthUnits,
  override val prefixedLengthAdjustmentInUnits: Long
) extends BinaryDecimalUnparserBase(e, signed, binaryDecimalVirtualPoint)
  with KnownPrefixedLengthUnparserMixin {

  override def childProcessors: Vector[Processor] = Vector(prefixedLengthUnparser)
  override lazy val runtimeDependencies = Vector()

  override def getBitLength(s: ParseOrUnparseState): Int = {
    // type is xs:decimal, the length is determined by the minimum number of
    // bytes needed to represent the number
    val value = getNumberToPut(s.asInstanceOf[UState])
    val len = Math.max(asBigInt(value).bitLength, 1)
    val signedLen = if (signed == YesNo.Yes) len + 1 else len
    (signedLen + 7) & ~0x7 // round up to nearest multilpe of 8
  }

  override def unparse(state: UState): Unit = {
    unparsePrefixedLength(state)
    super.unparse(state)
  }
}

abstract class BinaryDecimalUnparserBase(
  e: ElementRuntimeData,
  signed: YesNo,
  binaryDecimalVirtualPoint: Int
) extends BinaryNumberBaseUnparser(e) {

  override def getNumberToPut(state: UState): JNumber = {
    val node = state.currentInfosetNode.asSimple
    val value = node.dataValue.getNumber

    // We want to scale the bigInt by binaryDecimalVirtualPoint so that it is a BigInt
    val bigDec = asBigDecimal(value)
    //
    // NOTE: The code below would fail when using java.math.BigDecimal because of an ArithmeticException
    // due to the pow() call.  Adding the MathContext.DECIMAL128 fixed this issue.  This is the
    // defaultMathContext for scala's BigDecimal.  It's interesting to note that MathContext.UNLIMITED
    // also failed here.
    //
    val bigInt =
      if (binaryDecimalVirtualPoint != 0)
        bigDec.scaleByPowerOfTen(binaryDecimalVirtualPoint).toBigInteger()
      else bigDec.toBigInteger()
    bigInt
  }

  override def putNumber(
    dos: DataOutputStream,
    value: JNumber,
    nBits: Int,
    finfo: FormatInfo
  ): Boolean = {
    dos.putBigInt(asBigInt(value), nBits, signed == YesNo.Yes, finfo)
  }
}
