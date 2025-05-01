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

import java.lang.{ Long => JLong }
import java.lang.{ Number => JNumber }

import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.lib.schema.annotation.props.gen.YesNo
import org.apache.daffodil.lib.schema.annotation.props.gen.YesNo.Yes
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.lib.util.MaybeInt
import org.apache.daffodil.lib.util.Numbers._
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Evaluatable
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.parsers.BinaryNumberCheckWidth
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
    val dos = state.getDataOutputStream
    val res = putNumber(dos, value, nBits, state)

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

abstract class BinaryIntegerBaseUnparser(e: ElementRuntimeData)
  extends BinaryNumberBaseUnparser(e)
  with BinaryNumberCheckWidth {

  private val primNumeric = e.optPrimType.get.asInstanceOf[NodeInfo.PrimType.PrimNumeric]

  override def putNumber(
    dos: DataOutputStream,
    value: JNumber,
    nBits: Int,
    finfo: FormatInfo
  ): Boolean = {
    val state = finfo.asInstanceOf[UState]
    if (primNumeric.minWidth.isDefined) {
      val minWidth = primNumeric.minWidth.get
      val isSigned = primNumeric.isSigned
      checkMinWidth(state, isSigned, nBits, minWidth)
    }
    if (primNumeric.maxWidth.isDefined) {
      val maxWidth = primNumeric.maxWidth.get
      checkMaxWidth(state, nBits, maxWidth)
    }
    if (nBits > 64) {
      dos.putBigInt(asBigInt(value), nBits, primNumeric.isSigned, finfo)
    } else {
      dos.putLong(asLong(value), nBits, finfo)
    }
  }
}

class BinaryIntegerKnownLengthUnparser(
  e: ElementRuntimeData,
  override val lengthInBits: Int
) extends BinaryIntegerBaseUnparser(e)
  with HasKnownLengthInBits {

  override def runtimeDependencies = Vector()

}

class BinaryIntegerRuntimeLengthUnparser(
  val e: ElementRuntimeData,
  val lengthEv: Evaluatable[JLong],
  val lengthUnits: LengthUnits
) extends BinaryIntegerBaseUnparser(e)
  with HasRuntimeExplicitLength {

  override def runtimeDependencies = Vector(lengthEv)
}

class BinaryIntegerMinimumLengthUnparser(
  e: ElementRuntimeData,
  maybeNBits: MaybeInt
) extends BinaryIntegerBaseUnparser(e: ElementRuntimeData) {

  private val primNumeric = e.optPrimType.get.asInstanceOf[NodeInfo.PrimType.PrimNumeric]

  override def runtimeDependencies = Vector()

  override def getBitLength(s: ParseOrUnparseState): Int = {
    if (maybeNBits.isDefined) {
      // not xs:integer, so is a fixed number of bits based on the prim type
      maybeNBits.get
    } else {
      // type is xs:integer, the length is determined by the minimum number of
      // bytes needed to represent the number
      val value = getNumberToPut(s.asInstanceOf[UState])
      val len = Math.max(asBigInt(value).bitLength, 1)
      val signedLen = if (primNumeric.isSigned) len + 1 else len
      (signedLen + 7) & ~0x7 // round up to nearest multilpe of 8
    }
  }
}

class BinaryFloatUnparser(e: ElementRuntimeData) extends BinaryNumberBaseUnparser(e) {

  override def runtimeDependencies = Vector()

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

  override def runtimeDependencies = Vector()

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

  override def runtimeDependencies = Vector()

}

class BinaryDecimalRuntimeLengthUnparser(
  val e: ElementRuntimeData,
  signed: YesNo,
  binaryDecimalVirtualPoint: Int,
  val lengthEv: Evaluatable[JLong],
  val lengthUnits: LengthUnits
) extends BinaryDecimalUnparserBase(e, signed, binaryDecimalVirtualPoint)
  with HasRuntimeExplicitLength {

  override def runtimeDependencies = Vector(lengthEv)
}

class BinaryDecimalMinimumLengthUnparser(
  e: ElementRuntimeData,
  signed: YesNo,
  binaryDecimalVirtualPoint: Int
) extends BinaryDecimalUnparserBase(e, signed, binaryDecimalVirtualPoint) {

  override def runtimeDependencies = Vector()

  override def getBitLength(s: ParseOrUnparseState): Int = {
    // type is xs:decimal, the length is determined by the minimum number of
    // bytes needed to represent the number
    val value = getNumberToPut(s.asInstanceOf[UState])
    val len = Math.max(asBigInt(value).bitLength, 1)
    val signedLen = if (signed == YesNo.Yes) len + 1 else len
    (signedLen + 7) & ~0x7 // round up to nearest multilpe of 8
  }
}

abstract class BinaryDecimalUnparserBase(
  e: ElementRuntimeData,
  signed: YesNo,
  binaryDecimalVirtualPoint: Int
) extends BinaryNumberBaseUnparser(e)
  with BinaryNumberCheckWidth {

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
    val state = finfo.asInstanceOf[UState]
    val isSigned: Boolean = signed == Yes
    val minWidth: Int = if (isSigned) 2 else 1
    checkMinWidth(state, isSigned, nBits, minWidth)
    dos.putBigInt(asBigInt(value), nBits, isSigned, finfo)
  }
}
