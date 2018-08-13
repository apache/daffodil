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

package org.apache.daffodil.processors.unparsers

import org.apache.daffodil.processors.parsers.HasKnownLengthInBits
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.util.Maybe._
import java.lang.{ Number => JNumber, Long => JLong }
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.parsers.HasRuntimeExplicitLength
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.schema.annotation.props.gen.YesNo
import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.util.Numbers._
import org.apache.daffodil.io.FormatInfo

abstract class BinaryNumberBaseUnparser(override val context: ElementRuntimeData)
  extends PrimUnparser {

  protected def getBitLength(s: ParseOrUnparseState): Int

  protected def putNumber(dos: DataOutputStream, number: JNumber, nBits: Int, finfo: FormatInfo): Boolean

  def unparse(state: UState): Unit = {
    val nBits = getBitLength(state)
    val node = state.currentInfosetNode.asSimple
    val value = node.dataValue.asInstanceOf[JNumber]
    val dos = state.dataOutputStream

    val res =
      if (nBits > 0) {
        putNumber(dos, value, nBits, state)
      } else {
        true
      }

    if (!res) {
      Assert.invariant(dos.maybeRelBitLimit0b.isDefined)
      UnparseError(One(state.schemaFileLocation), One(state.currentLocation), "Insufficient space to unparse element %s, required %s bits, but only %s were available.",
        context.dpathElementCompileInfo.namedQName.toPrettyString, nBits, dos.maybeRelBitLimit0b.get)
    }
  }

}

abstract class BinaryIntegerBaseUnparser(e: ElementRuntimeData, signed: Boolean)
  extends BinaryNumberBaseUnparser(e) {

  override def putNumber(dos: DataOutputStream, value: JNumber, nBits: Int, finfo: FormatInfo): Boolean = {
    if (nBits > 64) {
      dos.putBigInt(asBigInt(value), nBits, signed, finfo)
    } else {
      dos.putLong(asLong(value), nBits, finfo)
    }
  }
}

class BinaryIntegerKnownLengthUnparser(e: ElementRuntimeData, signed: Boolean, override val lengthInBits: Int)
  extends BinaryIntegerBaseUnparser(e, signed)
  with HasKnownLengthInBits {

  override lazy val runtimeDependencies = Vector()

}

class BinaryIntegerRuntimeLengthUnparser(val e: ElementRuntimeData, signed: Boolean, val lengthEv: Evaluatable[JLong], val lUnits: LengthUnits)
  extends BinaryIntegerBaseUnparser(e, signed)
  with HasRuntimeExplicitLength {

  override val runtimeDependencies = Vector(lengthEv)
}

class BinaryFloatUnparser(e: ElementRuntimeData)
  extends BinaryNumberBaseUnparser(e) {

  override lazy val runtimeDependencies = Vector()

  override def getBitLength(s: ParseOrUnparseState) = 32

  override def putNumber(dos: DataOutputStream, value: JNumber, nBits: Int, finfo: FormatInfo): Boolean = {
    dos.putBinaryFloat(asFloat(value), finfo)
  }

}

class BinaryDoubleUnparser(e: ElementRuntimeData)
  extends BinaryNumberBaseUnparser(e) {

  override lazy val runtimeDependencies = Vector()

  override def getBitLength(s: ParseOrUnparseState) = 64

  override def putNumber(dos: DataOutputStream, value: JNumber, nBits: Int, finfo: FormatInfo): Boolean = {
    dos.putBinaryDouble(asDouble(value), finfo)
  }
}

class BinaryDecimalKnownLengthUnparser(e: ElementRuntimeData, signed: YesNo, binaryDecimalVirtualPoint: Int, val lengthInBits: Int)
  extends BinaryDecimalUnparserBase(e, signed, binaryDecimalVirtualPoint)
  with HasKnownLengthInBits {

  override lazy val runtimeDependencies = Vector()

}

class BinaryDecimalRuntimeLengthUnparser(val e: ElementRuntimeData, signed: YesNo, binaryDecimalVirtualPoint: Int, val lengthEv: Evaluatable[JLong], val lUnits: LengthUnits)
  extends BinaryDecimalUnparserBase(e, signed, binaryDecimalVirtualPoint)
  with HasRuntimeExplicitLength {

  override val runtimeDependencies = Vector(lengthEv)
}

abstract class BinaryDecimalUnparserBase(e: ElementRuntimeData, signed: YesNo, binaryDecimalVirtualPoint: Int)
  extends BinaryNumberBaseUnparser(e) {

  override def putNumber(dos: DataOutputStream, value: JNumber, nBits: Int, finfo: FormatInfo): Boolean = {

    // We want to scale the bigInt by binaryDecimalVirtualPoint so that it is a BigInt
    val bigDec = asBigDecimal(value)
    //
    // NOTE: The code below would fail when using java.math.BigDecimal because of an ArithmeticException
    // due to the pow() call.  Adding the MathContext.DECIMAL128 fixed this issue.  This is the
    // defaultMathContext for scala's BigDecimal.  It's interesting to note that MathContext.UNLIMITED
    // also failed here.
    //
    val bigInt =
      if (binaryDecimalVirtualPoint != 0) bigDec.scaleByPowerOfTen(binaryDecimalVirtualPoint).toBigInteger()
      else bigDec.toBigInteger()

    dos.putBigInt(bigInt, nBits, signed == YesNo.Yes, finfo)

  }
}
