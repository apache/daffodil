/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

abstract class BinaryNumberBaseUnparser(e: ElementRuntimeData)
  extends PrimUnparserObject(e) {

  protected def getBitLength(s: ParseOrUnparseState): Int

  protected def putNumber(dos: DataOutputStream, number: JNumber, nBits: Int): Boolean

  def unparse(state: UState): Unit = {
    val nBits = getBitLength(state)
    val node = state.currentInfosetNode.asSimple
    val value = node.dataValue.asInstanceOf[JNumber]
    val dos = state.dataOutputStream

    val res =
      if (nBits > 0) {
        putNumber(dos, value, nBits)
      } else {
        true
      }

    if (!res) {
      Assert.invariant(dos.maybeRelBitLimit0b.isDefined)
      UnparseError(One(state.schemaFileLocation), One(state.currentLocation), "Insufficient space to unparse element %s, required %s bits, but only %s were available.",
        e.dpathElementCompileInfo.namedQName.toPrettyString, nBits, dos.maybeRelBitLimit0b.get)
    }
  }

}

abstract class BinaryIntegerBaseUnparser(e: ElementRuntimeData, signed: Boolean)
  extends BinaryNumberBaseUnparser(e) {

  override def putNumber(dos: DataOutputStream, value: JNumber, nBits: Int): Boolean = {
    if (nBits > 64) {
      dos.putBigInt(asBigInt(value), nBits, signed)
    } else {
      dos.putLong(asLong(value), nBits)
    }
  }
}

class BinaryIntegerKnownLengthUnparser(e: ElementRuntimeData, signed: Boolean, override val lengthInBits: Int)
  extends BinaryIntegerBaseUnparser(e, signed)
  with HasKnownLengthInBits {
}

class BinaryIntegerRuntimeLengthUnparser(val e: ElementRuntimeData, signed: Boolean, val lengthEv: Evaluatable[JLong], val lUnits: LengthUnits)
  extends BinaryIntegerBaseUnparser(e, signed)
  with HasRuntimeExplicitLength {

  override val runtimeDependencies = List(lengthEv)
}

class BinaryFloatUnparser(e: ElementRuntimeData)
  extends BinaryNumberBaseUnparser(e) {

  override def getBitLength(s: ParseOrUnparseState) = 32

  override def putNumber(dos: DataOutputStream, value: JNumber, nBits: Int): Boolean = {
    dos.putBinaryFloat(asFloat(value))
  }

}

class BinaryDoubleUnparser(e: ElementRuntimeData)
  extends BinaryNumberBaseUnparser(e) {

  override def getBitLength(s: ParseOrUnparseState) = 64

  override def putNumber(dos: DataOutputStream, value: JNumber, nBits: Int): Boolean = {
    dos.putBinaryDouble(asDouble(value))
  }
}

class BinaryDecimalKnownLengthUnparser(e: ElementRuntimeData, signed: YesNo, binaryDecimalVirtualPoint: Int, val lengthInBits: Int)
  extends BinaryDecimalUnparserBase(e, signed, binaryDecimalVirtualPoint)
  with HasKnownLengthInBits {
}

class BinaryDecimalRuntimeLengthUnparser(val e: ElementRuntimeData, signed: YesNo, binaryDecimalVirtualPoint: Int, val lengthEv: Evaluatable[JLong], val lUnits: LengthUnits)
  extends BinaryDecimalUnparserBase(e, signed, binaryDecimalVirtualPoint)
  with HasRuntimeExplicitLength {

  override val runtimeDependencies = List(lengthEv)
}

abstract class BinaryDecimalUnparserBase(e: ElementRuntimeData, signed: YesNo, binaryDecimalVirtualPoint: Int)
  extends BinaryNumberBaseUnparser(e) {

  override def putNumber(dos: DataOutputStream, value: JNumber, nBits: Int): Boolean = {

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

    dos.putBigInt(bigInt, nBits, signed == YesNo.Yes)

  }
}
