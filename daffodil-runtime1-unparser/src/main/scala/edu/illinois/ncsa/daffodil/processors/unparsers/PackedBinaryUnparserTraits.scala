/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.ParseOrUnparseState
import edu.illinois.ncsa.daffodil.util.Maybe._
import java.lang.{ Number => JNumber }
import java.math.{ BigInteger => JBigInteger, BigDecimal => JBigDecimal }
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.io.DataOutputStream
import edu.illinois.ncsa.daffodil.io.FormatInfo

trait PackedBinaryConversion {
  def fromBigInteger(bigInt: JBigInteger, nBits: Int): Array[Byte]
}

abstract class PackedBinaryBaseUnparser(
  e: ElementRuntimeData)
  extends PrimUnparserObject(e)
  with PackedBinaryConversion {

  protected def getBitLength(s: ParseOrUnparseState): Int

  def putNumber(dos: DataOutputStream, number: JNumber, nBits: Int, finfo: FormatInfo): Boolean

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
        e.dpathElementCompileInfo.namedQName.toPrettyString, nBits, dos.maybeRelBitLimit0b.get)
    }
  }

}

abstract class PackedBinaryDecimalBaseUnparser(
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int)
  extends PackedBinaryBaseUnparser(e) {

    override def putNumber(dos: DataOutputStream, number: JNumber, nBits: Int, finfo: FormatInfo): Boolean = {

      val bigDec = number.asInstanceOf[JBigDecimal]
      if (bigDec.movePointRight(binaryDecimalVirtualPoint).scale != 0)
        e.schemaDefinitionError("Decimal point of number '%s' does not match the binaryVirtualDecmialPoint: %d", bigDec, binaryDecimalVirtualPoint)

      dos.putByteArray(
        fromBigInteger(bigDec.unscaledValue, nBits),
        nBits,
        finfo)
    }
}


abstract class PackedBinaryIntegerBaseUnparser(
  e: ElementRuntimeData)
  extends PackedBinaryBaseUnparser(e) {

    override def putNumber(dos: DataOutputStream, number: JNumber, nBits: Int, finfo: FormatInfo): Boolean = {

      val bigInt = number.isInstanceOf[JBigInteger] match {
        case true => number.asInstanceOf[JBigInteger]
        case false => new JBigInteger(number.toString)
      }

      dos.putByteArray(
        fromBigInteger(bigInt, nBits),
        nBits,
        finfo)
    }
}
