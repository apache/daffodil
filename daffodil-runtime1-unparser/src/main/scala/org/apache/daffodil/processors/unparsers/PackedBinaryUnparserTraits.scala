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

import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.ParseOrUnparseState
import org.apache.daffodil.util.Maybe._
import java.lang.{ Number => JNumber }
import java.math.{ BigInteger => JBigInteger, BigDecimal => JBigDecimal }
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.processors.Evaluatable

trait PackedBinaryConversion {
  def fromBigInteger(bigInt: JBigInteger, nBits: Int): Array[Byte]
}

abstract class PackedBinaryBaseUnparser(
  override val context: ElementRuntimeData)
  extends PrimUnparser
  with PackedBinaryConversion {

  override lazy val runtimeDependencies: Seq[Evaluatable[AnyRef]] = Seq()

  protected def getBitLength(s: ParseOrUnparseState): Int

  def putNumber(dos: DataOutputStream, number: JNumber, nBits: Int, finfo: FormatInfo): Boolean

  override def unparse(state: UState): Unit = {
    val nBits = getBitLength(state)
    val node = state.currentInfosetNode.asSimple
    val value = node.dataValue.asInstanceOf[JNumber]
    val dos = state.dataOutputStream

    val res = putNumber(dos, value, nBits, state)

    if (!res) {
      Assert.invariant(dos.maybeRelBitLimit0b.isDefined)
      UnparseError(One(state.schemaFileLocation), One(state.currentLocation), "Insufficient space to unparse element %s, required %s bits, but only %s were available.",
        context.dpathElementCompileInfo.namedQName.toPrettyString, nBits, dos.maybeRelBitLimit0b.get)
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

      val packedNum = fromBigInteger(bigDec.unscaledValue, nBits)
      dos.putByteArray(packedNum, packedNum.length * 8, finfo)
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

      val packedNum = fromBigInteger(bigInt, nBits)
      dos.putByteArray(packedNum, packedNum.length * 8, finfo)
  }
}
