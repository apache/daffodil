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

import java.lang.{ Number => JNumber }
import java.math.{ BigDecimal => JBigDecimal }
import java.math.{ BigInteger => JBigInteger }

import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitiveNullable
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Evaluatable
import org.apache.daffodil.runtime1.processors.ParseOrUnparseState
import org.apache.daffodil.runtime1.processors.unparsers._

trait PackedBinaryConversion {
  def fromBigInteger(bigInt: JBigInteger, nBits: Int): Array[Byte]
}

abstract class PackedBinaryBaseUnparser(override val context: ElementRuntimeData)
  extends PrimUnparser
  with PackedBinaryConversion {

  override def runtimeDependencies: Vector[Evaluatable[AnyRef]] = Vector()

  protected def getBitLength(s: ParseOrUnparseState): Int

  def putNumber(dos: DataOutputStream, number: JNumber, nBits: Int, finfo: FormatInfo): Boolean

  def getNumberToPut(state: UState): JNumber = {
    val node = state.currentInfosetNode.asSimple

    // Packed decimal calendars use the convert combinator which sets the string value of the calendar
    //   - using dataValue would give the Calendar value rather than that string. Since the Calendar value
    //   cannot be cast as a JNumber we need to use dataValueAsString and convert it to a JBigInteger.
    //   With packed numbers, dataValue is already a number so just use that.
    val nodeValue: DataValuePrimitiveNullable =
      node.erd.optPrimType.get match {
        case NodeInfo.Date | NodeInfo.DateTime | NodeInfo.Time =>
          new JBigInteger(node.dataValueAsString)
        case _ => node.dataValue
      }

    val value = nodeValue.getNumber
    value
  }

  override def unparse(state: UState): Unit = {
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

abstract class PackedBinaryDecimalBaseUnparser(
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int
) extends PackedBinaryBaseUnparser(e) {

  override def getNumberToPut(ustate: UState): JNumber = {
    val number = super.getNumberToPut(ustate)
    val bigDec = number.asInstanceOf[JBigDecimal]
    if (bigDec.movePointRight(binaryDecimalVirtualPoint).scale != 0) {
      e.schemaDefinitionError(
        "Decimal point of number '%s' does not match the binaryVirtualDecmialPoint: %d",
        bigDec,
        binaryDecimalVirtualPoint
      )
    }
    bigDec.unscaledValue
  }

  override def putNumber(
    dos: DataOutputStream,
    number: JNumber,
    nBits: Int,
    finfo: FormatInfo
  ): Boolean = {
    val packedNum = fromBigInteger(number.asInstanceOf[JBigInteger], nBits)
    dos.putByteArray(packedNum, packedNum.length * 8, finfo)
  }
}

abstract class PackedBinaryIntegerBaseUnparser(e: ElementRuntimeData)
  extends PackedBinaryBaseUnparser(e) {

  override def getNumberToPut(ustate: UState): JNumber = {
    val number = super.getNumberToPut(ustate)
    val bigInt = number.isInstanceOf[JBigInteger] match {
      case true => number.asInstanceOf[JBigInteger]
      case false => new JBigInteger(number.toString)
    }
    bigInt
  }

  override def putNumber(
    dos: DataOutputStream,
    number: JNumber,
    nBits: Int,
    finfo: FormatInfo
  ): Boolean = {
    val packedNum = fromBigInteger(number.asInstanceOf[JBigInteger], nBits)
    dos.putByteArray(packedNum, packedNum.length * 8, finfo)
  }
}
