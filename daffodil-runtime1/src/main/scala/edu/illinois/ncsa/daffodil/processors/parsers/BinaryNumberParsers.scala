/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnits
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.util.Misc

class FloatKnownLengthRuntimeByteOrderBinaryNumberParser(
  val bo: CompiledExpression,
  val len: Long,
  e: ElementRuntimeData)
  extends BinaryNumberBaseParser[Float](e)
  with HasRuntimeExplicitByteOrder[Float]
  with HasKnownLengthInBits[Float] {

  final def convertValue(n: BigInt, ignored_msb: Int): Float = {
    val nWith33rdBit = n | (BigInt(1) << 33) // make sure we have 5 bytes here. Then we'll ignore 5th byte.
    val ba = nWith33rdBit.toByteArray
    val bb = java.nio.ByteBuffer.wrap(ba)
    val res = ba.length match {
      case 5 => bb.getFloat(1)
      case _ => Assert.invariantFailed("byte array should be 5 long")
    }
    res
  }
}

class DoubleKnownLengthRuntimeByteOrderBinaryNumberParser(
  val bo: CompiledExpression,
  val len: Long,
  e: ElementRuntimeData)
  extends BinaryNumberBaseParser[Double](e)
  with HasRuntimeExplicitByteOrder[Double]
  with HasKnownLengthInBits[Double] {

  final def convertValue(n: BigInt, ignored_msb: Int): Double = {
    val nWith65thBit = n | (BigInt(1) << 65) // make sure we have 9 bytes of bigint here. Then we'll ignore the 9th byte.
    val ba = nWith65thBit.toByteArray
    val bb = java.nio.ByteBuffer.wrap(ba)
    val res = ba.length match {
      case 9 => bb.getDouble(1) // ignore first byte.
      case _ => Assert.invariantFailed("byte array should be 9 long")
    }
    res
  }
}

class DecimalKnownLengthRuntimeByteOrderBinaryNumberParser(
  val bo: CompiledExpression,
  val len: Long,
  e: ElementRuntimeData,
  binaryDecimalVirtualPoint: Int)
  extends BinaryNumberBaseParser[BigDecimal](e)
  with HasRuntimeExplicitByteOrder[BigDecimal]
  with HasKnownLengthInBits[BigDecimal] {

  final def convertValue(n: BigInt, ignored_msb: Int): BigDecimal = {
    val res = BigDecimal(n, binaryDecimalVirtualPoint)
    res
  }

  override def convertValueToString(n: BigDecimal): String = {
    n.underlying.toPlainString
  }
}
class HexBinaryRuntimeLengthBinaryNumberParser(
  val lUnits: LengthUnits,
  override val length: CompiledExpression,
  override val e: ElementRuntimeData)
  extends BinaryNumberBaseParser[String](e)
  with HasRuntimeExplicitLength[String] {

  def getByteOrder(s: PState): java.nio.ByteOrder = {
    java.nio.ByteOrder.BIG_ENDIAN
  }

  final def convertValue(n: BigInt, ignored_msb: Int): String = n.toString(16)
}

class HexBinaryKnownLengthBinaryNumberParser(
  val len: Long,
  e: ElementRuntimeData,
  lUnits: LengthUnits)
  extends BinaryNumberBaseParser[String](e) {

  // binary numbers will use this conversion. Others won't.
  val toBits = lUnits match {
    case LengthUnits.Bits => 1
    case LengthUnits.Bytes => 8
    case _ => e.schemaDefinitionError("Binary Numbers must have length units of Bits or Bytes.")
  }

  def getByteOrder(s: PState): java.nio.ByteOrder = {
    java.nio.ByteOrder.BIG_ENDIAN
  }

  def getBitLength(s: PState): Long = {
    len * toBits
  }
  def getLength(s: PState): Long = {
    len
  }

  final def convertValue(n: BigInt, ignored_msb: Int): String = n.toString(16)
}

class UnsignedRuntimeLengthRuntimeByteOrderBinaryNumberParser[T](
  val bo: CompiledExpression,
  val lUnits: LengthUnits,
  override val length: CompiledExpression,
  e: ElementRuntimeData)
  extends BinaryNumberBaseParser[T](e)
  with HasRuntimeExplicitLength[T]
  with HasRuntimeExplicitByteOrder[T]
  with HasUnsignedNumber[T] {
}

class UnsignedKnownLengthRuntimeByteOrderBinaryNumberParser[T](
  val bo: CompiledExpression,
  val len: Long,
  e: ElementRuntimeData)
  extends BinaryNumberBaseParser[T](e)
  with HasRuntimeExplicitByteOrder[T]
  with HasKnownLengthInBits[T]
  with HasUnsignedNumber[T] {
}

class SignedRuntimeLengthRuntimeByteOrderBinaryNumberParser[T](
  val bo: CompiledExpression,
  val lUnits: LengthUnits,
  override val length: CompiledExpression,
  e: ElementRuntimeData)
  extends BinaryNumberBaseParser[T](e)
  with HasRuntimeExplicitLength[T]
  with HasRuntimeExplicitByteOrder[T]
  with HasSignedNumber[T] {
}

class SignedKnownLengthRuntimeByteOrderBinaryNumberParser[T](
  val bo: CompiledExpression,
  val len: Long,
  e: ElementRuntimeData)
  extends BinaryNumberBaseParser[T](e)
  with HasRuntimeExplicitByteOrder[T]
  with HasKnownLengthInBits[T]
  with HasSignedNumber[T] {
}

abstract class BinaryNumberBaseParser[T](
  val e: ElementRuntimeData)
  extends PrimParser(e) {
  val primName = e.optPrimType.get.name
  override lazy val childProcessors = Nil

  val bitOrd = e.defaultBitOrder

  protected def getBitLength(s: PState): Long
  protected def getByteOrder(s: PState): java.nio.ByteOrder
  protected def convertValue(n: BigInt, msb: Int): T

  override def toString = Misc.getNameFromClass(this) // e.prettyName

  def convertValueToString(n: T): String = {
    n.toString
  }

  def parse(start: PState): Unit = withParseErrorThrowing(start) {
    try {
      val nBits = getBitLength(start)
      val bo = getByteOrder(start)
      if (start.bitLimit0b != -1L && (start.bitLimit0b - start.bitPos0b < nBits)) {
        PE(start, "Insufficient bits to create an xs:" + primName)
        return
      }
      val value = start.inStream.getBigInt(start.bitPos, nBits, bo, bitOrd)
      val newPos = start.bitPos + nBits
      val convertedValue: T = convertValue(value, nBits.toInt)
      start.simpleElement.setDataValue(convertedValue)
      start.setPos(newPos, -1, Nope)
    } catch {
      /*
       * The reason we catch IndexOutOfBounds is that is the exception 
       * thrown when indexing into the data stream and you reach past the end.
       */
      case e: IndexOutOfBoundsException => {
        PE(start, "BinaryNumber - Insufficient Bits for xs:%s : IndexOutOfBounds: \n%s", primName, e.getMessage())
      }
    }
  }

}
