package edu.illinois.ncsa.daffodil.processors

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.{ LengthKind, LengthUnits, ByteOrder, AlignmentUnits }
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._

trait RuntimeExplicitLengthMixin[T] {
  self: Terminal =>
  def e: ElementBase

  // get at compile time, not runtime.
  val lUnits = e.lengthUnits

  // binary numbers will use this conversion. Others won't.
  lazy val toBits = lUnits match {
    case LengthUnits.Bits => 1
    case LengthUnits.Bytes => 8
    case _ => e.schemaDefinitionError("Binary Numbers must have length units of Bits or Bytes.")
  }

  def getBitLength(s: PState): (PState, Long) = {
    val R(nBytesAsAny, newVMap) = e.length.evaluate(s.parentElement, s.variableMap, s)
    val nBytes = nBytesAsAny.asInstanceOf[Long]
    val start = s.withVariables(newVMap)
    (start, nBytes * toBits)
  }
  def getLength(s: PState): (PState, Long) = {
    val R(nBytesAsAny, newVMap) = e.length.evaluate(s.parentElement, s.variableMap, s)
    val nBytes = nBytesAsAny.asInstanceOf[Long]
    val start = s.withVariables(newVMap)
    (start, nBytes)
  }
}

trait KnownLengthInBitsMixin[T] {
  self: BinaryNumberBase[T] =>
  def len: Long
  def getBitLength(s: PState) = (s, len) // already in bits, so no multiply by 8 for this one.
}

trait RuntimeExplicitByteOrderMixin[T] {
  self: BinaryNumberBase[T] =>
  def e: ElementBase
  def getByteOrder(s: PState): (PState, java.nio.ByteOrder) = {
    val R(byteOrderAsAny, newVMap) = e.byteOrder.evaluate(s.parentElement, s.variableMap, s)
    val dfdlByteOrderEnum = ByteOrder(byteOrderAsAny.toString, e)
    val byteOrder = dfdlByteOrderEnum match {
      case ByteOrder.BigEndian => java.nio.ByteOrder.BIG_ENDIAN
      case ByteOrder.LittleEndian => java.nio.ByteOrder.LITTLE_ENDIAN
    }
    val start = s.withVariables(newVMap)
    (start, byteOrder)
  }
}

trait SignedNumberMixin[T] {
  self: BinaryNumberBase[T] =>
  def convertValue(n: BigInt, msb: Int): T = {
    val signed = n.testBit(msb - 1) match { // msb is zero-based bit counting
      case true => n - (BigInt(1) << msb)
      case false => n
    }
    signed.asInstanceOf[T]
  }
}

trait UnsignedNumberMixin[T] {
  self: BinaryNumberBase[T] =>
  def convertValue(n: BigInt, msb: Int): T = n.asInstanceOf[T]
}

// TODO: Double Conversion as a Sign-Trait

abstract class BinaryNumberBase[T](val e: ElementBase) extends Terminal(e, true) {
  val primName = e.primType.name

  val (staticJByteOrder, label) = {
    if (e.byteOrder.isConstant) {
      val staticByteOrderString = e.byteOrder.constantAsString
      val staticByteOrder = ByteOrder(staticByteOrderString, context)
      staticByteOrder match {
        case ByteOrder.BigEndian => (java.nio.ByteOrder.BIG_ENDIAN, "BE")
        case ByteOrder.LittleEndian => (java.nio.ByteOrder.LITTLE_ENDIAN, "LE")
      }
    } else (null, "Runtime")
  }

  //def getNum(t: Number): BigInt
  protected def getBitLength(s: PState): (PState, Long)
  protected def getByteOrder(s: PState): (PState, java.nio.ByteOrder)
  protected def convertValue(n: BigInt, msb: Int): T
  override def toString = "binary(xs:" + primName + ", " + label + ")"
  val gram = this

  protected val GramName = e.primType.name
  protected val GramDescription = { GramName(0).toUpper + GramName.substring(1, GramName.length) }

  val bitOrd = e.bitOrder

  def parser = new PrimParser(this, e) {
    override def toString = gram.toString

    def parse(start0: PState): PState = withParseErrorThrowing(start0) {
      try {
        val (start1, nBits) = getBitLength(start0)
        val (start, bo) = getByteOrder(start1)
        //if (start.bitLimit != -1L && (start.bitLimit - start.bitPos < nBits)) PE(start, "Not enough bits to create an xs:" + primName)

        val value = start.inStream.getBigInt(start.bitPos, nBits, bo, bitOrd)
        val newPos = start.bitPos + nBits
        //if (GramName == "hexBinary") {
        //  val bytes = value.asInstanceOf[Array[Byte]]
        //  var asString: StringBuilder = new StringBuilder()
        //  for (i <- 0 until bytes.length) {
        //    val byte = String.format("%02X", bytes(i).asInstanceOf[java.lang.Byte])
        //    asString.append(byte)
        //  }
        //  start.parentElement.setDataValue(asString.toString())
        //} else
        val convertedValue: T = convertValue(value, nBits.toInt)
        start.parentElement.setDataValue(convertValueToString(convertedValue))
        start.withPos(newPos, -1, Nope)
      } catch {
        case e: IndexOutOfBoundsException => { return PE(start0, "BinaryNumber - Insufficient Bits for xs:%s : IndexOutOfBounds: \n%s", primName, e.getMessage()) }
        case u: UnsuppressableException => throw u
        case e: Exception => { return PE(start0, "BinaryNumber - Exception: \n%s", e) }
      }
    }
  }

  def convertValueToString(n: T): String = {
    n.toString
  }

  def unparser = DummyUnparser
}

class UnsignedRuntimeLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase) extends BinaryNumberBase[T](e)
  with RuntimeExplicitLengthMixin[T] with RuntimeExplicitByteOrderMixin[T] with UnsignedNumberMixin[T] {
}

class UnsignedKnownLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase, val len: Long) extends BinaryNumberBase[T](e)
  with RuntimeExplicitByteOrderMixin[T] with KnownLengthInBitsMixin[T] with UnsignedNumberMixin[T] {
}

class SignedRuntimeLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase) extends BinaryNumberBase[T](e)
  with RuntimeExplicitLengthMixin[T] with RuntimeExplicitByteOrderMixin[T] with SignedNumberMixin[T] {
}

class SignedKnownLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase, val len: Long) extends BinaryNumberBase[T](e)
  with RuntimeExplicitByteOrderMixin[T] with KnownLengthInBitsMixin[T] with SignedNumberMixin[T] {
}

// Not needed. No runtime-determined lengths for binary floats.
//class FloatingPointRuntimeLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase) extends BinaryNumberBase[T](e)
//  with RuntimeExplicitLengthMixin[T] with RuntimeExplicitByteOrderMixin[T] with FloatingPointMixin[T] {
//}

class HexBinaryKnownLengthBinaryNumber(e: ElementBase, val len: Long)
  extends BinaryNumberBase[String](e) {
  override def toString = "hexBinary(xs:" + primName + ", " + label + ")"
  // get at compile time, not runtime.
  val lUnits = e.lengthUnits

  // binary numbers will use this conversion. Others won't.
  lazy val toBits = lUnits match {
    case LengthUnits.Bits => 1
    case LengthUnits.Bytes => 8
    case _ => e.schemaDefinitionError("Binary Numbers must have length units of Bits or Bytes.")
  }

  def getByteOrder(s: PState): (PState, java.nio.ByteOrder) = {
    (s, java.nio.ByteOrder.BIG_ENDIAN)
  }

  def getBitLength(s: PState): (PState, Long) = {
    (s, len * toBits)
  }
  def getLength(s: PState): (PState, Long) = {
    (s, len)
  }

  final def convertValue(n: BigInt, ignored_msb: Int): String = n.toString(16)
}

class HexBinaryRuntimeLengthBinaryNumber(e: ElementBase)
  extends BinaryNumberBase[String](e)
  with RuntimeExplicitLengthMixin[String] {

  override def toString = "hexBinary(xs:" + primName + ", " + label + ")"
  def getByteOrder(s: PState): (PState, java.nio.ByteOrder) = {
    (s, java.nio.ByteOrder.BIG_ENDIAN)
  }

  final def convertValue(n: BigInt, ignored_msb: Int): String = n.toString(16)
}

class FloatKnownLengthRuntimeByteOrderBinaryNumber(e: ElementBase, val len: Long)
  extends BinaryNumberBase[Float](e)
  with RuntimeExplicitByteOrderMixin[Float]
  with KnownLengthInBitsMixin[Float] {

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

class DoubleKnownLengthRuntimeByteOrderBinaryNumber(e: ElementBase, val len: Long)
  extends BinaryNumberBase[Double](e)
  with RuntimeExplicitByteOrderMixin[Double]
  with KnownLengthInBitsMixin[Double] {

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

class DecimalKnownLengthRuntimeByteOrderBinaryNumber(e: ElementBase, val len: Long)
  extends BinaryNumberBase[BigDecimal](e)
  with RuntimeExplicitByteOrderMixin[BigDecimal]
  with KnownLengthInBitsMixin[BigDecimal] {

  final def convertValue(n: BigInt, ignored_msb: Int): BigDecimal = {
    val res = BigDecimal(n, e.binaryDecimalVirtualPoint)
    res
  }

  override def convertValueToString(n: BigDecimal): String = {
    n.underlying.toPlainString
  }
}


