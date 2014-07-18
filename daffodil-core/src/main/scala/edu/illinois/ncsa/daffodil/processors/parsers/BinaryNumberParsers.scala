package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnits
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression

class FloatKnownLengthRuntimeByteOrderBinaryNumberParser(
  val bo: CompiledExpression,
  val len: Long,
  gram: Gram,
  e: ElementBase)
  extends BinaryNumberBaseParser[Float](gram, e)
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
  gram: Gram,
  e: ElementBase)
  extends BinaryNumberBaseParser[Double](gram, e)
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
  gram: Gram,
  e: ElementBase)
  extends BinaryNumberBaseParser[BigDecimal](gram, e)
  with HasRuntimeExplicitByteOrder[BigDecimal]
  with HasKnownLengthInBits[BigDecimal] {

  final def convertValue(n: BigInt, ignored_msb: Int): BigDecimal = {
    val res = BigDecimal(n, e.binaryDecimalVirtualPoint)
    res
  }

  override def convertValueToString(n: BigDecimal): String = {
    n.underlying.toPlainString
  }
}
class HexBinaryRuntimeLengthBinaryNumberParser(
  val lUnits: LengthUnits,
  gram: Gram,
  e: ElementBase)
  extends BinaryNumberBaseParser[String](gram, e)
  with HasRuntimeExplicitLength[String] {

  def getByteOrder(s: PState): (PState, java.nio.ByteOrder) = {
    (s, java.nio.ByteOrder.BIG_ENDIAN)
  }

  final def convertValue(n: BigInt, ignored_msb: Int): String = n.toString(16)
}

class HexBinaryKnownLengthBinaryNumberParser(
  val len: Long,
  gram: Gram,
  e: ElementBase)
  extends BinaryNumberBaseParser[String](gram, e) {

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

class UnsignedRuntimeLengthRuntimeByteOrderBinaryNumberParser[T](
  val bo: CompiledExpression,
  val lUnits: LengthUnits,
  gram: Gram,
  e: ElementBase)
  extends BinaryNumberBaseParser[T](gram, e)
  with HasRuntimeExplicitLength[T]
  with HasRuntimeExplicitByteOrder[T]
  with HasUnsignedNumber[T] {
}

class UnsignedKnownLengthRuntimeByteOrderBinaryNumberParser[T](
  val bo: CompiledExpression,
  val len: Long,
  gram: Gram,
  e: ElementBase)
  extends BinaryNumberBaseParser[T](gram, e)
  with HasRuntimeExplicitByteOrder[T]
  with HasKnownLengthInBits[T]
  with HasUnsignedNumber[T] {
}

class SignedRuntimeLengthRuntimeByteOrderBinaryNumberParser[T](
  val bo: CompiledExpression,
  val lUnits: LengthUnits,
  gram: Gram,
  e: ElementBase)
  extends BinaryNumberBaseParser[T](gram, e)
  with HasRuntimeExplicitLength[T]
  with HasRuntimeExplicitByteOrder[T]
  with HasSignedNumber[T] {
}

class SignedKnownLengthRuntimeByteOrderBinaryNumberParser[T](
  val bo: CompiledExpression,
  val len: Long,
  gram: Gram,
  e: ElementBase)
  extends BinaryNumberBaseParser[T](gram, e)
  with HasRuntimeExplicitByteOrder[T]
  with HasKnownLengthInBits[T]
  with HasSignedNumber[T] {
}

abstract class BinaryNumberBaseParser[T](
  gram: Gram,
  val e: ElementBase)
  extends PrimParser(gram, e) {
  val primName = e.primType.name
  val bitOrd = e.bitOrder

  protected def getBitLength(s: PState): (PState, Long)
  protected def getByteOrder(s: PState): (PState, java.nio.ByteOrder)
  protected def convertValue(n: BigInt, msb: Int): T

  override def toString = gram.toString

  def convertValueToString(n: T): String = {
    n.toString
  }

  def parse(start0: PState): PState = withParseErrorThrowing(start0) {
    try {
      val (start1, nBits) = getBitLength(start0)
      val (start, bo) = getByteOrder(start1)
      if (start.bitLimit0b != -1L && (start.bitLimit0b - start.bitPos0b < nBits)) {
        return PE(start, "Insufficient bits to create an xs:" + primName)
      }
      val value = start.inStream.getBigInt(start.bitPos, nBits, bo, bitOrd)
      val newPos = start.bitPos + nBits
      val convertedValue: T = convertValue(value, nBits.toInt)
      start.parentElement.setDataValue(convertValueToString(convertedValue))
      start.withPos(newPos, -1, Nope)
    } catch {
      case e: IndexOutOfBoundsException => {
        return PE(start0, "BinaryNumber - Insufficient Bits for xs:%s : IndexOutOfBounds: \n%s", primName, e.getMessage())
      }
      case u: UnsuppressableException => throw u
      case e: Exception => { return PE(start0, "BinaryNumber - Exception: \n%s", e) }
    }
  }

}