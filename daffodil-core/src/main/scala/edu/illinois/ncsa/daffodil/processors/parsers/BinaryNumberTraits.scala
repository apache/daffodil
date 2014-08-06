package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.dsom.R
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnits
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData

trait HasRuntimeExplicitLength[T] { self: BinaryNumberBaseParser[T] =>
  def e: ElementRuntimeData
  def lUnits: LengthUnits // get at compile time, not runtime.
  def length: CompiledExpression

  // binary numbers will use this conversion. Others won't.
  lazy val toBits = lUnits match {
    case LengthUnits.Bits => 1
    case LengthUnits.Bytes => 8
    case _ => e.schemaDefinitionError("Binary Numbers must have length units of Bits or Bytes.")
  }

  def getBitLength(s: PState): (PState, Long) = {
    val R(nBytesAsAny, newVMap) = length.evaluate(s.parentElement, s.variableMap, s)
    val nBytes = nBytesAsAny.asInstanceOf[Long]
    val start = s.withVariables(newVMap)
    (start, nBytes * toBits)
  }
  def getLength(s: PState): (PState, Long) = {
    val R(nBytesAsAny, newVMap) = length.evaluate(s.parentElement, s.variableMap, s)
    val nBytes = nBytesAsAny.asInstanceOf[Long]
    val start = s.withVariables(newVMap)
    (start, nBytes)
  }
}

trait HasRuntimeExplicitByteOrder[T] { self: BinaryNumberBaseParser[T] =>
  def e: ElementRuntimeData
  def bo: CompiledExpression // ensure byteOrder compiled expression is computed non lazily at compile time

  def getByteOrder(s: PState): (PState, java.nio.ByteOrder) = {
    val R(byteOrderAsAny, newVMap) = bo.evaluate(s.parentElement, s.variableMap, s)
    val dfdlByteOrderEnum = ByteOrder(byteOrderAsAny.toString, s)
    val byteOrder = dfdlByteOrderEnum match {
      case ByteOrder.BigEndian => java.nio.ByteOrder.BIG_ENDIAN
      case ByteOrder.LittleEndian => java.nio.ByteOrder.LITTLE_ENDIAN
    }
    val start = s.withVariables(newVMap)
    (start, byteOrder)
  }
}

trait HasSignedNumber[T] {
  self: BinaryNumberBaseParser[T] =>
  def convertValue(n: BigInt, msb: Int): T = {
    val signed = n.testBit(msb - 1) match { // msb is zero-based bit counting
      case true => n - (BigInt(1) << msb)
      case false => n
    }
    signed.asInstanceOf[T]
  }
}

trait HasUnsignedNumber[T] { self: BinaryNumberBaseParser[T] =>
  def convertValue(n: BigInt, msb: Int): T = n.asInstanceOf[T]
}

trait HasKnownLengthInBits[T] {
  self: BinaryNumberBaseParser[T] =>
  def len: Long
  def getBitLength(s: PState) = (s, len) // already in bits, so no multiply by 8 for this one.
}
