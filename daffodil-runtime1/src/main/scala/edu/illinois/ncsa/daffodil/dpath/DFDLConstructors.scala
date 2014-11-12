package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dpath.HexBinaryConversions._
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE

abstract class DFDLConstructorFunction(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) {

  def constructorName: String
  def maxHexDigits: Int

  lazy val nfeMsg = "%s cannot be cast to dfdl:" + constructorName + "\ndfdl:" + constructorName + " received an unrecognized type! Must be String, Byte, Short, Integer, Long or a subtype thereof."
  lazy val hexMsg = "dfdl:" + constructorName + " received string violates maximum hex digits.  Received %s expected a max of " + maxHexDigits

  protected def convert(longValue: Long, dstate: DState): Any

  override def computeValue(a: Any, dstate: DState): Any = {
    val long: Long = a match {
      case _: Byte | _: Short | _: Int => IntToLong.computeValue(a, dstate)
      case l: Long => l
      case s: String if s.startsWith("x") => {
        val hexStr = s.substring(1)
        if (hexStr.length > maxHexDigits) throw new NumberFormatException(hexMsg.format(hexStr.length))
        HexStringToLong.computeValue(hexStr, dstate)
      }
      case s: String => StringToLong.computeValue(s, dstate)
      case bi: BigInt => BigIntToLong.computeValue(bi, dstate)
      case bd: BigDecimal => IntToLong.computeValue(bd.toInt, dstate)
      case hb: Array[Byte] => {
        val str = "0x" + HexBinaryToString.computeValue(hb, dstate)
        throw new NumberFormatException(nfeMsg.format(str))
      }
      case x =>
        throw new NumberFormatException(nfeMsg.format(x))
    }
    convert(long, dstate)
  }
}

/**
 * The argument can
 * also be a long, unsignedLong, or any subtype
 * thereof, and in that case a xs:hexBinary value
 * containing a number of hex digits is produced.
 * The ordering and number of the digits
 * correspond to a binary big-endian twos-
 * complement implementation of the type of the
 * argument. Digits 0-9, A-F are used.
 * The number of digits produced depends on the
 * type of $arg, being 2, 4, 8 or 16. If $arg is a
 * literal number then the type is the smallest
 * signed type (long, int, short, byte) that can
 * contain the value.
 * If a literal number is not able to be represented
 * by a long, it is a schema definition error.
 *
 * • dfdl:hexBinary(xs:short(208)) is the hexBinary value "00D0".
 * • dfdl:hexBinary(208) is the hexBinary value "D0".
 * • dfdl:hexBinary(-2084) is the hexBinary value "F7FF".
 *
 */
case class DFDLHexBinary(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends FNOneArg(recipe, argType) with HexBinaryKind {
  val name = "DFDLHexBinary"

  lazy val nfeMsg = "%s cannot be cast to dfdl:hexBinary\ndfdl:hexBinary received an unrecognized type! Must be String, Byte, Short, Integer, Long or a subtype thereof."

  /**
   * If the argument is of some fixed-width type like Byte, Int, Short, Long or
   * the unsigned thereof, then you get hex digits corresponding to 1, 2, 4, or 8 bytes of
   * a binary twos-complement integer value of that type. If the argument is anything
   * else (including a literal number), then you get the smallest number of hex digit pairs
   * that can represent the value.  If you get xs:integer (aka BigInt/Java BigInteger), then
   * I'd say - smallest number of digits that can represent the value.
   *
   * So dfdl:hexBinary(208) is D0, dfdl:hexBinary(xs:integer(208))
   * is also D0 dfdl:hexBinary(xs:short(208)) is 00D0
   *
   */
  override def computeValue(a: Any, dstate: DState): Any = {
    val arr = a match {
      case s: String => {
        // Literal number
        reduce(s)
      }
      case b: Byte => HexBinaryConversions.toByteArray(b)
      case s: Short => HexBinaryConversions.toByteArray(s)
      case bi: BigInt => {
        // Literal number
        reduce(bi)
      }
      case i: Int => {
        // Possibly a Literal Number, try to fit it into the smallest
        // value anyway.
        reduce(i)
      }
      case l: Long => HexBinaryConversions.toByteArray(l)
      case ul: BigDecimal => {
        reduce(ul)
      }
      case hb: Array[Byte] => hb
      case x => throw new NumberFormatException(nfeMsg.format(x))
    }

    arr
  }
}

case class DFDLByte(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends DFDLConstructorFunction(recipe, argType) {

  val constructorName = "byte"
  val maxHexDigits = 2

  protected def convert(longValue: Long, dstate: DState): Any = LongToByte.computeValue(longValue, dstate)
}

case class DFDLUnsignedByte(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends DFDLConstructorFunction(recipe, argType) {
  val constructorName = "unsignedByte"
  val maxHexDigits = 2

  protected def convert(longValue: Long, dstate: DState): Any = LongToUnsignedByte.computeValue(longValue, dstate)
}

case class DFDLShort(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends DFDLConstructorFunction(recipe, argType) {
  val constructorName = "short"
  val maxHexDigits = 4

  protected def convert(longValue: Long, dstate: DState): Any = LongToShort.computeValue(longValue, dstate)
}

case class DFDLUnsignedShort(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends DFDLConstructorFunction(recipe, argType) {
  val constructorName = "unsignedShort"
  val maxHexDigits = 4

  protected def convert(longValue: Long, dstate: DState): Any = LongToUnsignedShort.computeValue(longValue, dstate)
}

case class DFDLInt(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends DFDLConstructorFunction(recipe, argType) {
  val constructorName = "int"
  val maxHexDigits = 8

  protected def convert(longValue: Long, dstate: DState): Any = LongToInt.computeValue(longValue, dstate)
}

case class DFDLUnsignedInt(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends DFDLConstructorFunction(recipe, argType) {
  val constructorName = "unsignedInt"
  val maxHexDigits = 8

  protected def convert(longValue: Long, dstate: DState): Any = LongToUnsignedInt.computeValue(longValue, dstate)
}

case class DFDLLong(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends DFDLConstructorFunction(recipe, argType) {
  val constructorName = "long"
  val maxHexDigits = 16

  protected def convert(longValue: Long, dstate: DState): Any = longValue
}

case class DFDLUnsignedLong(recipe: CompiledDPath, argType: NodeInfo.Kind)
  extends DFDLConstructorFunction(recipe, argType) {
  val constructorName = "unsignedLong"
  val maxHexDigits = 16

  protected def convert(longValue: Long, dstate: DState): Any = {}

  override def computeValue(a: Any, dstate: DState): Any = {
    val ulong: Any = a match {
      case _: Byte | _: Short | _: Int => IntegerToUnsignedLong.computeValue(a, dstate)
      case s: String if s.startsWith("x") => {
        val hexStr = s.substring(1)
        if (hexStr.length > maxHexDigits) throw new NumberFormatException(hexMsg.format(hexStr.length))
        HexStringToUnsignedLong.computeValue(hexStr, dstate)
      }
      case s: String => StringToUnsignedLong.computeValue(s, dstate)
      case bi: BigInt => IntegerToUnsignedLong.computeValue(bi, dstate)
      case bd: BigDecimal => IntegerToUnsignedLong.computeValue(bd.toInt, dstate)
      case x =>
        throw new NumberFormatException(nfeMsg.format(x))
    }
    ulong
  }

}