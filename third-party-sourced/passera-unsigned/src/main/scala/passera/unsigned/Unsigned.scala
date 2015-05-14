package passera.unsigned

import scala.math.ScalaNumericAnyConversions

trait Unsigned[U <: Unsigned[U, Promoted, SignedPromoted], Promoted <: Unsigned[_, Promoted, SignedPromoted], SignedPromoted]
  extends Any with ScalaNumericAnyConversions with Serializable {

  def toByte: Byte
  def toChar: Char
  def toShort: Short
  def toInt: Int
  def toLong: Long
  def toFloat: Float
  def toDouble: Double

  def toUByte: UByte
  def toUShort: UShort
  def toUInt: UInt
  def toULong: ULong

  def byteValue: Byte
  def shortValue: Short
  def intValue: Int
  def longValue: Long
  def floatValue: Float
  def doubleValue: Double

  // Implementing ScalaNumber
  def isWhole: Boolean = true
  def underlying = this

  def +(x: SignedPromoted)(implicit d: DummyImplicit): SignedPromoted
  def -(x: SignedPromoted)(implicit d: DummyImplicit): SignedPromoted
  def *(x: SignedPromoted)(implicit d: DummyImplicit): SignedPromoted
  def /(x: SignedPromoted)(implicit d: DummyImplicit): SignedPromoted
  def %(x: SignedPromoted)(implicit d: DummyImplicit): SignedPromoted
  def &(x: SignedPromoted)(implicit d: DummyImplicit): SignedPromoted
  def |(x: SignedPromoted)(implicit d: DummyImplicit): SignedPromoted
  def ^(x: SignedPromoted)(implicit d: DummyImplicit): SignedPromoted

  def +(x: UByte): Promoted
  def -(x: UByte): Promoted
  def *(x: UByte): Promoted
  def /(x: UByte): Promoted
  def %(x: UByte): Promoted
  def &(x: UByte): Promoted
  def |(x: UByte): Promoted
  def ^(x: UByte): Promoted
  def <(x: UByte): Boolean
  def >(x: UByte): Boolean
  def <=(x: UByte): Boolean
  def >=(x: UByte): Boolean

  def +(x: UShort): Promoted
  def -(x: UShort): Promoted
  def *(x: UShort): Promoted
  def /(x: UShort): Promoted
  def %(x: UShort): Promoted
  def &(x: UShort): Promoted
  def |(x: UShort): Promoted
  def ^(x: UShort): Promoted
  def <(x: UShort): Boolean
  def >(x: UShort): Boolean
  def <=(x: UShort): Boolean
  def >=(x: UShort): Boolean

  def +(x: ULong): ULong
  def -(x: ULong): ULong
  def *(x: ULong): ULong
  def /(x: ULong): ULong
  def %(x: ULong): ULong
  def &(x: ULong): ULong
  def |(x: ULong): ULong
  def ^(x: ULong): ULong
  def <(x: ULong): Boolean
  def >(x: ULong): Boolean
  def <=(x: ULong): Boolean
  def >=(x: ULong): Boolean

  def +(x: UInt): Promoted
  def -(x: UInt): Promoted
  def *(x: UInt): Promoted
  def /(x: UInt): Promoted
  def %(x: UInt): Promoted
  def &(x: UInt): Promoted
  def |(x: UInt): Promoted
  def ^(x: UInt): Promoted
  def <(x: UInt): Boolean
  def >(x: UInt): Boolean
  def <=(x: UInt): Boolean
  def >=(x: UInt): Boolean

  def unary_+ : Promoted
  def unary_- : Promoted

  // Equality comparison to UInt is baked in

  /*
  // Override equals to allow comparison with other number types.
  // By overriding ScalaNumber, we can cause UInt.equals to be invoked when
  // comparing a number on the left with a UInt on the right.
  // This is an (undocumented?) hack and might change in the future.
  override def equals(x: Any): Boolean
  def canEqual(x: Any): Boolean
  */

  def unary_~ : Promoted

  def <<(x: Int)(implicit d: DummyImplicit): Promoted
  def >>(x: Int)(implicit d: DummyImplicit): Promoted
  def >>>(x: Int)(implicit d: DummyImplicit): Promoted
  def <<(x: Long)(implicit d: DummyImplicit): Promoted
  def >>(x: Long)(implicit d: DummyImplicit): Promoted
  def >>>(x: Long)(implicit d: DummyImplicit): Promoted
  def <<(x: UInt): Promoted
  def >>(x: UInt): Promoted
  def >>>(x: UInt): Promoted
  def <<(x: ULong): Promoted
  def >>(x: ULong): Promoted
  def >>>(x: ULong): Promoted

  override def toString: String

  def +(x: java.lang.String): String

  def toHexString: String
  def toOctalString: String
  def toBinaryString: String
}
