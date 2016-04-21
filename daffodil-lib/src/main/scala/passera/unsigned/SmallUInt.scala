package passera.unsigned


/**
 * Supertrait of UByte, UShort, UInt
 */

trait SmallUInt[U <: Unsigned[U, UInt, Int]] extends Any with Unsigned[U, UInt, Int] with Serializable {
  override def toByte = intValue.toByte
  override def toShort = intValue.toShort
  override def toInt = intValue
  override def toLong = intValue & 0xffffffffL
  override def toFloat = intValue.toFloat
  override def toDouble = intValue.toDouble
  override def toChar = intValue.toChar
  def toBigInt = BigInt(intValue)

  def toUByte = UByte(intValue.toByte)
  def toUShort = UShort(intValue.toShort)
  def toUInt = UInt(intValue)
  def toULong = ULong(intValue & 0xffffffffL)

  override def byteValue = intValue.toByte
  override def shortValue = intValue.toShort
  override def intValue: Int
  override def longValue = (intValue & 0xffffffffL)
  override def floatValue = (intValue & 0xffffffffL).toFloat
  override def doubleValue = (intValue & 0xffffffffL).toDouble

  def +(x: Int)(implicit d: DummyImplicit): Int = this.toInt + x
  def -(x: Int)(implicit d: DummyImplicit): Int = this.toInt - x
  def *(x: Int)(implicit d: DummyImplicit): Int = this.toInt * x
  def /(x: Int)(implicit d: DummyImplicit): Int = this.toInt / x
  def %(x: Int)(implicit d: DummyImplicit): Int = this.toInt % x
  def &(x: Int)(implicit d: DummyImplicit): Int = this.toInt & x
  def |(x: Int)(implicit d: DummyImplicit): Int = this.toInt | x
  def ^(x: Int)(implicit d: DummyImplicit): Int = this.toInt ^ x

  def +(x: Long)(implicit d: DummyImplicit): Long = this.toLong + x
  def -(x: Long)(implicit d: DummyImplicit): Long = this.toLong - x
  def *(x: Long)(implicit d: DummyImplicit): Long = this.toLong * x
  def /(x: Long)(implicit d: DummyImplicit): Long = this.toLong / x
  def %(x: Long)(implicit d: DummyImplicit): Long = this.toLong % x
  def &(x: Long)(implicit d: DummyImplicit): Long = this.toLong & x
  def |(x: Long)(implicit d: DummyImplicit): Long = this.toLong | x
  def ^(x: Long)(implicit d: DummyImplicit): Long = this.toLong ^ x

  def +(x: UByte): UInt = this + x.toUInt
  def -(x: UByte): UInt = this - x.toUInt
  def *(x: UByte): UInt = this * x.toUInt
  def /(x: UByte): UInt = this / x.toUInt
  def %(x: UByte): UInt = this % x.toUInt
  def &(x: UByte): UInt = this & x.toUInt
  def |(x: UByte): UInt = this | x.toUInt
  def ^(x: UByte): UInt = this ^ x.toUInt
  def <(x: UByte): Boolean = this < x.toUInt
  def >(x: UByte): Boolean = this > x.toUInt
  def <=(x: UByte): Boolean = this <= x.toUInt
  def >=(x: UByte): Boolean = this >= x.toUInt

  def +(x: UShort): UInt = this + x.toUInt
  def -(x: UShort): UInt = this - x.toUInt
  def *(x: UShort): UInt = this * x.toUInt
  def /(x: UShort): UInt = this / x.toUInt
  def %(x: UShort): UInt = this % x.toUInt
  def &(x: UShort): UInt = this & x.toUInt
  def |(x: UShort): UInt = this | x.toUInt
  def ^(x: UShort): UInt = this ^ x.toUInt
  def <(x: UShort): Boolean = this < x.toUInt
  def >(x: UShort): Boolean = this > x.toUInt
  def <=(x: UShort): Boolean = this <= x.toUInt
  def >=(x: UShort): Boolean = this >= x.toUInt

  def +(x: ULong): ULong = this.toULong + x
  def -(x: ULong): ULong = this.toULong - x
  def *(x: ULong): ULong = this.toULong * x
  def /(x: ULong): ULong = this.toULong / x
  def %(x: ULong): ULong = this.toULong % x
  def &(x: ULong): ULong = this.toULong & x
  def |(x: ULong): ULong = this.toULong | x
  def ^(x: ULong): ULong = this.toULong ^ x
  def <(x: ULong): Boolean = this.toULong < x
  def >(x: ULong): Boolean = this.toULong > x
  def <=(x: ULong): Boolean = this.toULong <= x
  def >=(x: ULong): Boolean = this.toULong >= x

  def +(x: UInt) = UInt(intValue + x.intValue)
  def -(x: UInt) = UInt(intValue - x.intValue)
  def *(x: UInt) = UInt(intValue * x.intValue)

  def /(x: UInt) = {
    val n = intValue & 0xffffffffL
    val m = x.intValue & 0xffffffffL
    val r = n / m
    UInt(r.toInt)
  }

  def %(x: UInt) = {
    val n = intValue & 0xffffffffL
    val m = x.intValue & 0xffffffffL
    val r = n % m
    UInt(r.toInt)
  }

  def unary_+ = this.toUInt
  def unary_- = UInt(-intValue) // maybe just -intValue ??

  // Equality comparison to UInt is baked in

  def ==(x: Int)(implicit d: DummyImplicit) = intValue == x
  def ==(x: Long)(implicit d: DummyImplicit) = longValue == x
  // def ==(x: UInt) = longValue == x.longValue
  def ==(x: ULong) = longValue == x.longValue
  def ==(x: Float) = floatValue == x
  def ==(x: Double) = doubleValue == x

  def !=(x: Int)(implicit d: DummyImplicit) = intValue != x
  def !=(x: Long)(implicit d: DummyImplicit) = longValue != x
  // def !=(x: UInt) = longValue != x.longValue
  def !=(x: ULong) = longValue != x.longValue
  def !=(x: Float) = floatValue != x
  def !=(x: Double) = doubleValue != x

  /*
  // Override equals to allow comparison with other number types.
  // By overriding ScalaNumber, we can cause UInt.equals to be invoked when
  // comparing a number on the left with a UInt on the right.
  // This is an (undocumented?) hack and might change in the future.
  override def equals(x: Any) = x match {
    case x: SmallUInt[_] => this.toInt == x.intValue
    case x: ULong => this.toULong == x
    case x: Number => this.longValue == x.longValue && x.longValue >= 0
    case _ => false
  }
  def canEqual(x: Any) = x match {
    case _: SmallUInt[_] => true
    case _: ULong => true
    case _: Number => true
    case _ => false
  }
  */

  private def rot(x: Int) = (x + Int.MinValue)

  def <(x: UInt) = rot(intValue) < rot(x.intValue)
  def >(x: UInt) = rot(intValue) > rot(x.intValue)
  def <=(x: UInt) = rot(intValue) <= rot(x.intValue)
  def >=(x: UInt) = rot(intValue) >= rot(x.intValue)

  def &(x: UInt) = UInt(intValue & x.intValue)
  def |(x: UInt) = UInt(intValue | x.intValue)
  def ^(x: UInt) = UInt(intValue ^ x.intValue)

  def unary_~ = UInt(~intValue)

  def <<(x: Int)(implicit d: DummyImplicit) = UInt(intValue << x)
  def <<(x: Long)(implicit d: DummyImplicit) = UInt(intValue << x)
  def <<(x: UInt) = UInt(intValue << (x.toInt & 0x1f))
  def <<(x: ULong) = UInt(intValue << (x.toLong & 0x1f))

  def >>(x: Int)(implicit d: DummyImplicit) = UInt(intValue >>> x)
  def >>(x: Long)(implicit d: DummyImplicit) = UInt(intValue >>> x)
  def >>(x: UInt) = UInt(intValue >>> (x.toInt & 0x1f))
  def >>(x: ULong) = UInt(intValue >>> (x.toLong & 0x1f))

  def >>>(x: Int)(implicit d: DummyImplicit) = UInt(intValue >>> x)
  def >>>(x: Long)(implicit d: DummyImplicit) = UInt(intValue >>> x)
  def >>>(x: UInt) = UInt(intValue >>> (x.toInt & 0x1f))
  def >>>(x: ULong) = UInt(intValue >>> (x.toLong & 0x1f))

  override def toString = (intValue & 0xffffffffL).toString

  def +(x: java.lang.String) = this.toString + x

  def toHexString = (intValue & 0xffffffffL).toHexString
  def toOctalString = (intValue & 0xffffffffL).toOctalString
  def toBinaryString = (intValue & 0xffffffffL).toBinaryString
}
