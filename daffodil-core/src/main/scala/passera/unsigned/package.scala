/*
 * Copyright (c) 2011-2013, Nate Nystrom
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 *
 * Redistributions in binary form must reproduce the above copyright notice, this
 * list of conditions and the following disclaimer in the documentation and/or
 * other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package passera

import scala.language.implicitConversions

package object unsigned {
  implicit def ubyte2uint(x: UByte): UInt = UInt(x.toInt)
  implicit def ushort2uint(x: UShort): UInt = UInt(x.toInt)
  implicit def ubyte2ulong(x: UByte): ULong = ULong(x.toLong)
  implicit def ushort2ulong(x: UShort): ULong = ULong(x.toLong)
  implicit def uint2ulong(x: UInt): ULong = ULong(x.toLong)

  implicit def signedIntOps(x: Int): SignedIntOps = new SignedIntOps(x)
  implicit def signedLongOps(x: Long): SignedLongOps = new SignedLongOps(x)
  implicit def floatOps(x: Float): FloatOps = new FloatOps(x)
  implicit def doubleOps(x: Double): DoubleOps = new DoubleOps(x)
  implicit def signedRichIntOps(x: scala.runtime.RichInt): SignedRichIntOps =
    new SignedRichIntOps(
      x.self.asInstanceOf[Int]
    )
  implicit def richUInt(x: UInt): RichUInt = new RichUInt(x)
  implicit def richerUInt(x: UInt): RicherUInt = new RicherUInt(x.toInt)

  class FloatOps(x: Float) {
    def toUByte = UByte(x.toByte)
    def toUShort = UShort(x.toShort)
    def toUInt = UInt(x.toInt)
    def toULong = ULong(x.toLong)
    def +(y: UInt) = x + y.toFloat
    def -(y: UInt) = x - y.toFloat
    def *(y: UInt) = x * y.toFloat
    def /(y: UInt) = x / y.toFloat
    def %(y: UInt) = x % y.toFloat
  }

  class DoubleOps(x: Double) {
    def toUByte = UByte(x.toByte)
    def toUShort = UShort(x.toShort)
    def toUInt = UInt(x.toInt)
    def toULong = ULong(x.toLong)
    def +(y: UInt) = x + y.toDouble
    def -(y: UInt) = x - y.toDouble
    def *(y: UInt) = x * y.toDouble
    def /(y: UInt) = x / y.toDouble
    def %(y: UInt) = x % y.toDouble
  }

  class SignedIntOps(x: Int) {
    def toUByte = UByte((x & 0xff).toByte)
    def toUShort = UShort((x & 0xffff).toShort)
    def toUInt = UInt(x)
    def toULong = ULong(x.toLong)

    def +(y: UByte) = x + y.toInt
    def -(y: UByte) = x - y.toInt
    def *(y: UByte) = x * y.toInt
    def /(y: UByte) = x / y.toInt
    def %(y: UByte) = x % y.toInt
    def &(y: UByte) = x & y.toInt
    def |(y: UByte) = x | y.toInt
    def ^(y: UByte) = x ^ y.toInt

    def +(y: UShort) = x + y.toInt
    def -(y: UShort) = x - y.toInt
    def *(y: UShort) = x * y.toInt
    def /(y: UShort) = x / y.toInt
    def %(y: UShort) = x % y.toInt
    def &(y: UShort) = x & y.toInt
    def |(y: UShort) = x | y.toInt
    def ^(y: UShort) = x ^ y.toInt

    def +(y: UInt) = x + y.toInt
    def -(y: UInt) = x - y.toInt
    def *(y: UInt) = x * y.toInt
    def /(y: UInt) = x / y.toInt
    def %(y: UInt) = x % y.toInt
    def &(y: UInt) = x & y.toInt
    def |(y: UInt) = x | y.toInt
    def ^(y: UInt) = x ^ y.toInt

  }

  class SignedLongOps(x: Long) {
    def toUByte = UByte((x & 0xffL).toByte)
    def toUShort = UShort((x & 0xffffL).toShort)
    def toUInt = UInt((x & 0xffffffffL).toInt)
    def toULong = ULong(x)
    def +(y: UInt) = x + y.toLong
    def -(y: UInt) = x - y.toLong
    def *(y: UInt) = x * y.toLong
    def /(y: UInt) = x / y.toLong
    def %(y: UInt) = x % y.toLong
    def &(y: UInt) = x & y.toLong
    def |(y: UInt) = x | y.toLong
    def ^(y: UInt) = x ^ y.toLong
  }

  class SignedRichIntOps(x: Int) {
    def to(y: UInt): Range.Inclusive = x to y.toInt
    def until(y: UInt): Range = x until y.toInt
  }

  trait UByteOrdering extends Ordering[UByte] {
    def compare(x: UByte, y: UByte) =
      if (x < y) -1
      else if (x == y) 0
      else 1
  }
  implicit object UByteOrdering extends UByteOrdering

  trait UShortOrdering extends Ordering[UShort] {
    def compare(x: UShort, y: UShort) =
      if (x < y) -1
      else if (x == y) 0
      else 1
  }
  implicit object UShortOrdering extends UShortOrdering

  trait UIntOrdering extends Ordering[UInt] {
    def compare(x: UInt, y: UInt) =
      if (x < y) -1
      else if (x == y) 0
      else 1
  }
  implicit object UIntOrdering extends UIntOrdering

  trait ULongOrdering extends Ordering[ULong] {
    def compare(x: ULong, y: ULong) =
      if (x < y) -1
      else if (x == y) 0
      else 1
  }
  implicit object ULongOrdering extends ULongOrdering

  trait UByteIsIntegral extends Integral[UByte] {
    def plus(x: UByte, y: UByte): UByte = (x + y).toUByte
    def minus(x: UByte, y: UByte): UByte = (x - y).toUByte
    def times(x: UByte, y: UByte): UByte = (x * y).toUByte
    def quot(x: UByte, y: UByte): UByte = (x / y).toUByte
    def rem(x: UByte, y: UByte): UByte = (x % y).toUByte
    def negate(x: UByte): UByte = (-x).toUByte
    def fromInt(x: Int): UByte = UByte(x.toByte)
    def toInt(x: UByte): Int = x.toInt
    def toLong(x: UByte): Long = x.toLong
    def toFloat(x: UByte): Float = x.toFloat
    def toDouble(x: UByte): Double = x.toDouble
  }
  implicit object UByteIsIntegral extends UByteIsIntegral with UByteOrdering {
    def parseString(str: String): Option[UByte] = None
  }

  trait UShortIsIntegral extends Integral[UShort] {
    def plus(x: UShort, y: UShort): UShort = (x + y).toUShort
    def minus(x: UShort, y: UShort): UShort = (x - y).toUShort
    def times(x: UShort, y: UShort): UShort = (x * y).toUShort
    def quot(x: UShort, y: UShort): UShort = (x / y).toUShort
    def rem(x: UShort, y: UShort): UShort = (x % y).toUShort
    def negate(x: UShort): UShort = (-x).toUShort
    def fromInt(x: Int): UShort = UShort(x.toShort)
    def toInt(x: UShort): Int = x.toInt
    def toLong(x: UShort): Long = x.toLong
    def toFloat(x: UShort): Float = x.toFloat
    def toDouble(x: UShort): Double = x.toDouble
  }
  implicit object UShortIsIntegral extends UShortIsIntegral with UShortOrdering {
    def parseString(str: String): Option[UShort] = None
  }

  trait UIntIsIntegral extends Integral[UInt] {
    def plus(x: UInt, y: UInt): UInt = x + y
    def minus(x: UInt, y: UInt): UInt = x - y
    def times(x: UInt, y: UInt): UInt = x * y
    def quot(x: UInt, y: UInt): UInt = x / y
    def rem(x: UInt, y: UInt): UInt = x % y
    def negate(x: UInt): UInt = -x
    def fromInt(x: Int): UInt = UInt(x)
    def toInt(x: UInt): Int = x.toInt
    def toLong(x: UInt): Long = x.toLong
    def toFloat(x: UInt): Float = x.toFloat
    def toDouble(x: UInt): Double = x.toDouble
  }
  implicit object UIntIsIntegral extends UIntIsIntegral with UIntOrdering {
    def parseString(str: String): Option[UInt] = None
  }

  trait ULongIsIntegral extends Integral[ULong] {
    def plus(x: ULong, y: ULong): ULong = x + y
    def minus(x: ULong, y: ULong): ULong = x - y
    def times(x: ULong, y: ULong): ULong = x * y
    def quot(x: ULong, y: ULong): ULong = x / y
    def rem(x: ULong, y: ULong): ULong = x % y
    def negate(x: ULong): ULong = -x
    def fromInt(x: Int): ULong = ULong(x)
    def toInt(x: ULong): Int = x.toInt
    def toLong(x: ULong): Long = x.toLong
    def toFloat(x: ULong): Float = x.toFloat
    def toDouble(x: ULong): Double = x.toDouble
  }
  implicit object ULongIsIntegral extends ULongIsIntegral with ULongOrdering {
    def parseString(str: String): Option[ULong] = None
  }

  class RicherUInt(rep: Int) {
    def bitCount = Integer.bitCount(rep)
    def highestOneBit = Integer.highestOneBit(rep)
    def lowestOneBit = Integer.lowestOneBit(rep)
    def numberOfLeadingZeros = Integer.numberOfLeadingZeros(rep)
    def numberOfTrailingZeros = Integer.numberOfTrailingZeros(rep)
    def reverse = UInt(Integer.reverse(rep))
    def reverseBytes = UInt(Integer.reverseBytes(rep))
    def rotateLeft(dist: Int) = UInt(Integer.rotateLeft(rep, dist))
    def rotateRight(dist: Int) = UInt(Integer.rotateRight(rep, dist))
    def signum = if (rep == 0) 0 else 1
  }

  class RichUInt(x: UInt) {
    def to(y: UInt): Range.Inclusive = x.toInt to y.toInt
    def until(y: UInt): Range = x.toInt until y.toInt

    def compare(y: UInt): Int = {
      if (x < y) 1
      else if (x > y) -1
      else 0
    }
    def max(y: UInt) = if (x < y) y else x
    def min(y: UInt) = if (x > y) y else x

    def +(y: Int) = x.toInt + y
    def -(y: Int) = x.toInt - y
    def *(y: Int) = x.toInt * y
    def /(y: Int) = x.toInt / y
    def %(y: Int) = x.toInt % y

    def +(y: Long) = x.toLong + y
    def -(y: Long) = x.toLong - y
    def *(y: Long) = x.toLong * y
    def /(y: Long) = x.toLong / y
    def %(y: Long) = x.toLong % y

    def +(y: Float) = x.toFloat + y
    def -(y: Float) = x.toFloat - y
    def *(y: Float) = x.toFloat * y
    def /(y: Float) = x.toFloat / y
    def %(y: Float) = x.toFloat % y

    def +(y: Double) = x.toDouble + y
    def -(y: Double) = x.toDouble - y
    def *(y: Double) = x.toDouble * y
    def /(y: Double) = x.toDouble / y
    def %(y: Double) = x.toDouble % y

    def abs = x
  }
}
