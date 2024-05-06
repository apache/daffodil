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

package passera.unsigned

import scala.math.ScalaNumericAnyConversions

trait Unsigned[U <: Unsigned[U, Promoted, SignedPromoted], Promoted <: Unsigned[
  _,
  Promoted,
  SignedPromoted
], SignedPromoted]
  extends Any
  with ScalaNumericAnyConversions
  with Serializable {

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
