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
import scala.collection.immutable.NumericRange

package object numerics {
  implicit def toRicherInt(x: Int): RicherInt = new RicherInt(x)
  implicit def toRicherInt(x: scala.runtime.RichInt) = new YetRicherInt(
    x.self.asInstanceOf[Int]
  )

  implicit def toRicherLong(x: Long): RicherLong = new RicherLong(x)
  implicit def toRicherLong(x: scala.runtime.RichLong) = new YetRicherLong(
    x.self.asInstanceOf[Long]
  )

  class RicherInt(x: Int) extends Proxy {
    def self: Any = x

    import java.lang.Integer

    def bitCount: Int = Integer.bitCount(x)
    def highestOneBit: Int = Integer.highestOneBit(x)
    def lowestOneBit: Int = Integer.lowestOneBit(x)
    def numberOfLeadingZeros: Int = Integer.numberOfLeadingZeros(x)
    def numberOfTrailingZeros: Int = Integer.numberOfTrailingZeros(x)

    def reverse: Int = Integer.reverse(x)
    def reverseBytes: Int = Integer.reverseBytes(x)
    def rotateLeft(dist: Int): Int = Integer.rotateLeft(x, dist)
    def rotateRight(dist: Int): Int = Integer.rotateRight(x, dist)
    def signum: Int = Integer.signum(x)

    def <<@(dist: Int) = rotateLeft(dist)
    def >>@(dist: Int) = rotateRight(dist)
  }

  class YetRicherInt(x: Int) extends Proxy {
    def self: Any = x
    def to(y: Long, step: Long = 1L): NumericRange[Long] = x.toLong to y by step
    def until(y: Long, step: Long = 1L): NumericRange[Long] = x.toLong until y by step
    def max(y: Long): Long = x.toLong.max(y)
    def min(y: Long): Long = x.toLong.min(y)
  }

  class RicherLong(x: Long) extends Proxy {
    def self: Any = x

    import java.lang.Long

    def bitCount: Int = Long.bitCount(x)
    def highestOneBit: scala.Long = Long.highestOneBit(x)
    def lowestOneBit: scala.Long = Long.lowestOneBit(x)
    def numberOfLeadingZeros: Int = Long.numberOfLeadingZeros(x)
    def numberOfTrailingZeros: Int = Long.numberOfTrailingZeros(x)

    def reverse: scala.Long = Long.reverse(x)
    def reverseBytes: scala.Long = Long.reverseBytes(x)
    def rotateLeft(dist: Int): scala.Long = Long.rotateLeft(x, dist)
    def rotateRight(dist: Int): scala.Long = Long.rotateRight(x, dist)
    def signum: Int = Long.signum(x)

    def <<@(dist: Int) = rotateLeft(dist)
    def >>@(dist: Int) = rotateRight(dist)
  }

  class YetRicherLong(x: Long) extends Proxy {
    def self: Any = x
  }
}
