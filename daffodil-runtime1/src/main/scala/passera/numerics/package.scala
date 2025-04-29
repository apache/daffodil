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

package object numerics {
  implicit def toRicherInt(x: Int): RicherInt = new RicherInt(x)
  implicit def toRicherInt(x: scala.runtime.RichInt): YetRicherInt = new YetRicherInt(
    x.self.asInstanceOf[Int]
  )

  implicit def toRicherLong(x: Long): RicherLong = new RicherLong(x)
  implicit def toRicherLong(x: scala.runtime.RichLong): YetRicherLong = new YetRicherLong(
    x.self.asInstanceOf[Long]
  )

  class RicherInt(x: Int) {
    def self: Any = x

    import java.lang.Integer

    def bitCount = Integer.bitCount(x)
    def highestOneBit = Integer.highestOneBit(x)
    def lowestOneBit = Integer.lowestOneBit(x)
    def numberOfLeadingZeros = Integer.numberOfLeadingZeros(x)
    def numberOfTrailingZeros = Integer.numberOfTrailingZeros(x)

    def reverse = Integer.reverse(x)
    def reverseBytes = Integer.reverseBytes(x)
    def rotateLeft(dist: Int) = Integer.rotateLeft(x, dist)
    def rotateRight(dist: Int) = Integer.rotateRight(x, dist)
    def signum = Integer.signum(x)

    def <<@(dist: Int) = rotateLeft(dist)
    def >>@(dist: Int) = rotateRight(dist)

    override def equals(obj: Any): Boolean = super.equals(obj)
    override def toString: String = super.toString
    override def hashCode(): Int = super.hashCode()
  }

  class YetRicherInt(x: Int) {
    def self: Any = x
    def to(y: Long, step: Long = 1L) = x.toLong to y by step
    def until(y: Long, step: Long = 1L) = x.toLong until y by step
    def max(y: Long) = x.toLong.max(y)
    def min(y: Long) = x.toLong.min(y)
    override def equals(obj: Any): Boolean = super.equals(obj)
    override def toString: String = super.toString
    override def hashCode(): Int = super.hashCode()
  }

  class RicherLong(x: Long) {
    def self: Any = x

    import java.lang.Long

    def bitCount = Long.bitCount(x)
    def highestOneBit = Long.highestOneBit(x)
    def lowestOneBit = Long.lowestOneBit(x)
    def numberOfLeadingZeros = Long.numberOfLeadingZeros(x)
    def numberOfTrailingZeros = Long.numberOfTrailingZeros(x)

    def reverse = Long.reverse(x)
    def reverseBytes = Long.reverseBytes(x)
    def rotateLeft(dist: Int) = Long.rotateLeft(x, dist)
    def rotateRight(dist: Int) = Long.rotateRight(x, dist)
    def signum = Long.signum(x)

    def <<@(dist: Int) = rotateLeft(dist)
    def >>@(dist: Int) = rotateRight(dist)

    override def equals(obj: Any): Boolean = super.equals(obj)
    override def toString: String = super.toString
    override def hashCode(): Int = super.hashCode()
  }

  class YetRicherLong(x: Long) {
    def self: Any = x
    override def equals(obj: Any): Boolean = super.equals(obj)
    override def toString: String = super.toString
    override def hashCode(): Int = super.hashCode()
  }
}
