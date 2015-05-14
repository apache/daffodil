package passera

import scala.language.implicitConversions

package object numerics {
  implicit def toRicherInt(x: Int): RicherInt = new RicherInt(x)
  implicit def toRicherInt(x: scala.runtime.RichInt) = new YetRicherInt(x.self.asInstanceOf[Int])

  implicit def toRicherLong(x: Long): RicherLong = new RicherLong(x)
  implicit def toRicherLong(x: scala.runtime.RichLong) = new YetRicherLong(x.self.asInstanceOf[Long])

  class RicherInt(x: Int) extends Proxy {
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
  }

  class YetRicherInt(x: Int) extends Proxy {
    def self: Any = x
    def to(y: Long, step: Long = 1L) = x.toLong to y by step
    def until(y: Long, step: Long = 1L) = x.toLong until y by step
    def max(y: Long) = x.toLong max y
    def min(y: Long) = x.toLong min y
  }

  class RicherLong(x: Long) extends Proxy {
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
  }

  class YetRicherLong(x: Long) extends Proxy {
    def self: Any = x
  }
}
