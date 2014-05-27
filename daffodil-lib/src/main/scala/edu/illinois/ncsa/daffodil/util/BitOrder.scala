package edu.illinois.ncsa.daffodil.util

import edu.illinois.ncsa.daffodil.exceptions.Assert

object Bits {

  /**
   * Convert signed Byte type to Int that is the unsigned equivalent.
   */
  def asUnsignedByte(b: Byte): Int = if (b < 0) 256 + b else b
  def asSignedByte(i: Int) = {
    Assert.usage(i >= 0)
    val res = if (i > 127) i - 256 else i
    res.toByte
  }

  /**
   * Iterative. Obvious, but slow.
   */
  def asLSBitFirst1(b: Int) = {
    assert(b >= 0)
    var res = 0
    var bits = b
    for (i <- 0 to 7) {
      res = (res << 1) | (bits & 0x01)
      bits = bits >> 1
    }
    res
  }

  /**
   * All one unrolled expression. Should be faster.
   */
  def asLSBitFirst2(b: Int) = {
    assert(b >= 0)
    val res =
      ((b & 0x01) << 7) +
        ((b & 0x02) << 5) +
        ((b & 0x04) << 3) +
        ((b & 0x08) << 1) +
        ((b & 0x10) >> 1) +
        ((b & 0x20) >> 3) +
        ((b & 0x40) >> 5) +
        ((b & 0x80) >> 7)
    res
  }

  val LSBitTable = {
    val ints = 0 to 255
    val table = ints.map { asLSBitFirst1(_) }
    table
  }

  /**
   * Via lookup table: should be fastest.
   */
  def asLSBitFirst(b: Int): Int = {
    LSBitTable(b)
  }

  def asLSBitFirst(b: Byte): Byte = {
    asSignedByte(LSBitTable(asUnsignedByte(b)))
  }

  /**
   * Treat a byte array like a logical shift register
   * <p>
   * Using BigInt for this is a pain because it
   * does sign extension, or removes zero leading bytes
   * for non-negative values, etc.
   */
  def shiftLeft(ba: Array[Byte], n: Int) {
    Assert.usage(n < 8)
    var leftBits: Int = 0
    var i: Int = ba.length - 1
    val mask = ((1 << n) - 1)
    while (i >= 0) {
      val rightBits = (leftBits >>> 8) & mask
      val b = asUnsignedByte(ba(i))
      leftBits = (b << n)
      ba(i) = asSignedByte((leftBits | rightBits) & 0xFF)
      i = i - 1
    }
  }

  def shiftRight(ba: Array[Byte], n: Int) {
    Assert.usage(n < 8)
    var rightBits: Int = 0
    var i: Int = 0
    val mask = ((1 << n) - 1) & 0xFF
    while (i < ba.length) {
      val leftBits = rightBits << (8 - n)
      val b = asUnsignedByte(ba(i))
      rightBits = b & mask
      val v = ((b >>> n) | leftBits) & 0xFF
      ba(i) = asSignedByte(v)
      i = i + 1
    }
  }

  /**
   * Given a value b, and a field width, produces the value
   * obtained by reversing the order of its bits.
   *
   * For unit testing.
   */
  //  def swizzle(b: Int, width: Int) = {
  //    assert(width <= 8 && width >= 1)
  //    assert(b <= (1 << width) - 1)
  //    assert(b >= 0)
  //    val flipped = asLSBitFirst(b)
  //    val res = flipped >> 8 - width
  //    res
  //  }
}