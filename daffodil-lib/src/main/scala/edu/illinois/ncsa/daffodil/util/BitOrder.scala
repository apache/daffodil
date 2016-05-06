/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.util

import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder

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

  val LSBitTable = {
    val ints = 0 to 255
    val table = ints.map { asLSBitFirst1(_) }
    table.toArray
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

  def reverseBytesAndReverseBits(bb: ByteBuffer) {
    reverseBytes(bb)
    reverseBitsWithinBytes(bb)
  }

  def reverseBitsWithinBytes(bb: ByteBuffer) {
    Assert.usage(bb.position() == 0)
    var i: Int = 0
    val len = bb.remaining()
    while (i < len) {
      bb.put(i, Bits.asLSBitFirst(bb.get(i)))
      i = i + 1
    }
  }

  def reverseBytes(a: Array[Byte]) {
    var i: Int = 0
    val len = a.length
    while (i < (len >> 1)) {
      // swap positions end to end,
      // Do this in-place to avoid unnecessary further allocation.
      val upperByte = a(len - i - 1)
      val lowerByte = a(i)
      a(len - i - 1) = lowerByte
      a(i) = upperByte
      i = i + 1
    }
  }

  def reverseBytes(bb: ByteBuffer) {
    Assert.usage(bb.position() == 0)
    var i: Int = 0
    val len = bb.remaining
    while (i < (len >> 1)) {
      // swap positions end to end,
      // Do this in-place to avoid unnecessary further allocation.
      val upperByte = bb.get(len - i - 1)
      val lowerByte = bb.get(i)
      bb.put(len - i - 1, lowerByte)
      bb.put(i, upperByte)
      i = i + 1
    }
  }

  /**
   * Treat a byte buffer like a logical shift register.
   * Assumes MostSignificantBitFirst, so bits move to lower-numbered
   * positions.
   * <p>
   * Tempting, but using BigInt for this is a pain because it
   * does sign extension, or removes zero leading bytes
   * for non-negative values, etc. So we operate on byte buffers
   * instead.
   */
  def shiftLeft(bb: ByteBuffer, n: Int) {
    Assert.usage(n < 8)
    Assert.usage(bb.position() == 0)
    var leftBits: Int = 0
    var i: Int = bb.remaining() - 1
    val mask = ((1 << n) - 1)
    while (i >= 0) {
      val rightBits = (leftBits >>> 8) & mask
      val b = asUnsignedByte(bb.get(i))
      leftBits = (b << n)
      bb.put(i, asSignedByte((leftBits | rightBits) & 0xFF))
      i = i - 1
    }
  }

  /**
   * Assumes MostSignificantBitFirst bit order. Shifts "right" meaning
   * Bits increase in position.
   */
  def shiftRight(bb: ByteBuffer, n: Int) {
    Assert.usage(n < 8 && n >= 0)
    Assert.usage(bb.position() == 0)
    if (n == 0) return // nothing to do
    var rightBits: Int = 0
    var i: Int = 0
    val mask = ((1 << n) - 1) & 0xFF
    while (i < bb.remaining) {
      val leftBits = rightBits << (8 - n)
      val b = asUnsignedByte(bb.get(i))
      rightBits = b & mask
      val v = ((b >>> n) | leftBits) & 0xFF
      bb.put(i, asSignedByte(v))
      i = i + 1
    }
  }

  /**
   * The bit at position p is shifted to be at position p + n..
   *
   * The terms shiftRight and shiftLeft are ambiguous.
   *
   * Given a bit order, every bit in the bit stream is assigned a number.
   *
   *
   * In the case of MSBF, the least significant bits of byteN become the most significant of byte N+1.
   * In the case of LSBF, the most significant bits of byteN become the least significant of byte N+1.
   * For LSBF, if you write the bytes in order right-to-left, then shiftToHigherBitPosition
   * becomes a leftward movement of the bits displayed this way.
   */
  def shiftToHigherBitPosition(bitOrder: BitOrder, bb: ByteBuffer, n: Int) {
    if (bitOrder eq BitOrder.MostSignificantBitFirst) {
      shiftRight(bb, n)
    } else {
      reverseBitsWithinBytes(bb)
      shiftRight(bb, n)
      reverseBitsWithinBytes(bb)
    }
  }

}
