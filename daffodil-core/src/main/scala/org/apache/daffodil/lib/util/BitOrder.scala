/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.lib.util

import java.nio.ByteBuffer

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder

object Bits {

  /**
   * When dealing with bitOrder, it is very common to need a bit mask of some
   * amount of the right-most or left-most bits of a byte. These two arrays are
   * a useful way to get such a bit mask in constant time and make the logic
   * more clear. Each index in the array (between 0 and 8) contains a bit mask
   * for that many bits on either the leftor right side of a byte. For example:
   *
   *   Bits.maskR(2) == 0b00000011
   *   Bits.maskL(2) == 0b11000000
   */
  val maskR: IndexedSeq[Int] = (0 to 8).map { bits => (1 << bits) - 1 }
  val maskL: IndexedSeq[Int] = (0 to 8).map { bits => ((1 << bits) - 1) << (8 - bits) }

  /**
   * Convert signed Byte type to Int that is the unsigned equivalent.
   */
  def asUnsignedByte(b: Byte): Int = if (b < 0) 256 + b else b
  def asUnsignedByte(b: Long): Int = (b & 0xff).toInt

  def asSignedByte(i: Long): Byte = {
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

  def reverseBytesAndReverseBits(bb: ByteBuffer): Unit = {
    reverseBytes(bb)
    reverseBitsWithinBytes(bb)
  }

  def reverseBitsWithinBytes(bb: ByteBuffer): Unit = {
    Assert.usage(bb.position() == 0)
    var i: Int = 0
    val len = bb.remaining()
    while (i < len) {
      bb.put(i, Bits.asLSBitFirst(bb.get(i)))
      i = i + 1
    }
  }

  def reverseBytes(a: Array[Byte], length: Int): Unit = {
    var i: Int = 0
    var lowerIndex = 0
    var upperIndex = length - 1
    while (lowerIndex < upperIndex) {
      val tmp = a(lowerIndex)
      a(lowerIndex) = a(upperIndex)
      a(upperIndex) = tmp
      lowerIndex += 1
      upperIndex -= 1
    }
  }

  def reverseBytes(bb: ByteBuffer): Unit = {
    Assert.usage(bb.position() == 0)
    var i: Int = 0
    var lowerIndex = bb.position()
    var upperIndex = bb.remaining() - 1
    while (lowerIndex < upperIndex) {
      val tmp = bb.get(lowerIndex)
      bb.put(lowerIndex, bb.get(upperIndex))
      bb.put(upperIndex, tmp)
      lowerIndex += 1
      upperIndex -= 1
    }
  }

  /* Shifts the bits in a byte array left by a specified number of bits. This
   * assumes that the byte array is MSBF BE and that the number of bits to
   * shift is less than 8. This will not increase the size of the array if bits
   * are shifted off to the left. They will just be dropped. The purpose of
   * this function is to shift an array to the left to remove any padding bits
   * due to a fragment byte.
   */
  def shiftLeft(ba: Array[Byte], bitsToShift: Int): Unit = {
    Assert.usage(bitsToShift < 8)
    Assert.usage(ba.size > 0)

    val curShift = bitsToShift
    val nextShift = 8 - bitsToShift
    var i = 0
    var curByte = asUnsignedByte(ba(0))
    while (i < ba.size - 1) {
      val nextByte = asUnsignedByte(ba(i + 1))
      ba(i) = asSignedByte(((curByte << curShift) & 0xff) | (nextByte >>> nextShift))
      curByte = nextByte
      i += 1
    }
    ba(i) = asSignedByte((curByte << curShift) & 0xff)
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
  def shiftLeft(bb: ByteBuffer, n: Int): Unit = {
    Assert.usage(n < 8)
    Assert.usage(bb.position() == 0)
    var leftBits: Int = 0
    var i: Int = bb.remaining() - 1
    val mask = ((1 << n) - 1)
    while (i >= 0) {
      val rightBits = (leftBits >>> 8) & mask
      val b = asUnsignedByte(bb.get(i))
      leftBits = (b << n)
      val shiftedByte = (leftBits | rightBits) & 0xff
      bb.put(i, asSignedByte(shiftedByte))
      i = i - 1
    }
  }

  /**
   * Assumes MostSignificantBitFirst bit order. Shifts "right" meaning
   * Bits increase in position.
   */
  def shiftRight(bb: ByteBuffer, n: Int): Unit = {
    Assert.usage(n < 8 && n >= 0)
    Assert.usage(bb.position() == 0)
    if (n == 0) return // nothing to do
    var rightBits: Int = 0
    var i: Int = 0
    val mask = ((1 << n) - 1) & 0xff
    while (i < bb.remaining) {
      val leftBits = rightBits << (8 - n)
      val b = asUnsignedByte(bb.get(i))
      rightBits = b & mask
      val v = ((b >>> n) | leftBits) & 0xff
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
  def shiftToHigherBitPosition(bitOrder: BitOrder, bb: ByteBuffer, n: Int): Unit = {
    if (bitOrder eq BitOrder.MostSignificantBitFirst) {
      shiftRight(bb, n)
    } else {
      reverseBitsWithinBytes(bb)
      shiftRight(bb, n)
      reverseBitsWithinBytes(bb)
    }
  }

  /**
   * Returns a string of '1' and '0' characters for a single unsigned byte.
   *
   * This is for debugging
   */
  def asBits(unsignedByte: Int): String = {
    String.format("%8s", unsignedByte.toBinaryString).replace(' ', '0')
  }

  def signExtend(l: Long, bitLength: Int): Long = {
    Assert.usage(bitLength > 0 && bitLength <= 64)
    if (bitLength == 1) return l // a single bit has no sign to extend
    val shift = 64 - bitLength
    val res = ((l << shift) >> shift) // arithmetic shift right extends sign.
    res
  }

  def unSignExtend(l: Long, bitLength: Int): Long = {
    Assert.usage(bitLength > 0 && bitLength <= 64)
    val mask = if (bitLength == 64) -1L else (1L << bitLength) - 1
    l & mask
  }

  /**
   * Round up the bitPosition to the nearest byte position.
   *
   * For example, say we want to read up to a bit position that is not a full
   * byte. That will require reading the full byte, leaving the bytePosition
   * past our bit position. This function can be used to determine where that
   * byte position will be. Essentially, if bitPos0b is not a multiple of 8, we
   * round up one byte.
   */
  def roundUpBitToBytePosition(bitPos0b: Long): Long = {
    val bytePos0b = bitPos0b >> 3
    if ((bitPos0b & 0x7) == 0) bytePos0b
    else bytePos0b + 1
  }

  /**
   * From DFDL Spec. Sept 2013 draft 1.0.4, Section 13.7.1.4
   *
   * The value of a bit does not depend on the alignment of that bit,
   * but only where it appears in the bit string, and the byte order when
   * the length of the bit string is greater than 1 byte.
   *
   * Implements 2^N exponentiation with shifting 1 << N.
   */
  def littleEndianBitValue(bitPosition: Int, bitStringLength: Int) = {
    assert(bitPosition >= 1) // one based
    assert(bitStringLength >= 1)
    assert(bitStringLength >= bitPosition) // bit pos within the bit string length
    val numBitsInFinalPartialByte = bitStringLength % 8
    val numBitsInWholeBytes = bitStringLength - numBitsInFinalPartialByte
    val bitPosInByte = ((bitPosition - 1) % 8) + 1
    val widthOfActiveBitsInByte =
      if (bitPosition <= numBitsInWholeBytes)
        8
      else numBitsInFinalPartialByte
    val placeValueExponentOfBitInByte = widthOfActiveBitsInByte - bitPosInByte
    val bitValueInByte = 1 << placeValueExponentOfBitInByte
    val byteNumZeroBased = (bitPosition - 1) / 8
    val scaleFactorForBytePosition = 1 << (8 * byteNumZeroBased)
    val bitValue = bitValueInByte * scaleFactorForBytePosition
    bitValue
  }
}
