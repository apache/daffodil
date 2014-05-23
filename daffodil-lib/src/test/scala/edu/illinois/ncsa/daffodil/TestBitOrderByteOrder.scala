package edu.illinois.ncsa.daffodil

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import junit.framework.Assert.assertEquals
import edu.illinois.ncsa.daffodil.util._
import org.junit.Test

class TestByteOrder {

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
        8 else numBitsInFinalPartialByte
    val placeValueExponentOfBitInByte = widthOfActiveBitsInByte - bitPosInByte
    val bitValueInByte = 1 << placeValueExponentOfBitInByte
    val byteNumZeroBased = (bitPosition - 1) / 8
    val scaleFactorForBytePosition = 1 << (8 * byteNumZeroBased)
    val bitValue = bitValueInByte * scaleFactorForBytePosition
    bitValue
  }

  // Alternate formulation. Trying to simplify. Didn't work. 
  //  def littleEndianBitValue1(bitPosition: Int, bitStringLength: Int): Int = {
  //    assert(bitPosition >= 1) // one based
  //    assert(bitStringLength >= 1)
  //    assert(bitStringLength >= bitPosition) // bit pos within the bit string length1
  //    val bitPos0b = bitPosition - 1
  //    val countOfWholeBytesInBitStringWidth = (bitStringLength / 8)
  //    val lastBitPositionWithinAWholeByte = countOfWholeBytesInBitStringWidth * 8
  //    val countOfWholeBytesPrecedingOurByte = (bitPos0b / 8)
  //    if (bitPosition <= lastBitPositionWithinAWholeByte) {
  //      // it's a bit within the whole bytes of the number
  //      val res = 1 << ((7 - (bitPos0b % 8)) + (8 * countOfWholeBytesPrecedingOurByte))
  //      res
  //    } else {
  //      // it's a bit within the final partial byte.
  //      val fragmentPart = 1 << (((bitStringLength % 8) - (bitPosition % 8)))
  //      val res = (fragmentPart << (countOfWholeBytesPrecedingOurByte * 8))
  //      res
  //    }
  //  }

  @Test def testLittleEndianBitValue = {
    var bsl = 13
    assertEquals(0x80, littleEndianBitValue(1, bsl))
    assertEquals(0x40, littleEndianBitValue(2, bsl))
    assertEquals(0x20, littleEndianBitValue(3, bsl))
    assertEquals(0x10, littleEndianBitValue(4, bsl))
    assertEquals(0x08, littleEndianBitValue(5, bsl))
    assertEquals(0x04, littleEndianBitValue(6, bsl))
    assertEquals(0x02, littleEndianBitValue(7, bsl))
    assertEquals(0x01, littleEndianBitValue(8, bsl))
    assertEquals(0x1000, littleEndianBitValue(9, bsl))
    assertEquals(0x800, littleEndianBitValue(10, bsl))
    assertEquals(0x400, littleEndianBitValue(11, bsl))
    assertEquals(0x200, littleEndianBitValue(12, bsl))
    assertEquals(0x100, littleEndianBitValue(13, bsl))

    bsl = 3
    assertEquals(0x4, littleEndianBitValue(1, bsl))
    assertEquals(0x2, littleEndianBitValue(2, bsl))
    assertEquals(0x1, littleEndianBitValue(3, bsl))
  }
}

class TestBitOrder {

  @Test def testSwizzle = {
    assertEquals(1, Bits.swizzle(1, 1))
    assertEquals(2, Bits.swizzle(1, 2))
    assertEquals(3, Bits.swizzle(6, 3))
    assertEquals(0x15, Bits.swizzle(0x15, 5))
  }

  @Test def testAsLSBitFirst = {
    assertEquals(0x20, Bits.asLSBitFirst(0x04))
    assertEquals(0x80, Bits.asLSBitFirst(1))
    assertEquals(0xA5, Bits.asLSBitFirst(0xA5))
    assertEquals(0xCC, Bits.asLSBitFirst(0x33))
  }

  @Test def testBitOrder1 = {
    val bytes = Array[Int](0xE3, 0x67, 0x00, 0x80, 0x55, 0x67, 0x92, 0x1A, 0xFC)
    val LSBFirstBytes = bytes.map { Bits.asLSBitFirst(_) }
    /*
     * See MIL-STD-2045-47001D w/Change 1 spec, Table B-I page 70.
     * First 8 fields are:
     * 
     * Version 4-bits value 3
     * FPI value 0
     * Data Compression Type 2 bits - these bits not present due to FPI
     * GPI value = 1
     * FPI value = 1
     * URN 24 bits value = 207 (decimal)
     * FPI value = 1
     * Unit Name (string 448 max) value = UNITA (terminated by ASCII DEL (0x7F character)
     */
  }

}