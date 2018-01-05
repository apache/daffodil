/* Copyright (c) 2012-2016 Tresys Technology, LLC. All rights reserved.
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

import org.junit.Test
import junit.framework.Assert._
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder

class TestBits2 {

  val MSBF = BitOrder.MostSignificantBitFirst
  val LSBF = BitOrder.LeastSignificantBitFirst

  @Test def testShiftHigher1MSBF() {
    val bb = ByteBuffer.wrap(List(0xFF.toByte).toArray)
    Bits.shiftToHigherBitPosition(MSBF, bb, 1)
    assertEquals(List(0x7F.toByte), bb.array.toList)
  }

  @Test def testShiftHigher1LSBF() {
    val bb = ByteBuffer.wrap(List(0xFF.toByte).toArray)
    Bits.shiftToHigherBitPosition(LSBF, bb, 1)
    assertEquals(List(0xFE.toByte), bb.array.toList)
  }

  @Test def testShiftHigher2MSBF() {
    val bb = ByteBuffer.wrap(List(0xFF.toByte).toArray)
    Bits.shiftToHigherBitPosition(MSBF, bb, 2)
    assertEquals(List(0x3F.toByte), bb.array.toList)
  }

  @Test def testShiftHigher2LSBF() {
    val bb = ByteBuffer.wrap(List(0xFF.toByte).toArray)
    Bits.shiftToHigherBitPosition(LSBF, bb, 2)
    assertEquals(List(0xFC.toByte), bb.array.toList)
  }

  @Test def testShiftHigher7MSBF() {
    val bb = ByteBuffer.wrap(List(0xFF.toByte).toArray)
    Bits.shiftToHigherBitPosition(MSBF, bb, 7)
    assertEquals(List(0x01.toByte), bb.array.toList)
  }

  @Test def testShiftHigher7LSBF() {
    val bb = ByteBuffer.wrap(List(0xFF.toByte).toArray)
    Bits.shiftToHigherBitPosition(LSBF, bb, 7)
    assertEquals(List(0x80.toByte), bb.array.toList)
  }

  ///////////////////////////////////////////////

  @Test def testShiftHigher1MSBF2() {
    val bb = ByteBuffer.wrap(List(0xA5.toByte, 0xA5.toByte).toArray)
    Bits.shiftToHigherBitPosition(MSBF, bb, 1)
    assertEquals(List(0x52.toByte, 0xD2.toByte), bb.array.toList)
  }

  @Test def testShiftHigher1LSBF2() {
    val bb = ByteBuffer.wrap(List(0xA5.toByte, 0xA5.toByte).toArray)
    Bits.shiftToHigherBitPosition(LSBF, bb, 1)
    assertEquals(List(0x4A.toByte, 0x4B), bb.array.toList)
  }

  @Test def testShiftHigher2MSBF2() {
    val bb = ByteBuffer.wrap(List(0xA5.toByte, 0xA5.toByte).toArray)
    Bits.shiftToHigherBitPosition(MSBF, bb, 2)
    assertEquals(List(0x29.toByte, 0x69.toByte), bb.array.toList)
  }

  @Test def testShiftHigher2LSBF2() {
    val bb = ByteBuffer.wrap(List(0xA5.toByte, 0xA5.toByte).toArray)
    Bits.shiftToHigherBitPosition(LSBF, bb, 2)
    assertEquals(List(0x94.toByte, 0x96.toByte), bb.array.toList)
  }

  @Test def testShiftHigher7MSBF2() {
    val bb = ByteBuffer.wrap(List(0xA5.toByte, 0xA5.toByte).toArray)
    Bits.shiftToHigherBitPosition(MSBF, bb, 7)
    assertEquals(List(0x01.toByte, 0x4B.toByte), bb.array.toList)
  }

  @Test def testShiftHigher7LSBF2() {
    val bb = ByteBuffer.wrap(List(0xA5.toByte, 0xA5.toByte).toArray)
    Bits.shiftToHigherBitPosition(LSBF, bb, 7)
    assertEquals(List(0x80.toByte, 0xD2.toByte), bb.array.toList)
  }

}
