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
import edu.illinois.ncsa.daffodil.util._

class TestByteOrder {

  @Test def testLittleEndianBitValue = {
    var bsl = 13
    assertEquals(0x80, BitsUtils.littleEndianBitValue(1, bsl))
    assertEquals(0x40, BitsUtils.littleEndianBitValue(2, bsl))
    assertEquals(0x20, BitsUtils.littleEndianBitValue(3, bsl))
    assertEquals(0x10, BitsUtils.littleEndianBitValue(4, bsl))
    assertEquals(0x08, BitsUtils.littleEndianBitValue(5, bsl))
    assertEquals(0x04, BitsUtils.littleEndianBitValue(6, bsl))
    assertEquals(0x02, BitsUtils.littleEndianBitValue(7, bsl))
    assertEquals(0x01, BitsUtils.littleEndianBitValue(8, bsl))
    assertEquals(0x1000, BitsUtils.littleEndianBitValue(9, bsl))
    assertEquals(0x800, BitsUtils.littleEndianBitValue(10, bsl))
    assertEquals(0x400, BitsUtils.littleEndianBitValue(11, bsl))
    assertEquals(0x200, BitsUtils.littleEndianBitValue(12, bsl))
    assertEquals(0x100, BitsUtils.littleEndianBitValue(13, bsl))

    bsl = 3
    assertEquals(0x4, BitsUtils.littleEndianBitValue(1, bsl))
    assertEquals(0x2, BitsUtils.littleEndianBitValue(2, bsl))
    assertEquals(0x1, BitsUtils.littleEndianBitValue(3, bsl))
  }
}

class TestBitOrder {

  @Test def testAsLSBitFirst = {
    assertEquals(0x20, Bits.asLSBitFirst(0x04))
    assertEquals(0x80, Bits.asLSBitFirst(1))
    assertEquals(0xA5, Bits.asLSBitFirst(0xA5))
    assertEquals(0xCC, Bits.asLSBitFirst(0x33))
  }

}