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

import org.junit.Test
import junit.framework.Assert._

class TestBits {

  @Test def testShiftLeftByteArray1() {
    val ba = List(0x7C.toByte).toArray
    Bits.shiftLeft(ba, 3)
    assertEquals(List(0xE0.toByte), ba.toList)
  }

  @Test def testShiftLeftByteArray2() {
    val ba = List(0x7C.toByte, 0x3D.toByte).toArray
    Bits.shiftLeft(ba, 3)
    assertEquals(List(0xE1.toByte, 0xE8.toByte), ba.toList)
  }

  @Test def testShiftLeftByteArray3() {
    val ba = List(0x7C.toByte).toArray
    Bits.shiftLeft(ba, 0)
    assertEquals(List(0x7c.toByte), ba.toList)
  }

  @Test def testShiftRightByteArray1() {
    val ba = List(0x7C.toByte).toArray
    Bits.shiftRight(ba, 3)
    assertEquals(List(0x0F.toByte), ba.toList)
  }

  @Test def testShiftRightByteArray2() {
    val ba = List(0x7C.toByte).toArray
    Bits.shiftRight(ba, 0)
    assertEquals(List(0x7C.toByte), ba.toList)
  }

  @Test def testShiftRightByteArray3() {
    val ba = List(0x7C.toByte, 0x3D.toByte).toArray
    Bits.shiftRight(ba, 3)
    assertEquals(List(0x0F.toByte, 0x87.toByte), ba.toList)
  }

  @Test def testShiftRightByteArray4() {
    val ba = List(0x7C.toByte, 0x3D.toByte, 0x42.toByte).toArray
    Bits.shiftRight(ba, 3)
    assertEquals(List(0x0F.toByte, 0x87.toByte, 0xA8.toByte), ba.toList)
  }
}