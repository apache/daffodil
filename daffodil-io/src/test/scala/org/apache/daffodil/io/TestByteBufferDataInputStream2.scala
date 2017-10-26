/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.io

import org.junit.Test
import org.junit.Assert._
import org.apache.daffodil.Implicits._
import org.apache.daffodil.exceptions.Abort
import org.apache.daffodil.util.MaybeULong

class TestByteBufferDataInputStream2 {
  val tenDigits = "1234567890"
  val ten = tenDigits.getBytes("utf-8")
  val twentyDigits = tenDigits * 2
  val twenty = twentyDigits.getBytes("utf-8")

  @Test def testMark1 {
    val dis = ByteBufferDataInputStream(ten)
    val m1 = dis.mark("testMark1")
    var arr = dis.getByteArray(80)
    assertEquals(10, arr.size)
    assertEquals(0x31.toByte, arr(0))
    assertEquals(80, dis.bitPos0b)
    assertEquals(80, dis.bitLimit0b.get)
    assertEquals(81, dis.bitPos1b)
    assertEquals(81, dis.bitLimit1b.get)
    assertEquals(10, dis.bytePos0b)
    dis.reset(m1)
    arr = dis.getByteArray(80)
    assertEquals(10, arr.size)
    assertEquals(0x30.toByte, arr(9))
    assertEquals(80, dis.bitPos0b)
    assertEquals(80, dis.bitLimit0b.get)
    assertEquals(81, dis.bitPos1b)
    assertEquals(81, dis.bitLimit1b.get)
    assertEquals(10, dis.bytePos0b)
  }

  @Test def testMark2 {
    val dis = ByteBufferDataInputStream(twenty)
    var m1: DataInputStream.Mark = null
    dis.withBitLengthLimit(5 * 8) {
      val arr = dis.getByteArray(5 * 8)
      assertEquals(5, arr.size)
      m1 = dis.mark("testMark2")
      assertFalse(dis.asIteratorChar.hasNext)
    }
    dis.setBitLimit0b(MaybeULong(10 * 8))
    var arr = dis.getByteArray(5 * 8)
    dis.mark("testMark2b")
    assertEquals(5, arr.size)
    assertEquals(0x30.toByte, arr(4))
    assertEquals(80, dis.bitPos0b)
    assertEquals(80, dis.bitLimit0b.get)
    assertEquals(81, dis.bitPos1b)
    assertEquals(81, dis.bitLimit1b.get)
    assertEquals(10, dis.bytePos0b)
    dis.reset(m1)
    assertFalse(dis.isDefinedForLength(1))
    dis.asInstanceOf[ByteBufferDataInputStream].resetBitLimit0b(MaybeULong(10 * 8))
    arr = dis.getByteArray(5 * 8)
    assertEquals(5, arr.size)
    assertEquals(0x30.toByte, arr(4))
    assertEquals(80, dis.bitPos0b)
    assertEquals(80, dis.bitLimit0b.get)
    assertEquals(81, dis.bitPos1b)
    assertEquals(81, dis.bitLimit1b.get)
    assertEquals(10, dis.bytePos0b)
  }

  @Test def testMark3 {
    val dis = ByteBufferDataInputStream(twenty).asInstanceOf[ByteBufferDataInputStream]
    dis.setBitLimit0b(MaybeULong(5 * 8))
    var arr = dis.getByteArray(5 * 8)
    assertEquals(5, arr.size)
    val m1 = dis.mark("testMark3")
    assertFalse(dis.asIteratorChar.hasNext)
    dis.resetBitLimit0b(MaybeULong(10 * 8))
    arr = dis.getByteArray(5 * 8)
    val m2 = dis.mark("testMark3b")
    assertEquals(5, arr.size)
    assertEquals(0x30.toByte, arr(4))
    assertEquals(80, dis.bitPos0b)
    assertEquals(80, dis.bitLimit0b.get)
    assertEquals(81, dis.bitPos1b)
    assertEquals(81, dis.bitLimit1b.get)
    assertEquals(10, dis.bytePos0b)
    dis.reset(m1)
    intercept[Abort] {
      dis.reset(m2)
    }
  }

}
