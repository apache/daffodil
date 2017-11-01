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
import java.nio.ByteBuffer
import org.apache.daffodil.schema.annotation.props.gen.ByteOrder

class TestByteBufferDataInputStream4 {

  @Test def testLittleEndianFloat1() {
    val bb = ByteBuffer.allocate(4)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asFloatBuffer()
    fb.position(0)
    fb.put(1.125.toFloat)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    val expected = 1.125.toFloat
    val md = dis.getBinaryFloat()
    assertEquals(expected, md, 0.01)
    assertEquals(32, dis.bitPos0b)
  }

  @Test def testLittleEndianDouble1() {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asDoubleBuffer()
    fb.position(0)
    fb.put(1.125)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    val expected = 1.125
    val md = dis.getBinaryDouble()
    assertEquals(expected, md, 0.01)
    assertEquals(64, dis.bitPos0b)
  }

  @Test def testLittleEndianLong1() {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val expected = 0x0102030405060708L
    fb.put(expected)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    val md = dis.getSignedLong(64)
    assertEquals(expected, md)
    assertEquals(64, dis.bitPos0b)
  }

  @Test def testLittleEndianLong2() {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val expected = 0x01020304L
    fb.put(expected)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    val md = dis.getSignedLong(32)
    assertEquals(expected, md)
    assertEquals(32, dis.bitPos0b)
  }

  @Test def testLittleEndianLong3() {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val expected = 0x8070605040302010L
    fb.put(expected)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    val md = dis.getSignedLong(64)
    assertEquals(expected, md)
    assertEquals(64, dis.bitPos0b)
  }

  @Test def testLittleEndianLong4() {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val data = 0x0000000080706050L
    fb.put(data)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    val md = dis.getSignedLong(32)
    assertEquals(0x80706050.toInt, md)
    assertEquals(32, dis.bitPos0b)
  }

  @Test def testLittleEndianLong5() {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.BIG_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val data = 0x8000000000000000L
    fb.put(data)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    val md = dis.getSignedLong(1)
    assertEquals(1, md)
    assertEquals(1, dis.bitPos0b)
  }
}
