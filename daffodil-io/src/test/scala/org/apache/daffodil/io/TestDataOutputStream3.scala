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

import junit.framework.Assert._
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.schema.annotation.props.gen.ByteOrder
import org.junit.Test

class TestDataOutputStream3 {

  val lsbfFinfo = FormatInfoForUnitTest()
  lsbfFinfo.byteOrder = ByteOrder.LittleEndian
  lsbfFinfo.bitOrder = BitOrder.LeastSignificantBitFirst

  def newDirectOrBufferedDataOutputStream(jos: java.io.OutputStream, creator: DirectOrBufferedDataOutputStream,
    bo: BitOrder = BitOrder.MostSignificantBitFirst) = {
    val os = DirectOrBufferedDataOutputStream(jos, creator)
    os.setPriorBitOrder(bo)
    os
  }

  @Test def testPutLongDirect1_LE_LSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = newDirectOrBufferedDataOutputStream(baos, null)

    out.putLong(-1L, 32, lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(-1, buf(0))
    assertEquals(-1, buf(1))
    assertEquals(-1, buf(2))
    assertEquals(-1, buf(3))
    if (buf.length > 4)
      assertEquals(0, buf(4))

  }

  @Test def testPutLongDirect1Bit_LE_LSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = newDirectOrBufferedDataOutputStream(baos, null)

    out.putLong(1, 1, lsbfFinfo)

    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(0x01.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongDirect2Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = newDirectOrBufferedDataOutputStream(baos, null)

    out.putLong(3, 2, lsbfFinfo)

    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(0x03.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongDirect7Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = newDirectOrBufferedDataOutputStream(baos, null)

    out.putLong(0xA5, 7, lsbfFinfo)

    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(0x25.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongDirect8Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = newDirectOrBufferedDataOutputStream(baos, null)

    out.putLong(0xA5, 8, lsbfFinfo)

    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(0xA5.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongDirect9Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = newDirectOrBufferedDataOutputStream(baos, null)

    out.putLong(0xA5A5, 9, lsbfFinfo)

    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(0xA5.toByte, buf(0))
    assertEquals(0x01.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  @Test def testPutLongDirect63Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = newDirectOrBufferedDataOutputStream(baos, null)

    out.putLong(0xA5A5A5A5A5A5A5A5L, 63, lsbfFinfo)

    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    val res = 0x25A5A5A5A5A5A5A5L
    var i = 0
    while (i < 8) {
      val expected = "%x".format(((res >> (8 * i)) & 0xFF).toByte)
      val actual = "%x".format(buf(i))
      assertEquals(expected, actual)
      i += 1
    }
    if (buf.length > 8)
      assertEquals(0, buf(9))

  }

  @Test def testPutLongDirect64Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = newDirectOrBufferedDataOutputStream(baos, null)

    out.putLong(0xA5A5A5A5A5A5A5A5L, 64, lsbfFinfo)

    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    val res = 0xA5A5A5A5A5A5A5A5L
    var i = 0
    while (i < 8) {
      val expected = "%x".format(((res >> (8 * i)) & 0xFF).toByte)
      val actual = "%x".format(buf(i))
      assertEquals(expected, actual)
      i += 1
    }
    if (buf.length > 8)
      assertEquals(0, buf(9))

  }

  /////////////////////////////////////////////////////
  // Tests of Buffered and putLong
  /////////////////////////////////////////////////////

  @Test def testPutLongBuffered1_LE_LSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered

    out.putLong(-1L, 32, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(-1, buf(0))
    assertEquals(-1, buf(1))
    assertEquals(-1, buf(2))
    assertEquals(-1, buf(3))
    if (buf.length > 4)
      assertEquals(0, buf(4))

  }

  @Test def testPutLongBuffered1Bit_LE_LSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered

    out.putLong(1, 1, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(0x01.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongBuffered2Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered

    out.putLong(3, 2, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(0x03.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongBuffered7Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered

    out.putLong(0xA5, 7, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(0x25.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongBuffered8Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered

    out.putLong(0xA5, 8, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(0xA5.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongBuffered9Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered

    out.putLong(0xA5A5, 9, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(0xA5.toByte, buf(0))
    assertEquals(0x01.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  @Test def testPutLongBuffered63Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered

    out.putLong(0xA5A5A5A5A5A5A5A5L, 63, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    val res = 0x25A5A5A5A5A5A5A5L
    var i = 0
    while (i < 8) {
      val expected = "%x".format(((res >> (8 * i)) & 0xFF).toByte)
      val actual = "%x".format(buf(i))
      assertEquals(expected, actual)
      i += 1
    }
    if (buf.length > 8)
      assertEquals(0, buf(9))

  }

  @Test def testPutLongBuffered64Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered

    out.putLong(0xA5A5A5A5A5A5A5A5L, 64, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    val res = 0xA5A5A5A5A5A5A5A5L
    var i = 0
    while (i < 8) {
      val expected = "%x".format(((res >> (8 * i)) & 0xFF).toByte)
      val actual = "%x".format(buf(i))
      assertEquals(expected, actual)
      i += 1
    }
    if (buf.length > 8)
      assertEquals(0, buf(8))

  }

  /////////////////////////////////////////////////////
  // Tests of Direct + Buffered and putLong
  /////////////////////////////////////////////////////

  @Test def testPutLongDirectAndBuffered1_LE_LSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered

    out.putLong(-1L, 32, lsbfFinfo)
    direct.putLong(-1L, 32, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(-1, buf(0))
    assertEquals(-1, buf(1))
    assertEquals(-1, buf(2))
    assertEquals(-1, buf(3))
    assertEquals(-1, buf(4))
    assertEquals(-1, buf(5))
    assertEquals(-1, buf(6))
    assertEquals(-1, buf(7))
    if (buf.length > 8)
      assertEquals(0, buf(8))

  }

  @Test def testPutLongDirectAndBuffered1Bit_LE_LSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val direct = newDirectOrBufferedDataOutputStream(baos, null, BitOrder.LeastSignificantBitFirst)
    val out = direct.addBuffered

    out.putLong(1, 1, lsbfFinfo)
    direct.putLong(1, 1, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(0x03.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongDirectAndBuffered2Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val direct = newDirectOrBufferedDataOutputStream(baos, null, BitOrder.LeastSignificantBitFirst)
    val out = direct.addBuffered

    out.putLong(3, 2, lsbfFinfo)
    direct.putLong(2, 2, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(0x0E.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongDirectAndBuffered7Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    direct.setPriorBitOrder(BitOrder.LeastSignificantBitFirst)
    val out = direct.addBuffered

    out.putLong(0xA5, 7, lsbfFinfo)
    direct.putLong(0xA5, 7, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(0xA5.toByte, buf(0))
    assertEquals(0x12.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  @Test def testPutLongDirectAndBuffered8Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered

    out.putLong(0xA5, 8, lsbfFinfo)
    direct.putLong(0xA5, 8, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(0xA5.toByte, buf(0))
    assertEquals(0xA5.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  @Test def testPutLongDirectAndBuffered9Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val direct = newDirectOrBufferedDataOutputStream(baos, null, BitOrder.LeastSignificantBitFirst)
    val out = direct.addBuffered

    out.putLong(0xDEAD, 9, lsbfFinfo)
    direct.putLong(0xBEEF, 9, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(0xEF.toByte, buf(0))
    assertEquals(0x5A.toByte, buf(1))
    assertEquals(0x01.toByte, buf(2))
    if (buf.length > 3)
      assertEquals(0, buf(3))

  }

  @Test def testPutLongDirectAndBuffered63BitPlus1Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val direct = newDirectOrBufferedDataOutputStream(baos, null, BitOrder.LeastSignificantBitFirst)
    val out = direct.addBuffered

    out.putLong(0xA5A5A5A5A5A5A5A5L, 63, lsbfFinfo)
    direct.putLong(1, 1, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    val res = (0xA5A5A5A5A5A5A5A5L << 1) + 1
    var i = 0
    while (i < 8) {
      val expected = "%x".format(((res >> (8 * i)) & 0xFF).toByte)
      val actual = "%x".format(buf(i))
      assertEquals(expected, actual)
      i += 1
    }
    if (buf.length > 8)
      assertEquals(0, buf(9))

  }

  @Test def testPutLongDirectAndBuffered63BitPlus63Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val direct = newDirectOrBufferedDataOutputStream(baos, null, BitOrder.LeastSignificantBitFirst)
    val out = direct.addBuffered

    out.putLong(0xA5A5A5A5A5A5A5A5L, 63, lsbfFinfo)
    direct.putLong(0xA5A5A5A5A5A5A5A5L, 63, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    val res1 = 0xA5A5A5A5A5A5A5A5L
    var i = 0
    while (i < 8) {
      val expected = "%x".format(((res1 >> (8 * i)) & 0xFF).toByte)
      val actual = "%x".format(buf(i))
      assertEquals(expected, actual)
      i += 1
    }
    val res2 = 0x25A5A5A5A5A5A5A5L >>> 1
    i = 0
    while (i < 8) {
      val expected = "%x".format(((res2 >> (8 * i)) & 0xFF).toByte)
      val actual = "%x".format(buf(i + 8))
      assertEquals(expected, actual)
      i += 1
    }

    if (buf.length > 16)
      assertEquals(0, buf(16))

  }

  @Test def testPutLongDirectAndBuffered64BitPlus64Bit_LE_LSBF {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered

    out.putLong(0xA5A5A5A5A5A5A5A5L, 64, lsbfFinfo)
    direct.putLong(0xA5A5A5A5A5A5A5A5L, 64, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    val res1 = 0xA5A5A5A5A5A5A5A5L
    var i = 0
    while (i < 8) {
      val expected = "%x".format(((res1 >> (8 * i)) & 0xFF).toByte)
      val actual = "%x".format(buf(i))
      assertEquals(expected, actual)
      i += 1
    }
    val res2 = 0xA5A5A5A5A5A5A5A5L
    i = 0
    while (i < 8) {
      val expected = "%x".format(((res2 >> (8 * i)) & 0xFF).toByte)
      val actual = "%x".format(buf(i + 8))
      assertEquals(expected, actual)
      i += 1
    }

    if (buf.length > 16)
      assertEquals(0, buf(16))

  }

  @Test def testPutLong5_4Bits_LE_LSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = newDirectOrBufferedDataOutputStream(baos, null)

    out.putLong(5L, 4, lsbfFinfo)

    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf()

    assertEquals(0x05.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

}
