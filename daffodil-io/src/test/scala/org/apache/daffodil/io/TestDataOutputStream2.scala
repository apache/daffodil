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

package edu.illinois.ncsa.daffodil.io

import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import org.junit.Test
import java.nio.ByteBuffer

class TestDataOutputStream2 {

  @Test def testPutBitsDirect0_BE_MSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = DirectOrBufferedDataOutputStream(baos, null)

    out.setBitOrder(BitOrder.MostSignificantBitFirst)
    out.setByteOrder(ByteOrder.BigEndian)

    out.putBits(List(0xA5.toByte, 0xBE.toByte, 0xEF.toByte).toArray, 1, 16)

    out.setFinished()

    val buf = baos.getBuf()

    assertEquals(0xBE.toByte, buf(0))
    assertEquals(0xEF.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  @Test def testPutBitsDirect1_BE_MSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = DirectOrBufferedDataOutputStream(baos, null)

    out.setBitOrder(BitOrder.MostSignificantBitFirst)
    out.setByteOrder(ByteOrder.BigEndian)

    out.putBits(List(0xA5.toByte, 0xBE.toByte, 0xEF.toByte).toArray, 1, 9)

    out.setFinished()

    val buf = baos.getBuf()

    assertEquals(0xBE.toByte, buf(0))
    assertEquals(0x80.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  @Test def testPutBitsDirect7_BE_MSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = DirectOrBufferedDataOutputStream(baos, null)

    out.setBitOrder(BitOrder.MostSignificantBitFirst)
    out.setByteOrder(ByteOrder.BigEndian)

    out.putBits(List(0xA5.toByte, 0xBE.toByte, 0xEF.toByte).toArray, 1, 15)

    out.setFinished()

    val buf = baos.getBuf()

    assertEquals(0xBE.toByte, buf(0))
    assertEquals(0xEE.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  /*
   * BitBuffer tests
   */

  @Test def testPutBitBufferDirect0_BE_MSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = DirectOrBufferedDataOutputStream(baos, null)

    out.setBitOrder(BitOrder.MostSignificantBitFirst)
    out.setByteOrder(ByteOrder.BigEndian)

    val bb = ByteBuffer.wrap(List(0xA5.toByte, 0xBE.toByte, 0xEF.toByte).toArray)
    bb.position(1)
    out.putBitBuffer(bb, 16)

    out.setFinished()

    val buf = baos.getBuf()

    assertEquals(0xBE.toByte, buf(0))
    assertEquals(0xEF.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  @Test def testPutBitBufferDirect1_BE_MSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = DirectOrBufferedDataOutputStream(baos, null)

    out.setBitOrder(BitOrder.MostSignificantBitFirst)
    out.setByteOrder(ByteOrder.BigEndian)

    val bb = ByteBuffer.wrap(List(0xA5.toByte, 0xBE.toByte, 0xEF.toByte).toArray)
    bb.position(1)
    out.putBitBuffer(bb, 9)

    out.setFinished()

    val buf = baos.getBuf()

    assertEquals(0xBE.toByte, buf(0))
    assertEquals(0x80.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  @Test def testPutBitBufferDirect7_BE_MSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = DirectOrBufferedDataOutputStream(baos, null)

    out.setBitOrder(BitOrder.MostSignificantBitFirst)
    out.setByteOrder(ByteOrder.BigEndian)

    val bb = ByteBuffer.wrap(List(0xA5.toByte, 0xBE.toByte, 0xEF.toByte).toArray)
    bb.position(1)
    out.putBitBuffer(bb, 15)

    out.setFinished()

    val buf = baos.getBuf()

    assertEquals(0xBE.toByte, buf(0))
    assertEquals(0xEE.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }
}
