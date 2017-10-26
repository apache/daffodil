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
import passera.unsigned.ULong

class TestDataOutputStream4 {

  def setup(setAbs: Boolean = true) = {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val direct = DirectOrBufferedDataOutputStream(baos, null)
    direct.setBitOrder(BitOrder.MostSignificantBitFirst)
    direct.setByteOrder(ByteOrder.BigEndian)
    direct.setFillByte(0)

    direct.putLong(0x5a5a5, 19)

    // X101 1010 0101 1010 0101
    // 1011 0100 1011 0100 101X  big endian, MSBF

    val out = direct.addBuffered
    if (setAbs)
      out.setAbsStartingBitPos0b(ULong(20))
    val out2 = out.addBuffered
    if (setAbs)
      out2.setAbsStartingBitPos0b(ULong(39))

    out.putLong(0x5a5a5, 19)
    // 101 1010 0101 1010 0101
    // 1 0110 1001 0110 1001 01XX

    out2.putLong(0x5a5a5, 19)
    // 101 1010 0101 1010 0101
    // 10 1101 0010 1101 0010 1XXX

    // So the whole data is:
    // 1011 0100 1011 0100 101X
    //                        1 0110 1001 0110 1001 01XX
    //                                                10 1101 0010 1101 0010 1XXX  XXXX
    // B     4    B    4     B   6    9    6    9     6   D    2     D    2    8   0

    (baos, direct, out, out2)
  }

  private def checkResults(baos: ByteArrayOutputStreamWithGetBuf): Unit = {
    val buf = baos.getBuf()

    assertEquals(0xB4.toByte, buf(0))
    assertEquals(0xB4.toByte, buf(1))
    assertEquals(0xB6.toByte, buf(2))
    assertEquals(0x96.toByte, buf(3))
    assertEquals(0x96.toByte, buf(4))
    assertEquals(0xd2.toByte, buf(5))
    assertEquals(0xd2.toByte, buf(6))
    assertEquals(0x80.toByte, buf(7))
  }

  @Test def testPutLong19FinishInOrderAbs {
    val (baos, direct, out, out2) = setup()

    direct.setFinished()
    out.setFinished()
    out2.setFinished()

    checkResults(baos)

  }

  @Test def testPutLong19FinishOutOfOrder1Abs {
    val (baos, direct, out, out2) = setup()

    out.setFinished()
    direct.setFinished()
    out2.setFinished()

    checkResults(baos)

  }

  @Test def testPutLong19FinishOutOfOrder2Abs {
    val (baos, direct, out, out2) = setup()

    out2.setFinished()
    out.setFinished()
    direct.setFinished()

    checkResults(baos)

  }

  @Test def testPutLong19FinishOutOfOrder3Abs {
    val (baos, direct, out, out2) = setup()

    out2.setFinished()
    direct.setFinished()
    out.setFinished()

    checkResults(baos)

  }

  @Test def testPutLong19FinishInOrder {
    val (baos, direct, out, out2) = setup(false)

    direct.setFinished()
    out.setFinished()
    out2.setFinished()

    checkResults(baos)

  }

  @Test def testPutLong19FinishOutOfOrder1 {
    val (baos, direct, out, out2) = setup(false)

    out.setFinished()
    direct.setFinished()
    out2.setFinished()

    checkResults(baos)

  }

  @Test def testPutLong19FinishOutOfOrder2 {
    val (baos, direct, out, out2) = setup(false)

    out2.setFinished()
    out.setFinished()
    direct.setFinished()

    checkResults(baos)

  }

  @Test def testPutLong19FinishOutOfOrder3 {
    val (baos, direct, out, out2) = setup(false)

    out2.setFinished()
    direct.setFinished()
    out.setFinished()

    checkResults(baos)

  }
}
