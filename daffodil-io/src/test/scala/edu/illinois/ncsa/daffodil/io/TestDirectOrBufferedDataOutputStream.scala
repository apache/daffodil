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

import org.junit.Test
import junit.framework.Assert._

import java.io.ByteArrayInputStream
import java.io.InputStreamReader
import org.apache.commons.io.IOUtils

class TestDirectOrBufferedDataOutputStream {

  private def getString(baos: ByteArrayOutputStreamWithGetBuf) = {
    val is = new ByteArrayInputStream(baos.getBuf)
    val ir = new InputStreamReader(is, "ascii")
    val line = IOUtils.toString(ir)
    val res = line.replace("""\u0000""", "")
    res
  }

  @Test def testToString {
    val baos = new ByteArrayOutputStreamWithGetBuf()
    val layered = DirectOrBufferedDataOutputStream(baos, null)
    System.err.println(layered.toString())
  }

  @Test def testCollapsingBufferIntoDirect1 {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val layered = DirectOrBufferedDataOutputStream(baos, null)

    val hw = "Hello World!"
    val hwBytes = hw.getBytes("ascii")

    layered.putBytes(hwBytes)

    assertEquals(hw, getString(baos))

    val buf1 = layered.addBuffered

    buf1.putBytes("buf1".getBytes("ascii"))

    layered.setFinished() // collapses layered into buf1.

    assertTrue(layered.isDead)

    assertEquals(hw + "buf1", getString(baos))

    assertTrue(buf1.isDirect)
    assertFalse(buf1.isFinished)

  }

  @Test def testCollapsingFinishedBufferIntoLayered {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val layered = DirectOrBufferedDataOutputStream(baos, null)

    val hw = "Hello World!"
    val hwBytes = hw.getBytes("ascii")

    layered.putBytes(hwBytes)

    assertEquals(hw, getString(baos))

    val buf1 = layered.addBuffered

    buf1.putBytes("buf1".getBytes("ascii"))

    buf1.setFinished()

    assertTrue(buf1.isFinished)

    layered.setFinished() // collapses layered into buf1.

    assertTrue(buf1.isDead) // because it was finished when layered was subsequently finished
    assertTrue(layered.isDead)

    assertEquals(hw + "buf1", getString(baos))

  }

  @Test def testCollapsingTwoBuffersIntoDirect {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val layered = DirectOrBufferedDataOutputStream(baos, null)

    val hw = "Hello World!"
    val hwBytes = hw.getBytes("ascii")

    layered.putBytes(hwBytes)

    assertEquals(hw, getString(baos))

    val buf1 = layered.addBuffered
    val buf2 = buf1.addBuffered

    buf1.putBytes("buf1".getBytes("ascii"))
    buf2.putBytes("buf2".getBytes("ascii"))

    assertTrue(buf2.isBuffering)

    buf1.setFinished() // buf1 finished while layered (before it) is still unfinished.

    assertTrue(buf1.isFinished)

    layered.setFinished() // collapses layered into buf1. Since buf1 is finished already, this melds them, outputs everything
    // and leaves the whole thing finished.
    // leaves layered dead/unusable.

    assertTrue(buf1.isDead) // because it was finished when layered was subsequently finished
    assertTrue(layered.isDead)

    assertEquals(hw + "buf1" + "buf2", getString(baos))

    assertTrue(buf2.isDirect)

  }
}
