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

import java.nio.ByteBuffer
import java.nio.CharBuffer

import org.junit.Assert._
import org.junit.Test

import org.apache.daffodil.processors.charset.CharsetUtils
import org.apache.daffodil.util.Misc

class TestNonByteSizedCharsetEncoders1Bit {

  @Test def test1BitMSBF_01(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-MSBF")
    val encoder = cs1Bit.newEncoder()
    val cb = CharBuffer.wrap(Misc.hex2Bits("DEADBEEF"))
    val bb = ByteBuffer.allocate(4)
    val expectedBytes = Misc.hex2Bytes("DEADBEEF").toList
    val res = encoder.encode(cb, bb, false)
    assertTrue(res.isUnderflow())
    bb.flip()
    val actualBytes = bb.array().toList
    assertEquals(expectedBytes, actualBytes)
  }

  @Test def test1BitMSBF_02(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-MSBF")
    val encoder = cs1Bit.newEncoder() match {
      case nbs: NonByteSizeCharsetEncoder => nbs
      case _ => null
    }
    assertNotNull(encoder)
    val cb = CharBuffer.wrap(Misc.hex2Bits("DEADBEEF57"))
    val bb = ByteBuffer.allocate(4)
    val expectedBytes = Misc.hex2Bytes("DEADBEEF").toList
    val res = encoder.encode(cb, bb, false)
    assertTrue(res.isOverflow())
    bb.flip()
    val actualBytes = bb.array().toList
    assertEquals(expectedBytes, actualBytes)
  }

  @Test def test1BitLSBF_01(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-LSBF")
    val encoder = cs1Bit.newEncoder()
    val cb = CharBuffer.wrap(Misc.hex2Bits("7BB57DF7"))
    val bb = ByteBuffer.allocate(4)
    val expectedBytes = Misc.hex2Bytes("DEADBEEF").toList
    val res = encoder.encode(cb, bb, false)
    assertTrue(res.isUnderflow())
    bb.flip()
    val actualBytes = bb.array().toList
    assertEquals(expectedBytes, actualBytes)
  }

  @Test def test1BitLSBF_02(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-LSBF")
    val encoder = cs1Bit.newEncoder() match {
      case nbs: NonByteSizeCharsetEncoder => nbs
      case _ => null
    }
    assertNotNull(encoder)
    val cb = CharBuffer.wrap(Misc.hex2Bits("7BB57DF757"))
    val bb = ByteBuffer.allocate(4)
    val expectedBytes = Misc.hex2Bytes("DEADBEEF").toList
    val res = encoder.encode(cb, bb, false)
    assertTrue(res.isOverflow())
    bb.flip()
    val actualBytes = bb.array().toList
    assertEquals(expectedBytes, actualBytes)
  }

}
