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
import org.apache.daffodil.util.MaybeULong

class TestNonByteSizedCharsetDecoders3Bit {

  @Test def test3BitMSBF_01(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-OCTAL-MSBF")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(64)
    val bb = ByteBuffer.wrap(Misc.bits2Bytes("000 001 010 011 100 101 110 111"))
    val res = decoder.decode(bb, cb, false)
    assertTrue(res.isUnderflow())
    cb.flip()
    val digits = cb.toString()
    assertEquals("01234567", digits)
  }

  @Test def test3BitMSBF_02(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-OCTAL-MSBF")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(7) // not enough space for last digit
    val bb = ByteBuffer.wrap(Misc.bits2Bytes("1111 000 001 010 011 100 101 110 111"))
    decoder.setInitialBitOffset(4)
    val res = decoder.decode(bb, cb, false)
    assertTrue(res.isOverflow())
    cb.flip()
    val digits = cb.toString()
    assertEquals("0123456", digits)
  }

  @Test def test3BitMSBF_03(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-OCTAL-MSBF")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(4)
    val bb = ByteBuffer.wrap(Misc.bits2Bytes("101 010 11"))
    bb.limit(bb.limit - 1)
    decoder.setFinalByteBitLimitOffset0b(MaybeULong(3))
    val res = decoder.decode(bb, cb, false)
    assertTrue(res.isUnderflow())
    cb.flip()
    val digits = cb.toString()
    assertEquals("5", digits)
  }

  @Test def test3BitMSBF_04(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-OCTAL-MSBF")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(40)
    val bb = ByteBuffer.wrap(Misc.bits2Bytes("1111 000 0|01 010 011| 100 101 11|0 111 1111"))
    bb.limit(bb.limit - 1)
    decoder.setInitialBitOffset(4)
    decoder.setFinalByteBitLimitOffset0b(MaybeULong(2))
    val res = decoder.decode(bb, cb, false)
    assertTrue(res.isUnderflow())
    cb.flip()
    val digits = cb.toString()
    assertEquals("0123456", digits)
  }

  @Test def test3BitLSBF_01(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-OCTAL-LSBF")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(64)
    val bb = ByteBuffer.wrap(Misc.bits2Bytes("10 001 000  1 100 011 0  111 110 10"))
    val res = decoder.decode(bb, cb, false)
    assertTrue(res.isUnderflow())
    cb.flip()
    val digits = cb.toString()
    assertEquals("01234567", digits)
  }

  @Test def test3BitLSBF_02(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-OCTAL-LSBF")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(8)
    val bb = ByteBuffer.wrap(Misc.bits2Bytes("1 000 1111 | 011 010 00 | 10 101 100 | 0000 111 1"))
    decoder.setInitialBitOffset(4)
    val res = decoder.decode(bb, cb, false)
    assertTrue(res.isOverflow())
    cb.flip()
    val digits = cb.toString()
    assertEquals("01234567", digits)
  }

  @Test def test3BitLSBF_03(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-OCTAL-LSBF")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(4)
    val bb = ByteBuffer.wrap(Misc.bits2Bytes("00 111 101"))
    bb.limit(bb.limit - 1)
    decoder.setFinalByteBitLimitOffset0b(MaybeULong(4))
    val res = decoder.decode(bb, cb, false)
    assertTrue(res.isUnderflow())
    cb.flip()
    val digits = cb.toString()
    assertEquals("5", digits)
  }

  @Test def test3BitLSBF_04(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-OCTAL-LSBF")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(40)
    val bb = ByteBuffer.wrap(Misc.bits2Bytes("1 000 1111 | 011 010 00 | 10 101 100 | 0000 111 1"))
    bb.limit(bb.limit - 1)
    decoder.setInitialBitOffset(4)
    decoder.setFinalByteBitLimitOffset0b(MaybeULong(4))
    val res = decoder.decode(bb, cb, false)
    assertTrue(res.isUnderflow())
    cb.flip()
    val digits = cb.toString()
    assertEquals("01234567", digits)
  }

}
