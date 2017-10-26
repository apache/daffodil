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

import java.nio.ByteBuffer
import java.nio.CharBuffer

import org.junit.Assert._
import org.junit.Test

import edu.illinois.ncsa.daffodil.processors.charset.CharsetUtils
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.MaybeULong

class TestNonByteSizedCharsetDecoders1Bit {

  @Test def test1BitMSBF_01(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-MSBF")
    val decoder = cs1Bit.newDecoder()
    val cb = CharBuffer.allocate(64)
    val bb = ByteBuffer.wrap(Misc.hex2Bytes("DEADBEEF"))
    val res = decoder.decode(bb, cb, false)
    assertTrue(res.isUnderflow())
    cb.flip()
    val bits = cb.toString()
    val hex = Misc.bytes2Hex(Misc.bits2Bytes(bits))
    assertEquals("DEADBEEF", hex)
  }

  @Test def test1BitMSBF_02(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-MSBF")
    val decoder = cs1Bit.newDecoder() match {
      case nbs: NonByteSizeCharsetDecoder => nbs
      case _ => null
    }
    assertNotNull(decoder)
    val cb = CharBuffer.allocate(32)
    val bb = ByteBuffer.wrap(Misc.hex2Bytes("DEADBEEF57"))
    decoder.setInitialBitOffset(4)
    val res = decoder.decode(bb, cb, false)
    assertTrue(res.isOverflow())
    cb.flip()
    val bits = cb.toString()
    val hex = Misc.bytes2Hex(Misc.bits2Bytes(bits))
    assertEquals("EADBEEF5", hex)
  }

  @Test def test1BitMSBF_03(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-MSBF")
    val decoder = cs1Bit.newDecoder() match {
      case nbs: NonByteSizeCharsetDecoder => nbs
      case _ => null
    }
    assertNotNull(decoder)
    val cb = CharBuffer.allocate(4)
    val bb = ByteBuffer.wrap(Misc.hex2Bytes("57"))
    bb.limit(bb.limit - 1)
    decoder.setFinalByteBitLimitOffset0b(MaybeULong(4))
    val res = decoder.decode(bb, cb, false)
    assertTrue(res.isUnderflow())
    cb.flip()
    val bits = cb.toString() + "0000"
    val hex = Misc.bytes2Hex(Misc.bits2Bytes(bits))
    assertEquals("50", hex)
  }

  @Test def test1BitMSBF_04(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-MSBF")
    val decoder = cs1Bit.newDecoder() match {
      case nbs: NonByteSizeCharsetDecoder => nbs
      case _ => null
    }
    assertNotNull(decoder)
    val cb = CharBuffer.allocate(40)
    val bb = ByteBuffer.wrap(Misc.hex2Bytes("DEADBEEF57"))
    bb.limit(bb.limit - 1)
    decoder.setInitialBitOffset(4)
    decoder.setFinalByteBitLimitOffset0b(MaybeULong(4))
    val res = decoder.decode(bb, cb, false)
    assertTrue(res.isUnderflow())
    cb.flip()
    val bits = cb.toString()
    val hex = Misc.bytes2Hex(Misc.bits2Bytes(bits))
    assertEquals("EADBEEF5", hex)
  }

  @Test def test1BitLSBF_01(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-LSBF")
    val decoder = cs1Bit.newDecoder()
    val cb = CharBuffer.allocate(64)
    val bb = ByteBuffer.wrap(Misc.hex2Bytes("DEADBEEF"))
    val res = decoder.decode(bb, cb, false)
    // DE => 0111 1011 AD => 1011 0101 BE => 0111 1101 EF => 1111 0111
    assertTrue(res.isUnderflow())
    cb.flip()
    val bits = cb.toString()
    val hex = Misc.bytes2Hex(Misc.bits2Bytes(bits))
    assertEquals("7BB57DF7", hex)
  }

  @Test def test1BitLSBF_02(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-LSBF")
    val decoder = cs1Bit.newDecoder() match {
      case nbs: NonByteSizeCharsetDecoder => nbs
      case _ => null
    }
    assertNotNull(decoder)
    val cb = CharBuffer.allocate(32)
    val bb = ByteBuffer.wrap(Misc.hex2Bytes("DEADBEEF57"))
    decoder.setInitialBitOffset(4)
    // skips the E of the DE byte
    // writing RTL => 7 EF BE AD D
    // regroup as: 7E FB EA DD
    // Bits LSBF for this starting from right advancing LSBF 1 bit at a time:
    // DD => 1011 1011 EA => 0101 0111 FB => 1101 1111 7E => 0111 1110
    val res = decoder.decode(bb, cb, false)
    assertTrue(res.isOverflow())
    cb.flip()
    val bits = cb.toString()
    val hex = Misc.bytes2Hex(Misc.bits2Bytes(bits))
    // BB 57 DF 7E
    assertEquals("BB57DF7E", hex)
  }

  @Test def test1BitLSBF_03(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-LSBF")
    val decoder = cs1Bit.newDecoder() match {
      case nbs: NonByteSizeCharsetDecoder => nbs
      case _ => null
    }
    assertNotNull(decoder)
    val cb = CharBuffer.allocate(4)
    val bb = ByteBuffer.wrap(Misc.hex2Bytes("57"))
    bb.limit(bb.limit - 1)
    decoder.setFinalByteBitLimitOffset0b(MaybeULong(4))
    val res = decoder.decode(bb, cb, false)
    assertTrue(res.isUnderflow())
    cb.flip()
    val bits = "0000" + cb.toString()
    val hex = Misc.bytes2Hex(Misc.bits2Bytes(bits))
    assertEquals("0E", hex)
  }

  @Test def test1BitLSBF_04(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-LSBF")
    val decoder = cs1Bit.newDecoder() match {
      case nbs: NonByteSizeCharsetDecoder => nbs
      case _ => null
    }
    assertNotNull(decoder)
    val cb = CharBuffer.allocate(40)
    val bb = ByteBuffer.wrap(Misc.hex2Bytes("DEADBEEF57"))
    bb.limit(bb.limit - 1)
    decoder.setInitialBitOffset(4)
    decoder.setFinalByteBitLimitOffset0b(MaybeULong(4))
    // skips the E of the DE byte
    // writing RTL => 7 EF BE AD D
    // regroup as: 7E FB EA DD
    val res = decoder.decode(bb, cb, false)
    assertTrue(res.isUnderflow())
    cb.flip()
    val bits = cb.toString()
    val hex = Misc.bytes2Hex(Misc.bits2Bytes(bits))
    assertEquals("BB57DF7E", hex)
  }

}
