/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.io

import java.nio.ByteBuffer
import java.nio.CharBuffer

import org.apache.daffodil.io.processors.charset.CharsetUtils
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.lib.util.Misc

import org.junit.Assert._
import org.junit.Test

class TestNonByteSizedCharsetDecoders3Bit {

  val lsbfFinfo = FormatInfoForUnitTest()
  lsbfFinfo.byteOrder = ByteOrder.BigEndian
  lsbfFinfo.bitOrder = BitOrder.LeastSignificantBitFirst

  val msbfFinfo = FormatInfoForUnitTest()
  msbfFinfo.byteOrder = ByteOrder.BigEndian
  msbfFinfo.bitOrder = BitOrder.MostSignificantBitFirst

  @Test def test3BitMSBF_01(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-OCTAL-MSBF")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(64)
    val bb = ByteBuffer.wrap(Misc.bits2Bytes("000 001 010 011 100 101 110 111"))
    val dis = InputSourceDataInputStream(bb)
    val res = decoder.decode(dis, msbfFinfo, cb)
    assertEquals(8, res)
    cb.flip()
    val digits = cb.toString()
    assertEquals("01234567", digits)
  }

  @Test def test3BitMSBF_02(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-OCTAL-MSBF")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(7) // not enough space for last digit
    val bb = ByteBuffer.wrap(Misc.bits2Bytes("1111 000 001 010 011 100 101 110 111"))
    val dis = InputSourceDataInputStream(bb)
    dis.skip(4, msbfFinfo)
    val res = decoder.decode(dis, msbfFinfo, cb)
    assertEquals(7, res)
    cb.flip()
    val digits = cb.toString()
    assertEquals("0123456", digits)
  }

  @Test def test3BitMSBF_03(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-OCTAL-MSBF")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(4)
    val bb = ByteBuffer.wrap(Misc.bits2Bytes("101 010 11"))
    val dis = InputSourceDataInputStream(bb)
    dis.setBitLimit0b(MaybeULong(3))
    val res = decoder.decode(dis, msbfFinfo, cb)
    assertEquals(1, res)
    cb.flip()
    val digits = cb.toString()
    assertEquals("5", digits)
  }

  @Test def test3BitMSBF_04(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-OCTAL-MSBF")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(40)
    val bb = ByteBuffer.wrap(Misc.bits2Bytes("1111 000 0|01 010 011| 100 101 11|0 111 1111"))
    val dis = InputSourceDataInputStream(bb)
    dis.setBitLimit0b(MaybeULong(25))
    dis.skip(4, msbfFinfo)
    val res = decoder.decode(dis, msbfFinfo, cb)
    assertEquals(7, res)
    cb.flip()
    val digits = cb.toString()
    assertEquals("0123456", digits)
  }

  @Test def test3BitLSBF_01(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-OCTAL-LSBF")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(64)
    val bb = ByteBuffer.wrap(Misc.bits2Bytes("10 001 000  1 100 011 0  111 110 10"))
    val dis = InputSourceDataInputStream(bb)
    val res = decoder.decode(dis, lsbfFinfo, cb)
    assertEquals(8, res)
    cb.flip()
    val digits = cb.toString()
    assertEquals("01234567", digits)
  }

  @Test def test3BitLSBF_02(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-OCTAL-LSBF")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(8)
    val bb =
      ByteBuffer.wrap(Misc.bits2Bytes("1 000 1111 | 011 010 00 | 10 101 100 | 0000 111 1"))
    val dis = InputSourceDataInputStream(bb)
    dis.skip(4, lsbfFinfo)
    val res = decoder.decode(dis, lsbfFinfo, cb)
    assertEquals(8, res)
    cb.flip()
    val digits = cb.toString()
    assertEquals("01234567", digits)
  }

  @Test def test3BitLSBF_03(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-OCTAL-LSBF")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(4)
    val bb = ByteBuffer.wrap(Misc.bits2Bytes("00 111 101"))
    val dis = InputSourceDataInputStream(bb)
    dis.setBitLimit0b(MaybeULong(4))
    val res = decoder.decode(dis, lsbfFinfo, cb)
    assertEquals(1, res)
    cb.flip()
    val digits = cb.toString()
    assertEquals("5", digits)
  }

  @Test def test3BitLSBF_04(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-OCTAL-LSBF")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(40)
    val bb =
      ByteBuffer.wrap(Misc.bits2Bytes("1 000 1111 | 011 010 00 | 10 101 100 | 0000 111 1"))
    val dis = InputSourceDataInputStream(bb)
    dis.setBitLimit0b(MaybeULong(28))
    dis.skip(4, lsbfFinfo)
    val res = decoder.decode(dis, lsbfFinfo, cb)
    assertEquals(8, res)
    cb.flip()
    val digits = cb.toString()
    assertEquals("01234567", digits)
  }

}
