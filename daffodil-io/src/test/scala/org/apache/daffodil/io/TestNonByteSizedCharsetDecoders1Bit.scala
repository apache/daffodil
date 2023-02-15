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

import org.apache.daffodil.io.processors.charset.BitsCharsetNonByteSizeDecoder
import org.apache.daffodil.io.processors.charset.CharsetUtils
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.lib.util.Misc

import org.junit.Assert._
import org.junit.Test

class TestNonByteSizedCharsetDecoders1Bit {

  val lsbfFinfo = FormatInfoForUnitTest()
  lsbfFinfo.byteOrder = ByteOrder.BigEndian
  lsbfFinfo.bitOrder = BitOrder.LeastSignificantBitFirst

  val msbfFinfo = FormatInfoForUnitTest()
  msbfFinfo.byteOrder = ByteOrder.BigEndian
  msbfFinfo.bitOrder = BitOrder.MostSignificantBitFirst

  @Test def test1BitMSBF_01(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-MSBF")
    val decoder = cs1Bit.newDecoder()
    val cb = CharBuffer.allocate(64)
    val bb = ByteBuffer.wrap(Misc.hex2Bytes("DEADBEEF"))
    val dis = InputSourceDataInputStream(bb)
    val res = decoder.decode(dis, msbfFinfo, cb)
    assertEquals(32, res)
    cb.flip()
    val bits = cb.toString()
    val hex = Misc.bytes2Hex(Misc.bits2Bytes(bits))
    assertEquals("DEADBEEF", hex)
  }

  @Test def test1BitMSBF_02(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-MSBF")
    val decoder = cs1Bit.newDecoder().asInstanceOf[BitsCharsetNonByteSizeDecoder]
    assertNotNull(decoder)
    val cb = CharBuffer.allocate(32)
    val bb = ByteBuffer.wrap(Misc.hex2Bytes("DEADBEEF57"))
    val dis = InputSourceDataInputStream(bb)
    dis.skip(4, msbfFinfo)
    val res = decoder.decode(dis, msbfFinfo, cb)
    assertEquals(32, res)
    cb.flip()
    val bits = cb.toString()
    val hex = Misc.bytes2Hex(Misc.bits2Bytes(bits))
    assertEquals("EADBEEF5", hex)
  }

  @Test def test1BitMSBF_03(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-MSBF")
    val decoder = cs1Bit.newDecoder()
    val cb = CharBuffer.allocate(4)
    val bb = ByteBuffer.wrap(Misc.hex2Bytes("57"))
    val dis = InputSourceDataInputStream(bb)
    dis.setBitLimit0b(MaybeULong(4))
    val res = decoder.decode(dis, msbfFinfo, cb)
    assertEquals(4, res)
    cb.flip()
    val bits = cb.toString() + "0000"
    val hex = Misc.bytes2Hex(Misc.bits2Bytes(bits))
    assertEquals("50", hex)
  }

  @Test def test1BitMSBF_04(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-MSBF")
    val decoder = cs1Bit.newDecoder()
    val cb = CharBuffer.allocate(40)
    val bb = ByteBuffer.wrap(Misc.hex2Bytes("DEADBEEF57"))
    val dis = InputSourceDataInputStream(bb)
    dis.setBitLimit0b(MaybeULong(36))
    dis.skip(4, msbfFinfo)
    val res = decoder.decode(dis, msbfFinfo, cb)
    assertEquals(32, res)
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
    val dis = InputSourceDataInputStream(bb)
    val res = decoder.decode(dis, lsbfFinfo, cb)
    // DE => 0111 1011 AD => 1011 0101 BE => 0111 1101 EF => 1111 0111
    assertEquals(32, res)
    cb.flip()
    val bits = cb.toString()
    val hex = Misc.bytes2Hex(Misc.bits2Bytes(bits))
    assertEquals("7BB57DF7", hex)
  }

  @Test def test1BitLSBF_02(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-LSBF")
    val decoder = cs1Bit.newDecoder()
    assertNotNull(decoder)
    val cb = CharBuffer.allocate(32)
    val bb = ByteBuffer.wrap(Misc.hex2Bytes("DEADBEEF57"))
    val dis = InputSourceDataInputStream(bb)
    dis.skip(4, lsbfFinfo)
    // skips the E of the DE byte
    // writing RTL => 7 EF BE AD D
    // regroup as: 7E FB EA DD
    // Bits LSBF for this starting from right advancing LSBF 1 bit at a time:
    // DD => 1011 1011 EA => 0101 0111 FB => 1101 1111 7E => 0111 1110
    val res = decoder.decode(dis, lsbfFinfo, cb)
    assertEquals(32, res)
    cb.flip()
    val bits = cb.toString()
    val hex = Misc.bytes2Hex(Misc.bits2Bytes(bits))
    // BB 57 DF 7E
    assertEquals("BB57DF7E", hex)
  }

  @Test def test1BitLSBF_03(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-LSBF")
    val decoder = cs1Bit.newDecoder()
    val cb = CharBuffer.allocate(4)
    val bb = ByteBuffer.wrap(Misc.hex2Bytes("57"))
    val dis = InputSourceDataInputStream(bb)
    dis.setBitLimit0b(MaybeULong(4))
    val res = decoder.decode(dis, lsbfFinfo, cb)
    assertEquals(4, res)
    cb.flip()
    val bits = "0000" + cb.toString()
    val hex = Misc.bytes2Hex(Misc.bits2Bytes(bits))
    assertEquals("0E", hex)
  }

  @Test def test1BitLSBF_04(): Unit = {
    val cs1Bit = CharsetUtils.getCharset("X-DFDL-BITS-LSBF")
    val decoder = cs1Bit.newDecoder()
    val cb = CharBuffer.allocate(40)
    val bb = ByteBuffer.wrap(Misc.hex2Bytes("DEADBEEF57"))
    val dis = InputSourceDataInputStream(bb)
    dis.setBitLimit0b(MaybeULong(36))
    dis.skip(4, lsbfFinfo)
    // skips the E of the DE byte
    // writing RTL => 7 EF BE AD D
    // regroup as: 7E FB EA DD
    val res = decoder.decode(dis, lsbfFinfo, cb)
    assertEquals(32, res)
    cb.flip()
    val bits = cb.toString()
    val hex = Misc.bytes2Hex(Misc.bits2Bytes(bits))
    assertEquals("BB57DF7E", hex)
  }

}
