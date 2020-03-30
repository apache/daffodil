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

import org.junit.Assert._
import org.junit.Test

import org.apache.daffodil.processors.charset.CharsetUtils
import org.apache.daffodil.util.Misc
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.schema.annotation.props.gen.BitOrder

class TestNonByteSizedCharsetDecoders8Bit {

  val lsbfFinfo = FormatInfoForUnitTest()
  lsbfFinfo.byteOrder = ByteOrder.BigEndian
  lsbfFinfo.bitOrder = BitOrder.LeastSignificantBitFirst

  val msbfFinfo = FormatInfoForUnitTest()
  msbfFinfo.byteOrder = ByteOrder.BigEndian
  msbfFinfo.bitOrder = BitOrder.MostSignificantBitFirst


  @Test def test8BitMSBF_01(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-ISO-88591-8-BIT-PACKED-MSB-FIRST")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(8)
    val bb = ByteBuffer.wrap(Misc.bits2Bytes("00110000 00110001 00110010 00110011 00110100 00110101 00110110 00110111"))
    val dis = InputSourceDataInputStream(bb)
    val res = decoder.decode(dis, msbfFinfo, cb)
    assertEquals(8, res)
    cb.flip()
    val digits = cb.toString()
    assertEquals("01234567", digits)
  }

  @Test def test8BitMSBF_02(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-ISO-88591-8-BIT-PACKED-MSB-FIRST")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(7) // not enough space for last digit
    val bb = ByteBuffer.wrap(Misc.bits2Bytes("00110000 00110001 00110010 00110011 00110100 00110101 00110110 00110111"))
    val dis = InputSourceDataInputStream(bb)
    //dis.skip(4, msbfFinfo)
    val res = decoder.decode(dis, msbfFinfo, cb)
    assertEquals(7, res)
    cb.flip()
    val digits = cb.toString()
    assertEquals("0123456", digits)
  }

  @Test def test8BitLSBF_01(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-ISO-88591-8-BIT-PACKED-LSB-FIRST")
    val decoder = cs.newDecoder()
    val cb = CharBuffer.allocate(64)
    val bb = ByteBuffer.wrap(Misc.bits2Bytes("00110000 00110001 00110010 00110011 00110100 00110101 00110110 00110111"))
    val dis = InputSourceDataInputStream(bb)
    val res = decoder.decode(dis, lsbfFinfo, cb)
    assertEquals(8, res)
    cb.flip()
    val digits = cb.toString()
    assertEquals("01234567", digits)
  }
}
