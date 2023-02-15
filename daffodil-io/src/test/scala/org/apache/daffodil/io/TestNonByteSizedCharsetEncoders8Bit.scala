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
import org.apache.daffodil.lib.util.Misc

import org.junit.Assert._
import org.junit.Test

class TestNonByteSizedCharsetEncoders8Bit {

  @Test def test8BitMSBF_01(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-ISO-88591-8-BIT-PACKED-MSB-FIRST")
    val encoder = cs.newEncoder()
    val cb = CharBuffer.wrap("01234567")
    val expectedBytes = Misc
      .bits2Bytes("00110000 00110001 00110010 00110011 00110100 00110101 00110110 00110111")
      .toList
    val bb = ByteBuffer.allocate(8)
    val res = encoder.encode(cb, bb, false)
    assertTrue(res.isUnderflow())
    bb.flip()
    val actualBytes = bb.array().toList
    assertEquals(expectedBytes, actualBytes)
  }

  @Test def test8BitMSBF_02(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-ISO-88591-8-BIT-PACKED-MSB-FIRST")
    val encoder = cs.newEncoder()
    val cb = CharBuffer.wrap("012345677")
    val expectedBytes = Misc
      .bits2Bytes("00110000 00110001 00110010 00110011 00110100 00110101 00110110 00110111")
      .toList
    val bb = ByteBuffer.allocate(8) // not enough space for last digit
    val res = encoder.encode(cb, bb, false)
    assertTrue(res.isOverflow())
    bb.flip()
    val actualBytes = bb.array().toList
    assertEquals(expectedBytes, actualBytes)
  }

  @Test def test8BitLSBF_01(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-ISO-88591-8-BIT-PACKED-LSB-FIRST")
    val encoder = cs.newEncoder()
    val cb = CharBuffer.wrap("01234567")
    val expectedBytes = Misc
      .bits2Bytes("00110000 00110001 00110010 00110011 00110100 00110101 00110110 00110111")
      .toList
    val bb = ByteBuffer.allocate(8)
    val res = encoder.encode(cb, bb, false)
    assertTrue(res.isUnderflow())
    bb.flip()
    val actualBytes = bb.array().toList
    assertEquals(expectedBytes, actualBytes)
  }

  @Test def test8BitLSBF_02(): Unit = {
    val cs = CharsetUtils.getCharset("X-DFDL-ISO-88591-8-BIT-PACKED-LSB-FIRST")
    val encoder = cs.newEncoder()
    assertNotNull(encoder)
    val cb = CharBuffer.wrap("012345677")
    val expectedBytes = Misc
      .bits2Bytes("00110000 00110001 00110010 00110011 00110100 00110101 00110110 00110111")
      .toList
    val bb = ByteBuffer.allocate(8) // not enough space for last digit
    val res = encoder.encode(cb, bb, false)
    assertTrue(res.isOverflow())
    bb.flip()
    val actualBytes = bb.array().toList
    assertEquals(expectedBytes, actualBytes)
  }
}
