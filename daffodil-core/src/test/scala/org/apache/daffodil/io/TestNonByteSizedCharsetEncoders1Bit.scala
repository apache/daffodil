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
    val encoder = cs1Bit.newEncoder()
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
    val encoder = cs1Bit.newEncoder()
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
