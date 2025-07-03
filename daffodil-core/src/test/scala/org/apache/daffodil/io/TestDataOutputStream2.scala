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

import java.io.File
import java.nio.ByteBuffer

import org.apache.daffodil.lib.util.Maybe

import org.junit.Assert._
import org.junit.Test

class TestDataOutputStream2 {

  val beFinfo = FormatInfoForUnitTest()

  /*
   * BitBuffer tests
   */

  @Test def testPutBitBufferDirect0_BE_MSBF(): Unit = {

    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val out = DirectOrBufferedDataOutputStream(
      baos,
      null,
      false,
      4096,
      2000 * (1 << 20),
      new File("."),
      Maybe.Nope
    )

    val bb = ByteBuffer.wrap(List(0xa5.toByte, 0xbe.toByte, 0xef.toByte).toArray)
    bb.position(1)
    out.putBitBuffer(bb, 16, beFinfo)

    out.setFinished(beFinfo)

    val buf = baos.getBuf

    assertEquals(0xbe.toByte, buf(0))
    assertEquals(0xef.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  @Test def testPutBitBufferDirect1_BE_MSBF(): Unit = {

    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val out = DirectOrBufferedDataOutputStream(
      baos,
      null,
      false,
      4096,
      2000 * (1 << 20),
      new File("."),
      Maybe.Nope
    )

    val bb = ByteBuffer.wrap(List(0xa5.toByte, 0xbe.toByte, 0xef.toByte).toArray)
    bb.position(1)
    out.putBitBuffer(bb, 9, beFinfo)

    out.setFinished(beFinfo)

    val buf = baos.getBuf

    assertEquals(0xbe.toByte, buf(0))
    assertEquals(0x80.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  @Test def testPutBitBufferDirect7_BE_MSBF(): Unit = {

    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val out = DirectOrBufferedDataOutputStream(
      baos,
      null,
      false,
      4096,
      2000 * (1 << 20),
      new File("."),
      Maybe.Nope
    )

    val bb = ByteBuffer.wrap(List(0xa5.toByte, 0xbe.toByte, 0xef.toByte).toArray)
    bb.position(1)
    out.putBitBuffer(bb, 15, beFinfo)

    out.setFinished(beFinfo)

    val buf = baos.getBuf

    assertEquals(0xbe.toByte, buf(0))
    assertEquals(0xee.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }
}
