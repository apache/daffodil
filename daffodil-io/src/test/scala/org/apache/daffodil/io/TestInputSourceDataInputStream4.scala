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

import org.junit.Test
import org.junit.Assert._
import java.nio.ByteBuffer
import org.apache.daffodil.schema.annotation.props.gen.ByteOrder

class TestInputSourceDataInputStream4 {

  val leFinfo = FormatInfoForUnitTest()
  leFinfo.byteOrder = ByteOrder.LittleEndian

  @Test def testLittleEndianFloat1(): Unit = {
    val bb = ByteBuffer.allocate(4)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asFloatBuffer()
    fb.position(0)
    fb.put(1.125.toFloat)
    val bytes = bb.array()
    val dis = InputSourceDataInputStream(bytes)

    val expected = 1.125.toFloat
    val md = dis.getBinaryFloat(leFinfo)
    assertEquals(expected, md, 0.01)
    assertEquals(32, dis.bitPos0b)
  }

  @Test def testLittleEndianDouble1(): Unit = {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asDoubleBuffer()
    fb.position(0)
    fb.put(1.125)
    val bytes = bb.array()
    val dis = InputSourceDataInputStream(bytes)
    val expected = 1.125
    val md = dis.getBinaryDouble(leFinfo)
    assertEquals(expected, md, 0.01)
    assertEquals(64, dis.bitPos0b)
  }

  @Test def testLittleEndianLong1(): Unit = {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val expected = 0x0102030405060708L
    fb.put(expected)
    val bytes = bb.array()
    val dis = InputSourceDataInputStream(bytes)
    val md = dis.getSignedLong(64, leFinfo)
    assertEquals(expected, md)
    assertEquals(64, dis.bitPos0b)
  }

  @Test def testLittleEndianLong2(): Unit = {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val expected = 0x01020304L
    fb.put(expected)
    val bytes = bb.array()
    val dis = InputSourceDataInputStream(bytes)
    val md = dis.getSignedLong(32, leFinfo)
    assertEquals(expected, md)
    assertEquals(32, dis.bitPos0b)
  }

  @Test def testLittleEndianLong3(): Unit = {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val expected = 0x8070605040302010L
    fb.put(expected)
    val bytes = bb.array()
    val dis = InputSourceDataInputStream(bytes)
    val md = dis.getSignedLong(64, leFinfo)
    assertEquals(expected, md)
    assertEquals(64, dis.bitPos0b)
  }

  @Test def testLittleEndianLong4(): Unit = {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val data = 0x0000000080706050L
    fb.put(data)
    val bytes = bb.array()
    val dis = InputSourceDataInputStream(bytes)
    val md = dis.getSignedLong(32, leFinfo)
    assertEquals(0x80706050.toInt, md)
    assertEquals(32, dis.bitPos0b)
  }

  @Test def testLittleEndianLong5(): Unit = {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.BIG_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val data = 0x8000000000000000L
    fb.put(data)
    val bytes = bb.array()
    val dis = InputSourceDataInputStream(bytes)
    val md = dis.getSignedLong(1, leFinfo)
    assertEquals(1, md)
    assertEquals(1, dis.bitPos0b)
  }
}
