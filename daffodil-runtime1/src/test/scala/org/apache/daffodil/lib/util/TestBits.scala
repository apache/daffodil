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

package org.apache.daffodil.lib.util

import java.nio.ByteBuffer

import org.junit.Assert._
import org.junit.Test

class TestBits {

  @Test def testShiftLeftByteArray1(): Unit = {
    val bb = ByteBuffer.wrap(List(0x7c.toByte).toArray)
    Bits.shiftLeft(bb, 3)
    assertEquals(List(0xe0.toByte), bb.array.toList)
  }

  @Test def testShiftLeftByteArray2(): Unit = {
    val bb = ByteBuffer.wrap(List(0x7c.toByte, 0x3d.toByte).toArray)
    Bits.shiftLeft(bb, 3)
    assertEquals(List(0xe1.toByte, 0xe8.toByte), bb.array.toList)
  }

  @Test def testShiftLeftByteArray3(): Unit = {
    val bb = ByteBuffer.wrap(List(0x7c.toByte).toArray)
    Bits.shiftLeft(bb, 0)
    assertEquals(List(0x7c.toByte), bb.array.toList)
  }

  @Test def testShiftRightByteArray1(): Unit = {
    val bb = ByteBuffer.wrap(List(0x7c.toByte).toArray)
    Bits.shiftRight(bb, 3)
    assertEquals(List(0x0f.toByte), bb.array.toList)
  }

  @Test def testShiftRightByteArray2(): Unit = {
    val bb = ByteBuffer.wrap(List(0x7c.toByte).toArray)
    Bits.shiftRight(bb, 0)
    assertEquals(List(0x7c.toByte), bb.array.toList)
  }

  @Test def testShiftRightByteArray3(): Unit = {
    val bb = ByteBuffer.wrap(List(0x7c.toByte, 0x3d.toByte).toArray)
    Bits.shiftRight(bb, 3)
    assertEquals(List(0x0f.toByte, 0x87.toByte), bb.array.toList)
  }

  @Test def testShiftRightByteArray4(): Unit = {
    val bb = ByteBuffer.wrap(List(0x7c.toByte, 0x3d.toByte, 0x42.toByte).toArray)
    Bits.shiftRight(bb, 3)
    assertEquals(List(0x0f.toByte, 0x87.toByte, 0xa8.toByte), bb.array.toList)
  }
}
