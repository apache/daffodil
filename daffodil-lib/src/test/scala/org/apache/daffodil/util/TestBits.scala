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

package org.apache.daffodil.util

import org.junit.Test
import org.junit.Assert._
import java.nio.ByteBuffer

class TestBits {

  @Test def testShiftLeftByteArray1(): Unit = {
    val bb = ByteBuffer.wrap(List(0x7C.toByte).toArray)
    Bits.shiftLeft(bb, 3)
    assertEquals(List(0xE0.toByte), bb.array.toList)
  }

  @Test def testShiftLeftByteArray2(): Unit = {
    val bb = ByteBuffer.wrap(List(0x7C.toByte, 0x3D.toByte).toArray)
    Bits.shiftLeft(bb, 3)
    assertEquals(List(0xE1.toByte, 0xE8.toByte), bb.array.toList)
  }

  @Test def testShiftLeftByteArray3(): Unit = {
    val bb = ByteBuffer.wrap(List(0x7C.toByte).toArray)
    Bits.shiftLeft(bb, 0)
    assertEquals(List(0x7c.toByte), bb.array.toList)
  }

  @Test def testShiftRightByteArray1(): Unit = {
    val bb = ByteBuffer.wrap(List(0x7C.toByte).toArray)
    Bits.shiftRight(bb, 3)
    assertEquals(List(0x0F.toByte), bb.array.toList)
  }

  @Test def testShiftRightByteArray2(): Unit = {
    val bb = ByteBuffer.wrap(List(0x7C.toByte).toArray)
    Bits.shiftRight(bb, 0)
    assertEquals(List(0x7C.toByte), bb.array.toList)
  }

  @Test def testShiftRightByteArray3(): Unit = {
    val bb = ByteBuffer.wrap(List(0x7C.toByte, 0x3D.toByte).toArray)
    Bits.shiftRight(bb, 3)
    assertEquals(List(0x0F.toByte, 0x87.toByte), bb.array.toList)
  }

  @Test def testShiftRightByteArray4(): Unit = {
    val bb = ByteBuffer.wrap(List(0x7C.toByte, 0x3D.toByte, 0x42.toByte).toArray)
    Bits.shiftRight(bb, 3)
    assertEquals(List(0x0F.toByte, 0x87.toByte, 0xA8.toByte), bb.array.toList)
  }
}