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
import org.apache.daffodil.schema.annotation.props.gen.BitOrder

class TestBits2 {

  val MSBF = BitOrder.MostSignificantBitFirst
  val LSBF = BitOrder.LeastSignificantBitFirst

  @Test def testShiftHigher1MSBF(): Unit = {
    val bb = ByteBuffer.wrap(List(0xFF.toByte).toArray)
    Bits.shiftToHigherBitPosition(MSBF, bb, 1)
    assertEquals(List(0x7F.toByte), bb.array.toList)
  }

  @Test def testShiftHigher1LSBF(): Unit = {
    val bb = ByteBuffer.wrap(List(0xFF.toByte).toArray)
    Bits.shiftToHigherBitPosition(LSBF, bb, 1)
    assertEquals(List(0xFE.toByte), bb.array.toList)
  }

  @Test def testShiftHigher2MSBF(): Unit = {
    val bb = ByteBuffer.wrap(List(0xFF.toByte).toArray)
    Bits.shiftToHigherBitPosition(MSBF, bb, 2)
    assertEquals(List(0x3F.toByte), bb.array.toList)
  }

  @Test def testShiftHigher2LSBF(): Unit = {
    val bb = ByteBuffer.wrap(List(0xFF.toByte).toArray)
    Bits.shiftToHigherBitPosition(LSBF, bb, 2)
    assertEquals(List(0xFC.toByte), bb.array.toList)
  }

  @Test def testShiftHigher7MSBF(): Unit = {
    val bb = ByteBuffer.wrap(List(0xFF.toByte).toArray)
    Bits.shiftToHigherBitPosition(MSBF, bb, 7)
    assertEquals(List(0x01.toByte), bb.array.toList)
  }

  @Test def testShiftHigher7LSBF(): Unit = {
    val bb = ByteBuffer.wrap(List(0xFF.toByte).toArray)
    Bits.shiftToHigherBitPosition(LSBF, bb, 7)
    assertEquals(List(0x80.toByte), bb.array.toList)
  }

  ///////////////////////////////////////////////

  @Test def testShiftHigher1MSBF2(): Unit = {
    val bb = ByteBuffer.wrap(List(0xA5.toByte, 0xA5.toByte).toArray)
    Bits.shiftToHigherBitPosition(MSBF, bb, 1)
    assertEquals(List(0x52.toByte, 0xD2.toByte), bb.array.toList)
  }

  @Test def testShiftHigher1LSBF2(): Unit = {
    val bb = ByteBuffer.wrap(List(0xA5.toByte, 0xA5.toByte).toArray)
    Bits.shiftToHigherBitPosition(LSBF, bb, 1)
    assertEquals(List(0x4A.toByte, 0x4B), bb.array.toList)
  }

  @Test def testShiftHigher2MSBF2(): Unit = {
    val bb = ByteBuffer.wrap(List(0xA5.toByte, 0xA5.toByte).toArray)
    Bits.shiftToHigherBitPosition(MSBF, bb, 2)
    assertEquals(List(0x29.toByte, 0x69.toByte), bb.array.toList)
  }

  @Test def testShiftHigher2LSBF2(): Unit = {
    val bb = ByteBuffer.wrap(List(0xA5.toByte, 0xA5.toByte).toArray)
    Bits.shiftToHigherBitPosition(LSBF, bb, 2)
    assertEquals(List(0x94.toByte, 0x96.toByte), bb.array.toList)
  }

  @Test def testShiftHigher7MSBF2(): Unit = {
    val bb = ByteBuffer.wrap(List(0xA5.toByte, 0xA5.toByte).toArray)
    Bits.shiftToHigherBitPosition(MSBF, bb, 7)
    assertEquals(List(0x01.toByte, 0x4B.toByte), bb.array.toList)
  }

  @Test def testShiftHigher7LSBF2(): Unit = {
    val bb = ByteBuffer.wrap(List(0xA5.toByte, 0xA5.toByte).toArray)
    Bits.shiftToHigherBitPosition(LSBF, bb, 7)
    assertEquals(List(0x80.toByte, 0xD2.toByte), bb.array.toList)
  }

}
