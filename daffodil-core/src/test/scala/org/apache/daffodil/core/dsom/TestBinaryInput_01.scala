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

package org.apache.daffodil.core.dsom

import java.math.{ BigInteger => JBigInt }
import java.nio.ByteBuffer

import org.apache.daffodil.io.DataInputStream
import org.apache.daffodil.io.FakeFormatInfo
import org.apache.daffodil.io.FormatInfo
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.lib.util.Misc

import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Test

// Do no harm number 16 of 626 fail in regression, 154 in total of 797

class TestBinaryInput_01 {

  var startOver: DataInputStream.Mark = null
  var dis: DataInputStream = null

  def fromBytes(
    ba: Array[Byte],
    byteOrd: ByteOrder,
    bitOrd: BitOrder
  ): (DataInputStream, FakeFormatInfo) = {
    dis = InputSourceDataInputStream(ba)
    val finfo = new FakeFormatInfo(bitOrd, byteOrd)
    startOver = dis.mark("TestBinaryInput_01")
    (dis, finfo)
  }

  def fromString(str: String, byteOrd: ByteOrder, bitOrd: BitOrder) = {
    dis = InputSourceDataInputStream(ByteBuffer.wrap(str.getBytes("utf-8")))
    val finfo = new FakeFormatInfo(bitOrd, byteOrd)
    startOver = dis.mark("TestBinaryInput_01")
    (dis, finfo)
  }

  @After def shutDown(): Unit = {
    dis.discard(startOver)
  }

  def getLong(finfo: FormatInfo, dis: DataInputStream, offset: Int, len: Int) = {
    dis.reset(startOver)
    startOver = dis.mark("TestBinaryInput_01")
    if (offset > 0) dis.skip(offset, finfo)
    dis.getUnsignedLong(len, finfo).toLong
  }

  def getBigInt(finfo: FormatInfo, dis: DataInputStream, offset: Int, len: Int) = {
    dis.reset(startOver)
    startOver = dis.mark("TestBinaryInput_01")
    if (offset > 0) dis.skip(offset, finfo)
    dis.getSignedBigInt(len, finfo)
  }

  /*** DFDL-334 ***/
  // Verify Bit Extraction
  val msbFirst = BitOrder.MostSignificantBitFirst
  val lsbFirst = BitOrder.LeastSignificantBitFirst
  val BE = ByteOrder.BigEndian
  val LE = ByteOrder.LittleEndian

  @Test def testOneBit1(): Unit = {
    val (dis, finfo) = fromBytes(Misc.bits2Bytes(Seq("00000011")), BE, msbFirst)

    val bit1 = getLong(finfo, dis, 6, 2)
    assertEquals(3, bit1)
    val bit2 = getLong(finfo, dis, 4, 2)
    assertEquals(0, bit2)
  }

  @Test def testOneBit2(): Unit = {
    val (dis, finfo) = fromBytes(Misc.bits2Bytes(Seq("11000000")), BE, msbFirst)

    val bit1 = getLong(finfo, dis, 0, 2)
    assertEquals(3, bit1)
    val bit2 = getLong(finfo, dis, 2, 2)
    assertEquals(0, bit2)
  }

  @Test def testOneBit3(): Unit = {
    val (dis, finfo) = fromBytes(Misc.bits2Bytes(Seq("00000011")), BE, msbFirst)

    val n = getLong(finfo, dis, 6, 2)
    assertEquals(3, n)
  }

  @Test
  def testBufferBitExtraction(): Unit = {
    val (dis, finfo) = fromString("3", BE, msbFirst)

    assertEquals(3, getLong(finfo, dis, 1, 3))
  }

  @Test
  def testBufferBitExtractionShift(): Unit = {
    val (dis, finfo) = fromString("3", BE, msbFirst)

    assertEquals(12, getLong(finfo, dis, 2, 4))
  }

  @Test
  def testBufferLeastSignificantBitExtractionShift(): Unit = {
    val (dis, finfo) = fromString("4", BE, msbFirst)

    assertEquals(4, getLong(finfo, dis, 5, 3))
  }

  // Verify aligned byte/short/int/long/bigint extraction
  @Test
  def testBufferByteBigEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("3", BE, msbFirst)

    assertEquals(51, getLong(finfo, dis, 0, 8))
  }

  @Test
  def testBufferByteLittleEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("3", LE, msbFirst)

    assertEquals(51, getLong(finfo, dis, 0, 8))
  }

  @Test
  def testBufferShortBigEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("Om", BE, msbFirst)
    assertEquals(20333, getLong(finfo, dis, 0, 16))
  }

  @Test
  def testBufferShortLittleEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("Om", LE, msbFirst)
    assertEquals(27983, getLong(finfo, dis, 0, 16))
  }

  @Test
  def testBufferIntBigEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("Help", BE, msbFirst)
    assertEquals(1214606448, getLong(finfo, dis, 0, 32))
  }

  @Test
  def testBufferIntLittleEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("Help", LE, msbFirst)
    assertEquals(1886152008, getLong(finfo, dis, 0, 32))
  }

  @Test
  def testBufferLongBigEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("Harrison", BE, msbFirst)
    assertEquals(JBigInt.valueOf(5215575679192756078L), getBigInt(finfo, dis, 0, 64))
  }

  @Test
  def testBufferLongLittleEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("Harrison", LE, msbFirst)
    assertEquals(JBigInt.valueOf(7957705963315814728L), getBigInt(finfo, dis, 0, 64))
  }

  @Test
  def testBufferBigIntBigEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("Thirty-two character long string", BE, msbFirst)
    val bigInt = getBigInt(finfo, dis, 0, 256)
    assertEquals(
      new JBigInt(
        "38178759144797737047702418052138682097409903778432721523410841200294898527847"
      ),
      bigInt
    )
  }

  @Test
  def testBufferBigIntLittleEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("Thirty-two character long string", LE, msbFirst)
    assertEquals(
      new JBigInt(
        "46783304350265979503919546124020768339208030665592297039403372142674722777172"
      ),
      getBigInt(finfo, dis, 0, 256)
    )
  }

  // Aligned but not full string
  @Test
  def testBufferPartialIntBigEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("SBT", BE, msbFirst)
    assertEquals(5456468, getLong(finfo, dis, 0, 24))
  }

  @Test
  def testBufferPartialIntLittleEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("SBT", LE, msbFirst)
    assertEquals(5522003, getLong(finfo, dis, 0, 24))
  }

  // Non-Aligned 1 Byte or less
  @Test
  def testBufferBitNumberBigEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("3", BE, msbFirst)

    assertEquals(3, getLong(finfo, dis, 1, 3))
  }

  @Test
  def testBufferBitNumberLittleEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("3", LE, msbFirst)

    assertEquals(3, getLong(finfo, dis, 1, 3))
  }

  @Test
  def testBufferBitByteBigEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("3>", BE, msbFirst)
    assertEquals(204, getLong(finfo, dis, 2, 8))
  }

  @Test
  def testBufferBitByteLittleEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("3>", LE, msbFirst)
    assertEquals(0xcc, getLong(finfo, dis, 2, 8))
  }

  // Non-Aligned multi-byte
  @Test
  def testBufferPartialInt22At0BigEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("SBT", BE, msbFirst)
    assertEquals(1364117, getLong(finfo, dis, 0, 22))
  }

  @Test
  def testBufferPartialInt22At0LittleEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("SBT", LE, msbFirst)
    assertEquals(0x154253, getLong(finfo, dis, 0, 22))
    // Corrected. Was 544253, but that omits shifting the most significant byte left 2
    // because the field is only 22 long.
  }

  @Test
  def testBufferPartialInt22At2BigEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("SBT", BE, msbFirst)
    assertEquals(0x134254, getLong(finfo, dis, 2, 22))
  }

  @Test
  def testBufferPartialInt22At2LittleEndianExtraction(): Unit = {
    val (dis, finfo) = fromString("SBT", LE, msbFirst)
    assertEquals(0x14094d, getLong(finfo, dis, 2, 22)) // Corrected.
    // Was 0x50094d, but that omits shifting the most significant byte >> 2 to
    // add the bits on the most significant side, not the least significant side.
  }

  @Test def testOneBit1LSBFirst(): Unit = {
    val (dis, finfo) = fromBytes(Misc.bits2Bytes(Seq("01100000")), LE, lsbFirst)

    val bit1 = getLong(finfo, dis, 5, 2)
    assertEquals(3, bit1)
    val bit2 = getLong(finfo, dis, 4, 2)
    assertEquals(2, bit2)
  }

  @Test def testOneBit2LSBFirst(): Unit = {
    val (dis, finfo) = fromBytes(Misc.bits2Bytes(Seq("01010000")), LE, lsbFirst)

    val bit1 = getLong(finfo, dis, 5, 2)
    assertEquals(2, bit1)
    val bit2 = getLong(finfo, dis, 4, 2)
    assertEquals(1, bit2)
  }

  @Test def testOneBit3LSBFirst(): Unit = {
    val (dis, finfo) = fromBytes(JBigInt.valueOf(0xe4567a).toByteArray, LE, lsbFirst)

    val bit1 = getLong(finfo, dis, 13, 12)
    assertEquals(0x2b7, bit1)
  }
}
