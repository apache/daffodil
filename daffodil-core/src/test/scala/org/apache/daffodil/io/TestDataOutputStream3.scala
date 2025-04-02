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

import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.lib.util.Maybe

import org.junit.Assert._
import org.junit.Test

class TestDataOutputStream3 {

  val lsbfFinfo = FormatInfoForUnitTest()
  lsbfFinfo.byteOrder = ByteOrder.LittleEndian
  lsbfFinfo.bitOrder = BitOrder.LeastSignificantBitFirst

  def newDirectOrBufferedDataOutputStream(
    jos: java.io.OutputStream,
    creator: DirectOrBufferedDataOutputStream,
    bo: BitOrder = BitOrder.MostSignificantBitFirst
  ) = {
    val os = DirectOrBufferedDataOutputStream(
      jos,
      creator,
      false,
      4096,
      2000 * (1 << 20),
      new File("."),
      Maybe.Nope
    )
    os.setPriorBitOrder(bo)
    os
  }

  @Test def testPutLongDirect1_LE_LSBF(): Unit = {

    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val out = newDirectOrBufferedDataOutputStream(baos, null)

    out.putLong(-1L, 32, lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(-1, buf(0))
    assertEquals(-1, buf(1))
    assertEquals(-1, buf(2))
    assertEquals(-1, buf(3))
    if (buf.length > 4)
      assertEquals(0, buf(4))

  }

  @Test def testPutLongDirect1Bit_LE_LSBF(): Unit = {

    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val out = newDirectOrBufferedDataOutputStream(baos, null)

    out.putLong(1, 1, lsbfFinfo)

    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(0x01.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongDirect2Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val out = newDirectOrBufferedDataOutputStream(baos, null)

    out.putLong(3, 2, lsbfFinfo)

    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(0x03.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongDirect7Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val out = newDirectOrBufferedDataOutputStream(baos, null)

    out.putLong(0xa5, 7, lsbfFinfo)

    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(0x25.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongDirect8Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val out = newDirectOrBufferedDataOutputStream(baos, null)

    out.putLong(0xa5, 8, lsbfFinfo)

    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(0xa5.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongDirect9Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val out = newDirectOrBufferedDataOutputStream(baos, null)

    out.putLong(0xa5a5, 9, lsbfFinfo)

    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(0xa5.toByte, buf(0))
    assertEquals(0x01.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  @Test def testPutLongDirect63Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val out = newDirectOrBufferedDataOutputStream(baos, null)

    out.putLong(0xa5a5a5a5a5a5a5a5L, 63, lsbfFinfo)

    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    val res = 0x25a5a5a5a5a5a5a5L
    var i = 0
    while (i < 8) {
      val expected = "%x".format(((res >> (8 * i)) & 0xff).toByte)
      val actual = "%x".format(buf(i))
      assertEquals(expected, actual)
      i += 1
    }
    if (buf.length > 8)
      assertEquals(0, buf(9))

  }

  @Test def testPutLongDirect64Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val out = newDirectOrBufferedDataOutputStream(baos, null)

    out.putLong(0xa5a5a5a5a5a5a5a5L, 64, lsbfFinfo)

    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    val res = 0xa5a5a5a5a5a5a5a5L
    var i = 0
    while (i < 8) {
      val expected = "%x".format(((res >> (8 * i)) & 0xff).toByte)
      val actual = "%x".format(buf(i))
      assertEquals(expected, actual)
      i += 1
    }
    if (buf.length > 8)
      assertEquals(0, buf(9))

  }

  /////////////////////////////////////////////////////
  // Tests of Buffered and putLong
  /////////////////////////////////////////////////////

  @Test def testPutLongBuffered1_LE_LSBF(): Unit = {

    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered()

    out.putLong(-1L, 32, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(-1, buf(0))
    assertEquals(-1, buf(1))
    assertEquals(-1, buf(2))
    assertEquals(-1, buf(3))
    if (buf.length > 4)
      assertEquals(0, buf(4))

  }

  @Test def testPutLongBuffered1Bit_LE_LSBF(): Unit = {

    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered()

    out.putLong(1, 1, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(0x01.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongBuffered2Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered()

    out.putLong(3, 2, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(0x03.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongBuffered7Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered()

    out.putLong(0xa5, 7, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(0x25.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongBuffered8Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered()

    out.putLong(0xa5, 8, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(0xa5.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongBuffered9Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered()

    out.putLong(0xa5a5, 9, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(0xa5.toByte, buf(0))
    assertEquals(0x01.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  @Test def testPutLongBuffered63Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered()

    out.putLong(0xa5a5a5a5a5a5a5a5L, 63, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    val res = 0x25a5a5a5a5a5a5a5L
    var i = 0
    while (i < 8) {
      val expected = "%x".format(((res >> (8 * i)) & 0xff).toByte)
      val actual = "%x".format(buf(i))
      assertEquals(expected, actual)
      i += 1
    }
    if (buf.length > 8)
      assertEquals(0, buf(9))

  }

  @Test def testPutLongBuffered64Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered()

    out.putLong(0xa5a5a5a5a5a5a5a5L, 64, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    val res = 0xa5a5a5a5a5a5a5a5L
    var i = 0
    while (i < 8) {
      val expected = "%x".format(((res >> (8 * i)) & 0xff).toByte)
      val actual = "%x".format(buf(i))
      assertEquals(expected, actual)
      i += 1
    }
    if (buf.length > 8)
      assertEquals(0, buf(8))

  }

  /////////////////////////////////////////////////////
  // Tests of Direct + Buffered and putLong
  /////////////////////////////////////////////////////

  @Test def testPutLongDirectAndBuffered1_LE_LSBF(): Unit = {

    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered()

    out.putLong(-1L, 32, lsbfFinfo)
    direct.putLong(-1L, 32, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(-1, buf(0))
    assertEquals(-1, buf(1))
    assertEquals(-1, buf(2))
    assertEquals(-1, buf(3))
    assertEquals(-1, buf(4))
    assertEquals(-1, buf(5))
    assertEquals(-1, buf(6))
    assertEquals(-1, buf(7))
    if (buf.length > 8)
      assertEquals(0, buf(8))

  }

  @Test def testPutLongDirectAndBuffered1Bit_LE_LSBF(): Unit = {

    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val direct =
      newDirectOrBufferedDataOutputStream(baos, null, BitOrder.LeastSignificantBitFirst)
    val out = direct.addBuffered()

    out.putLong(1, 1, lsbfFinfo)
    direct.putLong(1, 1, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(0x03.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongDirectAndBuffered2Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val direct =
      newDirectOrBufferedDataOutputStream(baos, null, BitOrder.LeastSignificantBitFirst)
    val out = direct.addBuffered()

    out.putLong(3, 2, lsbfFinfo)
    direct.putLong(2, 2, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(0x0e.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

  @Test def testPutLongDirectAndBuffered7Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    direct.setPriorBitOrder(BitOrder.LeastSignificantBitFirst)
    val out = direct.addBuffered()

    out.putLong(0xa5, 7, lsbfFinfo)
    direct.putLong(0xa5, 7, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(0xa5.toByte, buf(0))
    assertEquals(0x12.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  @Test def testPutLongDirectAndBuffered8Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered()

    out.putLong(0xa5, 8, lsbfFinfo)
    direct.putLong(0xa5, 8, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(0xa5.toByte, buf(0))
    assertEquals(0xa5.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  @Test def testPutLongDirectAndBuffered9Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val direct =
      newDirectOrBufferedDataOutputStream(baos, null, BitOrder.LeastSignificantBitFirst)
    val out = direct.addBuffered()

    out.putLong(0xdead, 9, lsbfFinfo)
    direct.putLong(0xbeef, 9, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(0xef.toByte, buf(0))
    assertEquals(0x5a.toByte, buf(1))
    assertEquals(0x01.toByte, buf(2))
    if (buf.length > 3)
      assertEquals(0, buf(3))

  }

  @Test def testPutLongDirectAndBuffered63BitPlus1Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val direct =
      newDirectOrBufferedDataOutputStream(baos, null, BitOrder.LeastSignificantBitFirst)
    val out = direct.addBuffered()

    out.putLong(0xa5a5a5a5a5a5a5a5L, 63, lsbfFinfo)
    direct.putLong(1, 1, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    val res = (0xa5a5a5a5a5a5a5a5L << 1) + 1
    var i = 0
    while (i < 8) {
      val expected = "%x".format(((res >> (8 * i)) & 0xff).toByte)
      val actual = "%x".format(buf(i))
      assertEquals(expected, actual)
      i += 1
    }
    if (buf.length > 8)
      assertEquals(0, buf(9))

  }

  @Test def testPutLongDirectAndBuffered63BitPlus63Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val direct =
      newDirectOrBufferedDataOutputStream(baos, null, BitOrder.LeastSignificantBitFirst)
    val out = direct.addBuffered()

    out.putLong(0xa5a5a5a5a5a5a5a5L, 63, lsbfFinfo)
    direct.putLong(0xa5a5a5a5a5a5a5a5L, 63, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    val res1 = 0xa5a5a5a5a5a5a5a5L
    var i = 0
    while (i < 8) {
      val expected = "%x".format(((res1 >> (8 * i)) & 0xff).toByte)
      val actual = "%x".format(buf(i))
      assertEquals(expected, actual)
      i += 1
    }
    val res2 = 0x25a5a5a5a5a5a5a5L >>> 1
    i = 0
    while (i < 8) {
      val expected = "%x".format(((res2 >> (8 * i)) & 0xff).toByte)
      val actual = "%x".format(buf(i + 8))
      assertEquals(expected, actual)
      i += 1
    }

    if (buf.length > 16)
      assertEquals(0, buf(16))

  }

  @Test def testPutLongDirectAndBuffered64BitPlus64Bit_LE_LSBF(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val direct = newDirectOrBufferedDataOutputStream(baos, null)
    val out = direct.addBuffered()

    out.putLong(0xa5a5a5a5a5a5a5a5L, 64, lsbfFinfo)
    direct.putLong(0xa5a5a5a5a5a5a5a5L, 64, lsbfFinfo)

    direct.setFinished(lsbfFinfo)
    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    val res1 = 0xa5a5a5a5a5a5a5a5L
    var i = 0
    while (i < 8) {
      val expected = "%x".format(((res1 >> (8 * i)) & 0xff).toByte)
      val actual = "%x".format(buf(i))
      assertEquals(expected, actual)
      i += 1
    }
    val res2 = 0xa5a5a5a5a5a5a5a5L
    i = 0
    while (i < 8) {
      val expected = "%x".format(((res2 >> (8 * i)) & 0xff).toByte)
      val actual = "%x".format(buf(i + 8))
      assertEquals(expected, actual)
      i += 1
    }

    if (buf.length > 16)
      assertEquals(0, buf(16))

  }

  @Test def testPutLong5_4Bits_LE_LSBF(): Unit = {

    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val out = newDirectOrBufferedDataOutputStream(baos, null)

    out.putLong(5L, 4, lsbfFinfo)

    out.setFinished(lsbfFinfo)

    val buf = baos.getBuf

    assertEquals(0x05.toByte, buf(0))
    if (buf.length > 1)
      assertEquals(0, buf(1))

  }

}
