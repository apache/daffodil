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

import org.apache.daffodil.io.processors.charset.BitsCharsetDecoderUnalignedCharDecodeException
import org.apache.daffodil.lib.Implicits.intercept
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.MaybeULong

import org.junit.Assert._
import org.junit.Test

class TestInputSourceDataInputStream6 {

  val beFinfo = FormatInfoForUnitTest()

  val leFinfo = FormatInfoForUnitTest()
  leFinfo.byteOrder = ByteOrder.LittleEndian

  val lsbfFinfo = FormatInfoForUnitTest()
  lsbfFinfo.byteOrder = ByteOrder.LittleEndian
  lsbfFinfo.bitOrder = BitOrder.LeastSignificantBitFirst

  @Test def testUnalignedByteArrayMSBFirst(): Unit = {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.BIG_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val data = 0xf102030405060708L
    fb.put(data)
    val bytes = bb.array()
    val dis = InputSourceDataInputStream(bytes)
    dis.skip(1, beFinfo) // move over one bit
    val arr = dis.getByteArray((8 * 8) - 1, beFinfo)
    assertEquals(8, arr.size)
    assertEquals(0xe2.toByte, arr(0))
    assertEquals(0x04.toByte, arr(1))
    assertEquals(0x06.toByte, arr(2))
    assertEquals(0x08.toByte, arr(3))
    assertEquals(0x0a.toByte, arr(4))
    assertEquals(0x0c.toByte, arr(5))
    assertEquals(0x0e.toByte, arr(6))
    assertEquals(0x10.toByte, arr(7))
    assertEquals(64, dis.bitPos0b)
  }

  @Test def testUnalignedByteArrayLittleEndianMSBFirst(): Unit = {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val data = 0xf102030405060708L
    fb.put(data)
    val bytes = bb.array()
    val dis = InputSourceDataInputStream(bytes)
    dis.skip(1, leFinfo)
    assertEquals(1, dis.bitPos0b)
    val arr = dis.getByteArray((8 * 8) - 1, leFinfo)
    assertEquals(8, arr.size)
    assertEquals(0xe2.toByte, arr(0))
    assertEquals(0x05.toByte, arr(1))
    assertEquals(0x06.toByte, arr(2))
    assertEquals(0x08.toByte, arr(3))
    assertEquals(0x0a.toByte, arr(4))
    assertEquals(0x0c.toByte, arr(5))
    assertEquals(0x0e.toByte, arr(6))
    assertEquals(0x10.toByte, arr(7))
    assertEquals(64, dis.bitPos0b)
  }

  @Test def testUnalignedByteArrayLittleEndianLSBFirst(): Unit = {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val data = 0x01020304050607f8L
    fb.put(data)
    val bytes = bb.array()
    val dis = InputSourceDataInputStream(bytes)
    dis.skip(1, lsbfFinfo)
    val arr = dis.getByteArray(((8 * 8) - 1), lsbfFinfo)
    assertEquals(8, arr.size)
    assertEquals(0x00.toByte, arr(0))
    assertEquals(0x81.toByte, arr(1))
    assertEquals(0x01.toByte, arr(2))
    assertEquals(0x82.toByte, arr(3))
    assertEquals(0x02.toByte, arr(4))
    assertEquals(0x83.toByte, arr(5))
    assertEquals(0x03.toByte, arr(6))
    assertEquals(0xfc.toByte, arr(7))
    assertEquals(64, dis.bitPos0b)
  }

  /**
   * Tests of unaligned char buffer - when charset has mandatory 8-bit alignment
   *
   * These just ensure that we move over to the mandatory alignment before decoding
   * any characters.
   */
  @Test def testGetSomeString1(): Unit = {
    val dis = InputSourceDataInputStream("01".getBytes())
    dis.getSignedLong(1, beFinfo)
    val e = intercept[BitsCharsetDecoderUnalignedCharDecodeException] {
      dis.getSomeString(1, beFinfo)
    }
    val msg = e.getMessage()
    assertTrue(msg.toLowerCase.contains("not byte aligned"))
    assertEquals(2, e.bitAlignment1b)
    assertEquals(1, e.bytePos1b)
    assertEquals(2, e.bitPos1b)
  }

  @Test def testGetSomeString2(): Unit = {
    val dis = InputSourceDataInputStream("0年月日".getBytes("utf-8"))
    dis.getSignedLong(4, beFinfo)
    val e = intercept[BitsCharsetDecoderUnalignedCharDecodeException] {
      dis.getSomeString(3, beFinfo)
    }
    val msg = e.getMessage()
    assertTrue(msg.toLowerCase.contains("not byte aligned"))
    assertEquals(5, e.bitAlignment1b)
  }

  @Test def testGetSomeStringDataEndsMidByte(): Unit = {
    val dis = InputSourceDataInputStream("年月日".getBytes("utf-8"))
    dis.setBitLimit0b(MaybeULong((8 * 6) + 2)) // 2 extra bits after first 2 chars
    val ms = dis.getSomeString(3, beFinfo)
    assertTrue(ms.isDefined)
    val s = ms.get
    assertEquals(2, s.length)
    assertEquals('年', s(0))
    assertEquals('月', s(1))
    assertEquals(8 * 6, dis.bitPos0b)
  }

  @Test def testGetSomeStringDataEndsMidByte2(): Unit = {
    val dis = InputSourceDataInputStream("年月日".getBytes("utf-8"))
    dis.setBitLimit0b(MaybeULong((8 * 6) + 2)) // 2 extra bits after first 2 chars
    val ms = dis.getSomeString(3, beFinfo)
    assertTrue(ms.isDefined)
    val s = ms.get
    assertEquals(2, s.length)
    assertEquals('年', s(0))
    assertEquals('月', s(1))
    assertEquals(8 * 6, dis.bitPos0b)
    val ms2 = dis.getSomeString(3, beFinfo) // ask for next character
    assertEquals(Maybe.Nope, ms2)
  }

  @Test def testGetSomeStringDataEndsMidByte3(): Unit = {
    val dis = InputSourceDataInputStream("年月日".getBytes("utf-8"))
    dis.setBitLimit0b(
      MaybeULong((8 * 6) + 10)
    ) // 1 more byte plus 2 extra bits after first 2 chars
    val ms = dis.getSomeString(3, beFinfo)
    assertTrue(ms.isDefined)
    val s = ms.get
    assertEquals(3, s.length)
    assertEquals('年', s(0))
    assertEquals('月', s(1))
    assertEquals(this.unicodeReplacementCharacter, s(2))
    assertEquals(8 * 7, dis.bitPos0b)
  }

  def unicodeReplacementCharacter = '\uFFFD'

  /**
   * Tests of char iteration with skips of bits that force
   * re-aligning to mandatory alignment boundaries
   *
   * These just insure that we move over to the mandatory alignment before decoding
   * any characters.
   */

  @Test def testCharIteratorWithInterruptingBitSkips1(): Unit = {
    val dis = InputSourceDataInputStream("0年1月2日".getBytes("utf-8"))
    val iter = dis.asIteratorChar
    iter.setFormatInfo(beFinfo)
    dis.skip(1, beFinfo)
    assertTrue(
      iter.hasNext
    ) // examining a character here requires aligning to mandatory alignment of 8 bit boundary.
    assertEquals(1, dis.bitPos0b)
    assertEquals('年', iter.next())
    assertEquals(32, dis.bitPos0b)
    dis.skip(1, beFinfo)
    assertTrue(iter.hasNext)
    assertEquals(33, dis.bitPos0b)
    assertEquals('月', iter.next())
    assertEquals(64, dis.bitPos0b)
    dis.skip(1, beFinfo)
    assertTrue(iter.hasNext)
    assertEquals('日', iter.next())
    assertEquals(96, dis.bitPos0b)
    assertFalse(dis.skip(1, beFinfo))
    assertFalse(iter.hasNext)
  }

  /**
   * This test shows that if you do a bad-practice thing, and
   * actually move the bitPos between hasNext() and next(), that
   * the "right thing" happens, which is that the iterator notices this,
   *
   * Also shows that hasNext() doesn't ever move the bitPos even
   * if it has to align to a mandatory character alignment boundary.
   */
  @Test def testCharIteratorWithInterruptingBitSkipsBetweenHasNextAndNext(): Unit = {
    val dis = InputSourceDataInputStream("0年1月2日".getBytes("utf-8"))
    val iter = dis.asIteratorChar
    iter.setFormatInfo(beFinfo)
    dis.skip(1, beFinfo)
    assertTrue(
      iter.hasNext
    ) // examining a character here requires aligning to mandatory alignment of 8 bit boundary.
    assertEquals(1, dis.bitPos0b)
    dis.skip(1, beFinfo) // this skip should invalidate the character cached by hasNext.
    assertEquals(2, dis.bitPos0b)
    assertTrue(iter.hasNext)
    assertEquals(2, dis.bitPos0b)
    val c = iter.next()
    assertEquals(
      32,
      dis.bitPos0b
    ) // has next doesn't cause movement even to align to mandatory.
    assertEquals('年', c)
    assertTrue(iter.hasNext)
    dis.skip(4, beFinfo)
    assertEquals(36, dis.bitPos0b)
    val d = iter.next()
    assertEquals(64, dis.bitPos0b)
    assertEquals('月', d)
  }

}
