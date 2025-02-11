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
import java.nio.charset.CoderResult

import org.apache.daffodil.io.processors.charset.BitsCharsetNonByteSizeEncoder
import org.apache.daffodil.io.processors.charset.BitsCharsetOctalLSBF
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.lib.util.Misc

import org.junit.Assert._
import org.junit.Test

/**
 * tests of 7-bit characters
 */
class TestInputSourceDataInputStream3Bit {

  val finfo = FormatInfoForUnitTest()

  /**
   * Helper class for creating example data that is unaligned.
   */
  object BitsBitte {

    def onlyBits(bits: String) = bits.split("[^01]+").mkString

    def rtl(bits: String): Seq[String] = {
      val bitDigits = onlyBits(bits)
      bitDigits.reverse.sliding(8, 8).toList.map { _.reverse }.reverse
    }

    def rtl(bitsArr: Seq[String]*): Seq[String] = {
      val bitsList = bitsArr.toList.reverse.flatten
      val allBits = bitsList.mkString
      rtl(allBits)
    }

    def toBytes(bitsListRTL: Seq[String]): Array[Byte] = {
      bitsListRTL.reverse
        .map { _.reverse }
        .mkString
        .sliding(8, 8)
        .toList
        .map { _.reverse }
        .flatMap { Misc.bits2Bytes(_) }
        .toArray
    }

    def encode3(s: String): Seq[String] = {
      val encoder = BitsCharsetOctalLSBF.newEncoder()
      val bb = ByteBuffer.allocate(4 * s.length)
      val cb = CharBuffer.wrap(s)
      val coderResult = encoder.encode(cb, bb, true)
      Assert.invariant(coderResult == CoderResult.UNDERFLOW)
      bb.flip()
      val res = (0 to bb.limit() - 1).map { bb.get(_) }
      // val bitsAsString = Misc.bytes2Bits(res.toArray)
      val enc = encoder.asInstanceOf[BitsCharsetNonByteSizeEncoder]
      val nBits = s.length * enc.bitsCharset.bitWidthOfACodeUnit
      val bitStrings = res.map { b =>
        (b & 0xff).toBinaryString.reverse.padTo(8, '0').reverse
      }.toList
      val allBits = bitStrings.reverse.mkString.takeRight(nBits)
      val threeBitChunks =
        allBits.reverse.sliding(3, 3).map { _.reverse }.toList.reverse.mkString
      rtl(threeBitChunks)
    }

    def enc(s: String) = toBytes(encode3(s))

  }

  /** Test the test rig */
  @Test def testBitsBitteBits() = {

    val a = BitsBitte.encode3("5")
    assertEquals(Seq("101"), a)
    val aBytes = BitsBitte.toBytes(a)
    assertEquals(List(0x05.toByte), aBytes.toList)

    val ab = BitsBitte.encode3("56")
    assertEquals(Seq("110101"), ab)
    val abBytes = BitsBitte.toBytes(ab)
    assertEquals(List(0x35.toByte), abBytes.toList)
    assertEquals(BitsBitte.rtl("110 101"), ab)

    val someBits = BitsBitte.rtl("010 001") // starts on right.
    val moreBits = BitsBitte.encode3("345670") // encodes LSBFirst, packs into 8-bit bytes.
    val bytesList = BitsBitte.rtl(someBits, moreBits)
    val bytes = BitsBitte.toBytes(bytesList)
    assertEquals(3, bytes.length)
    // val bytesAsHex = Misc.bytes2Hex(bytes)
    val bytesAsBits = Misc.bytes2Bits(bytes).sliding(8, 8).toList
    val chunks = bytesAsBits.map { _.reverse }.mkString.reverse.sliding(3, 3).toList
    assertEquals(List("000", "111", "110", "101", "100", "011", "010", "001"), chunks)

  }

  /*
   * Tests of unaligned char buffers (ie., 3-bit characters)
   */
  @Test def testGetSomeStringOne3BitChar(): Unit = {
    val dis = InputSourceDataInputStream(BitsBitte.enc("01234567"))
    val cs = BitsCharsetOctalLSBF
    val finfo = FormatInfoForUnitTest()
    finfo.reset(cs)
    finfo.bitOrder = BitOrder.LeastSignificantBitFirst
    val ms = dis.getSomeString(1, finfo)
    assertTrue(ms.isDefined)
    val s = ms.get
    assertEquals(1, s.length)
    assertEquals(3, dis.bitPos0b)
    assertEquals('0', s(0))
  }

  @Test def testGetSomeString3BitString(): Unit = {
    val dat = "01234567"
    val cs = BitsCharsetOctalLSBF
    val dis = InputSourceDataInputStream(BitsBitte.enc(dat))
    val finfo = FormatInfoForUnitTest()
    finfo.reset(cs)
    finfo.bitOrder = BitOrder.LeastSignificantBitFirst
    val ms = dis.getSomeString(8, finfo)
    assertTrue(ms.isDefined)
    val s = ms.get
    assertEquals(8, s.length)
    assertEquals(cs.bitWidthOfACodeUnit * 8, dis.bitPos0b)
    assertEquals(dat, s)
  }

  @Test def testGetSomeString3BitStringOffBy2(): Unit = {
    val dat = "01234567"
    val cs = BitsCharsetOctalLSBF
    val bytes = BitsBitte.toBytes(BitsBitte.rtl(BitsBitte.rtl("11"), BitsBitte.encode3(dat)))
    val dis = InputSourceDataInputStream(bytes)
    val finfo = FormatInfoForUnitTest()
    finfo.reset(cs)
    finfo.bitOrder = BitOrder.LeastSignificantBitFirst
    dis.skip(2, finfo)
    val ms = dis.getSomeString(8, finfo)
    assertTrue(ms.isDefined)
    val s = ms.get
    assertEquals(8, s.length)
    assertEquals(dat, s)
    assertEquals(2 + (cs.bitWidthOfACodeUnit * 8), dis.bitPos0b)
  }

  @Test def testGetSomeStringDataEndsMidByte(): Unit = {
    val dat = "01234567"
    val cs = BitsCharsetOctalLSBF
    val bytes = BitsBitte.toBytes(BitsBitte.rtl(BitsBitte.rtl("11"), BitsBitte.encode3(dat)))
    val dis = InputSourceDataInputStream(bytes)
    val finfo = FormatInfoForUnitTest()
    finfo.reset(cs)
    finfo.bitOrder = BitOrder.LeastSignificantBitFirst
    dis.setBitLimit0b(MaybeULong(12))
    dis.skip(2, finfo)
    val ms = dis.getSomeString(8, finfo)
    assertTrue(ms.isDefined)
    val s = ms.get
    assertEquals(3, s.length)
    assertEquals("012", s)
    assertEquals(11, dis.bitPos0b)
  }

  /**
   * Tests that when the data ends mid-byte, that we can decode a character
   * so long as we have enough data to provide its bits, even though we don't have
   * enough bits to grab the next full byte.
   *
   * This covers a subtle bug case. Where the decoder terminates due to not being
   * able to fetch another byte of source data (aka an "underflow"), yet there actually
   * are sufficient bits without that byte to decode a character.
   */
  @Test def testGetSomeStringDataEndsMidByte2(): Unit = {
    val dat = "01234567"
    val cs = BitsCharsetOctalLSBF
    val bytes = BitsBitte.toBytes(BitsBitte.rtl(BitsBitte.rtl("1"), BitsBitte.encode3(dat)))
    val dis = InputSourceDataInputStream(bytes)
    val finfo = FormatInfoForUnitTest()
    finfo.reset(cs)
    finfo.bitOrder = BitOrder.LeastSignificantBitFirst
    dis.setBitLimit0b(MaybeULong(8))
    dis.skip(1, finfo)
    val ms = dis.getSomeString(8, finfo)
    assertTrue(ms.isDefined)
    val s = ms.get
    assertEquals(2, s.length)
    assertEquals("01", s)
    assertEquals(7, dis.bitPos0b)
  }

  /**
   * Similar to above test, except the remaining partial byte does not provide
   * enough bits to finish a character.
   */

  @Test def testGetSomeStringrDataEndsMidByte3(): Unit = {
    val dat = "56701234"
    val cs = BitsCharsetOctalLSBF
    val bytes = BitsBitte.toBytes(BitsBitte.rtl(BitsBitte.encode3(dat)))
    val dis = InputSourceDataInputStream(bytes)
    val finfo = FormatInfoForUnitTest()
    finfo.reset(cs)
    finfo.bitOrder = BitOrder.LeastSignificantBitFirst
    dis.setBitLimit0b(MaybeULong(5))
    val ms = dis.getSomeString(8, finfo)
    assertTrue(ms.isDefined)
    val s = ms.get
    assertEquals(1, s.length)
    assertEquals("5", s)
    assertEquals(3, dis.bitPos0b)
  }

  @Test def testGetSomeStringDataEndsMidByte3a(): Unit = {
    val dat = "77756701234"
    val cs = BitsCharsetOctalLSBF
    val bytes = BitsBitte.toBytes(BitsBitte.rtl(BitsBitte.encode3(dat)))
    val dis = InputSourceDataInputStream(bytes)
    val finfo = FormatInfoForUnitTest()
    finfo.reset(cs)
    finfo.bitOrder = BitOrder.LeastSignificantBitFirst
    dis.setBitLimit0b(MaybeULong(14))
    val ms = dis.getSomeString(8, finfo)
    assertTrue(ms.isDefined)
    val s = ms.get
    assertEquals(4, s.length)
    assertEquals("7775", s)
    assertEquals(12, dis.bitPos0b)
  }

  @Test def testGetSomeStringDataEndsMidByte3b(): Unit = {
    val dat = "56701234"
    val cs = BitsCharsetOctalLSBF
    val bytes = BitsBitte.toBytes(BitsBitte.rtl(BitsBitte.rtl("1"), BitsBitte.encode3(dat)))
    val dis = InputSourceDataInputStream(bytes)
    val finfo = FormatInfoForUnitTest()
    finfo.reset(cs)
    finfo.bitOrder = BitOrder.LeastSignificantBitFirst
    dis.setBitLimit0b(MaybeULong(6))
    dis.skip(1, finfo)
    val ms = dis.getSomeString(8, finfo)
    assertTrue(ms.isDefined)
    val s = ms.get
    assertEquals(1, s.length)
    assertEquals("5", s)
    assertEquals(4, dis.bitPos0b)
  }

  /**
   * Tests of char iteration with skips of bits
   *
   * This test shows that if you do a bad-practice thing, and
   * actually move the bitPos between hasNext() and next(), that
   * the "right thing" happens, which is that the iterator notices this,
   * and internally does a reset to the new position and attempts
   * to decode characters from that new location.
   *
   * Also shows that hasNext() doesn't ever move the bitPos.
   */
  @Test def testCharIteratorWithInterruptingBitSkips1(): Unit = {
    val dis = InputSourceDataInputStream(BitsBitte.enc("01234567"))
    val cs = BitsCharsetOctalLSBF
    val finfo = FormatInfoForUnitTest()
    finfo.reset(cs)
    finfo.bitOrder = BitOrder.LeastSignificantBitFirst
    dis.setBitLimit0b(MaybeULong(24))
    val iter = dis.asIteratorChar
    iter.setFormatInfo(finfo)
    dis.skip(3, finfo)
    assertTrue(iter.hasNext)
    assertEquals(3, dis.bitPos0b)
    assertEquals('1', iter.next())
    assertEquals(6, dis.bitPos0b)
    dis.skip(3, finfo)
    assertTrue(iter.hasNext)
    assertEquals(9, dis.bitPos0b)
    assertEquals('3', iter.next())
    assertEquals(12, dis.bitPos0b)
    dis.skip(1, finfo)
    dis.skip(2, finfo)
    assertTrue(iter.hasNext)
    assertEquals('5', iter.next())
    assertEquals(18, dis.bitPos0b)
    assertTrue(dis.skip(3, finfo))
    assertEquals('7', iter.next())
    assertFalse(iter.hasNext)
  }

  @Test def test3BitEncoderOverflowError(): Unit = {
    val encoder = BitsCharsetOctalLSBF.newEncoder()
    val bb = ByteBuffer.allocate(1) // only big enough for a single byte
    val cb = CharBuffer.wrap("123") // 3 octal digits will cause overflow
    val coderResult = encoder.encode(cb, bb, true)
    assertTrue(coderResult == CoderResult.OVERFLOW)
  }

  @Test def test3BitEncoderMalformedError(): Unit = {
    val encoder = BitsCharsetOctalLSBF.newEncoder()
    val bb = ByteBuffer.allocate(3)
    val cb = CharBuffer.wrap("12?") // ? is not encodable in octal
    val coderResult = encoder.encode(cb, bb, true)
    assertTrue(coderResult.isUnmappable())
    assertEquals(coderResult.length, 1)
  }

}
