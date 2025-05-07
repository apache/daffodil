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
import org.apache.daffodil.io.processors.charset.BitsCharsetUSASCII7BitPacked
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.lib.util.Misc

import org.junit.Assert._
import org.junit.Test

/**
 * Helper class for creating example data that is unaligned.
 */
object Bitte {

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

  def encode7(s: String): Seq[String] = {
    val encoder = BitsCharsetUSASCII7BitPacked.newEncoder()
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
    val sevenBitChunks = allBits.reverse.sliding(7, 7).map { _.reverse }.toList.reverse.mkString
    rtl(sevenBitChunks)
  }

  def enc(s: String) = toBytes(encode7(s))

}

/**
 * tests of 7-bit characters
 */
class TestInputSourceDataInputStream7 {

  val finfo = FormatInfoForUnitTest()
  finfo.reset(BitsCharsetUSASCII7BitPacked)
  finfo.bitOrder = BitOrder.LeastSignificantBitFirst

  /** Test the test rig */
  @Test def testBitteBits() = {

    val a = Bitte.encode7("a")
    assertEquals(Seq("1100001"), a)
    val aBytes = Bitte.toBytes(a)
    assertEquals(List(0x61.toByte), aBytes.toList)

    val ab = Bitte.encode7("ab")
    assertEquals(Seq("110001", "01100001"), ab)
    val abBytes = Bitte.toBytes(ab)
    assertEquals(List(0x61.toByte, 0x31.toByte), abBytes.toList)
    assertEquals(Bitte.rtl("1100010 1100001"), ab)

    val someBits = Bitte.rtl("0110010 0110001") // starts on right.
    val moreBits = Bitte.encode7("345678") // encodes LSBFirst, packs into 8-bit bytes.
    val bytesList = Bitte.rtl(someBits, moreBits)
    val bytes = Bitte.toBytes(bytesList)
    assertEquals(7, bytes.length)
    // val bytesAsHex = Misc.bytes2Hex(bytes)
    val bytesAsBits = Misc.bytes2Bits(bytes).sliding(8, 8).toList
    val chunks = bytesAsBits.map { _.reverse }.mkString.reverse.sliding(7, 7).toList
    assertEquals(
      List(
        "0111000",
        "0110111",
        "0110110",
        "0110101",
        "0110100",
        "0110011",
        "0110010",
        "0110001"
      ),
      chunks
    )

  }

  /**
   * Test specifically shows that one can retrieve the byte that is
   * at the end, and is only partial due to a bitLimit being in place.
   *
   * In this case, the data consists of 3 7-bit characters, so 21 bits.
   * There are 3 bytes of data, but the last one has only 5 bits in it.
   *
   * The `getByteArray` call returns 3 bytes, but one is a fragment byte
   */
  @Test def testGetByteArrayLengthLimit1(): Unit = {
    val dis = InputSourceDataInputStream(Bitte.enc("abc"))
    dis.setBitLimit0b(MaybeULong(21))
    val finfo = FormatInfoForUnitTest()
    finfo.bitOrder = BitOrder.MostSignificantBitFirst
    val arr = dis.getByteArray(21, finfo)
    assertEquals(21, dis.bitLimit0b.get)
    assertEquals(21, dis.bitPos0b)
    assertEquals(3, arr.size)
    assertEquals(0x61.toByte, arr(0))
    assertEquals(0xf1.toByte, arr(1))
    assertEquals(0x18.toByte, arr(2))
  }

  /*
   * Tests of unaligned char buffers (ie., 7-bit characters)
   */
  @Test def testGetSomeStringOne7BitChar(): Unit = {
    val dis = InputSourceDataInputStream(Bitte.enc("abcdefgh"))
    val ms = dis.getSomeString(1, finfo)
    assertTrue(ms.isDefined)
    val s = ms.get
    assertEquals(1, s.length)
    assertEquals(7, dis.bitPos0b)
    assertEquals('a', s(0))
  }

  @Test def testGetSomeString7BitString(): Unit = {
    val dis = InputSourceDataInputStream(Bitte.enc("abcdefgh"))
    val ms = dis.getSomeString(8, finfo)
    assertTrue(ms.isDefined)
    val s = ms.get
    assertEquals(8, s.length)
    assertEquals(56, dis.bitPos0b)
    assertEquals("abcdefgh", s)
  }
  @Test def testGetSomeString7BitStringOffBy3(): Unit = {
    val bytes = Bitte.toBytes(Bitte.rtl(Bitte.rtl("101"), Bitte.encode7("abcdefgh")))
    val dis = InputSourceDataInputStream(bytes)
    dis.skip(3, finfo)
    val ms = dis.getSomeString(8, finfo)
    assertTrue(ms.isDefined)
    val s = ms.get
    assertEquals(8, s.length)
    assertEquals("abcdefgh", s)
    assertEquals(59, dis.bitPos0b)
  }

  @Test def testGetSomeStringDataEndsMidByte(): Unit = {
    val bytes = Bitte.toBytes(Bitte.rtl(Bitte.rtl("101"), Bitte.encode7("abcdefgh")))
    val dis = InputSourceDataInputStream(bytes)
    dis.setBitLimit0b(MaybeULong(25))
    dis.skip(3, finfo)
    val ms = dis.getSomeString(8, finfo)
    assertTrue(ms.isDefined)
    val s = ms.get
    assertEquals(3, s.length)
    assertEquals("abc", s)
    assertEquals(24, dis.bitPos0b)
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
    val bytes = Bitte.toBytes(Bitte.rtl(Bitte.rtl("101"), Bitte.encode7("abcdefgh")))
    val dis = InputSourceDataInputStream(bytes)
    dis.setBitLimit0b(MaybeULong(20))
    dis.skip(3, finfo)
    val ms = dis.getSomeString(8, finfo)
    assertTrue(ms.isDefined)
    val s = ms.get
    assertEquals(2, s.length)
    assertEquals("ab", s)
    assertEquals(17, dis.bitPos0b)
  }

  /**
   * Similar to above test, except the remaining partial byte does not provide
   * enough bits to finish a character.
   */
  @Test def testGetSomeStringDataEndsMidByte3(): Unit = {
    val bytes = Bitte.toBytes(Bitte.rtl(Bitte.rtl("101"), Bitte.encode7("abcdefgh")))
    val dis = InputSourceDataInputStream(bytes)
    dis.setBitLimit0b(MaybeULong(16))
    dis.skip(3, finfo)
    val ms = dis.getSomeString(8, finfo)
    assertTrue(ms.isDefined)
    val s = ms.get
    assertEquals(1, s.length)
    assertEquals("a", s)
    assertEquals(10, dis.bitPos0b)
  }

  /**
   * Tests of char iteration with skips of bits that force
   * re-aligning to mandatory alignment boundaries
   *
   * These just insure that we move over to the mandatory alignment before decoding
   * any characters.
   */

  @Test def testCharIteratorWithInterruptingBitSkips1(): Unit = {
    val dis = InputSourceDataInputStream(Bitte.enc("0a1b2c"))
    dis.setBitLimit0b(MaybeULong(42))
    val iter = dis.asIteratorChar
    iter.setFormatInfo(finfo)
    dis.skip(7, finfo)
    assertTrue(iter.hasNext)
    assertEquals(7, dis.bitPos0b)
    assertEquals('a', iter.next())
    assertEquals(14, dis.bitPos0b)
    dis.skip(7, finfo)
    assertTrue(iter.hasNext)
    assertEquals(21, dis.bitPos0b)
    assertEquals('b', iter.next())
    assertEquals(28, dis.bitPos0b)
    dis.skip(7, finfo)
    assertTrue(iter.hasNext)
    // Problem. We have 42 bits, so it *should* be letting us get the last character.
    // However, it is not, because we're not being allowed to get the next byte (the whole next byte isn't available)
    // But if it allowed us to get as far as bit 42, which is available, then we could get the character.
    //
    // This is a bug in the 7-bit decoder decodeLoop - if it cannot get another byte it needs to check if the
    // data needed can be satisfied from the current fragment.
    //
    assertEquals('c', iter.next())
    assertEquals(42, dis.bitPos0b)
    assertFalse(dis.skip(1, finfo))
    assertFalse(iter.hasNext)
  }

  /**
   * This test shows that if you do a bad-practice thing, and
   * actually move the bitPos between hasNext() and next(), that
   * the "right thing" happens, which is that the iterator notices this,
   * and internally does a reset to the new position and attempts
   * to decode characters from that new location.
   *
   * Also shows that hasNext() doesn't ever move the bitPos even
   * if it has to align to a mandatory character alignment boundary.
   */
  @Test def testCharIteratorWithInterruptingBitSkipsBetweenHasNextAndNext(): Unit = {
    val dis = InputSourceDataInputStream(Bitte.enc("0a1b2c"))
    dis.setBitLimit0b(MaybeULong(42))
    val iter = dis.asIteratorChar
    iter.setFormatInfo(finfo)
    assertTrue(iter.hasNext)
    dis.skip(7, finfo)
    assertEquals(7, dis.bitPos0b)
    assertEquals('a', iter.next())
    assertEquals(14, dis.bitPos0b)
    dis.skip(7, finfo)
    assertEquals(21, dis.bitPos0b)
    assertEquals('b', iter.next())
    assertEquals(28, dis.bitPos0b)
    dis.skip(7, finfo)
    assertEquals(35, dis.bitPos0b)
    assertTrue(iter.hasNext)
    assertEquals(35, dis.bitPos0b)
    assertEquals('c', iter.next())
    assertEquals(42, dis.bitPos0b)

  }

  @Test def testUSASCII7BitEncoderOverflowError(): Unit = {
    val encoder = BitsCharsetUSASCII7BitPacked.newEncoder()
    val bb = ByteBuffer.allocate(1) // only big enough for a single byte
    val cb = CharBuffer.wrap("ab") // two characters will cause overflow
    val coderResult = encoder.encode(cb, bb, true)
    assertTrue(coderResult == CoderResult.OVERFLOW)
  }

  @Test def testUSASCII7BitEncoderMalformedError(): Unit = {
    val encoder = BitsCharsetUSASCII7BitPacked.newEncoder()
    val bb = ByteBuffer.allocate(3)
    val cb = CharBuffer.wrap("ab" + 128.toChar) // 128 is not encodable in 7 bits
    val coderResult = encoder.encode(cb, bb, true)
    assertTrue(coderResult.isUnmappable())
    assertEquals(coderResult.length, 1)
  }

}
