/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package org.apache.daffodil.io

import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.CoderResult

import org.junit.Assert._
import org.junit.Test

import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.util.Misc
import org.apache.daffodil.processors.charset.OctalLSBF3BitCharset

/**
 * tests of 7-bit characters
 */
class TestByteBufferDataInputStream3Bit {

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
      bitsListRTL.reverse.map { _.reverse }.mkString.sliding(8, 8).toList.map { _.reverse }.flatMap { Misc.bits2Bytes(_) }.toArray
    }

    def encode3(s: String): Seq[String] = {
      val encoder = OctalLSBF3BitCharset.newEncoder
      val bb = ByteBuffer.allocate(4 * s.length)
      val cb = CharBuffer.wrap(s)
      val coderResult = encoder.encode(cb, bb, true)
      Assert.invariant(coderResult == CoderResult.UNDERFLOW)
      bb.flip()
      val res = (0 to bb.limit() - 1).map { bb.get(_) }
      // val bitsAsString = Misc.bytes2Bits(res.toArray)
      val enc = encoder.asInstanceOf[NonByteSizeCharsetEncoder]
      val nBits = s.length * enc.bitWidthOfACodeUnit
      val bitStrings = res.map { b => (b & 0xFF).toBinaryString.reverse.padTo(8, '0').reverse }.toList
      val allBits = bitStrings.reverse.mkString.takeRight(nBits)
      val threeBitChunks = allBits.reverse.sliding(3, 3).map { _.reverse }.toList.reverse.mkString
      rtl(threeBitChunks)
    }

    def enc(s: String) = toBytes(encode3(s))

  }

  /** Test the test rig */
  @Test def testBitsBitteBits = {

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
    assertEquals(List("000", "111", "110", "101", "100", "011", "010", "001"),
      chunks)

  }

  /*
   * Tests of unaligned char buffers (ie., 3-bit characters)
   */
  @Test def testFillCharBufferOne3BitChar {
    val dis = ByteBufferDataInputStream(BitsBitte.enc("01234567"))
    dis.setDecoder(OctalLSBF3BitCharset.newDecoder)
    val cb = CharBuffer.allocate(1)
    val ml = dis.fillCharBuffer(cb)
    cb.flip
    assertTrue(ml.isDefined)
    assertEquals(1, ml.get)
    assertEquals(3, dis.bitPos0b)
    assertEquals('0', cb.get())
  }

  @Test def testFillCharBuffer3BitString {
    val dat = "01234567"
    val cs = OctalLSBF3BitCharset
    val dis = ByteBufferDataInputStream(BitsBitte.enc(dat))
    dis.setDecoder(cs.newDecoder)
    val cb = CharBuffer.allocate(8)
    val ml = dis.fillCharBuffer(cb)
    cb.flip
    assertTrue(ml.isDefined)
    assertEquals(8, ml.get)
    assertEquals(cs.bitWidthOfACodeUnit * 8, dis.bitPos0b)
    assertEquals(dat, cb.toString())
  }

  @Test def testFillCharBuffer3BitStringOffBy2 {
    val dat = "01234567"
    val cs = OctalLSBF3BitCharset
    val bytes = BitsBitte.toBytes(BitsBitte.rtl(BitsBitte.rtl("11"), BitsBitte.encode3(dat)))
    val dis = ByteBufferDataInputStream(bytes)
    dis.setDecoder(cs.newDecoder)
    val cb = CharBuffer.allocate(8)
    dis.skip(2)
    val ml = dis.fillCharBuffer(cb)
    cb.flip
    assertTrue(ml.isDefined)
    assertEquals(8, ml.get)
    assertEquals(dat, cb.toString())
    assertEquals(2 + (cs.bitWidthOfACodeUnit * 8), dis.bitPos0b)
  }

  @Test def testFillCharBufferDataEndsMidByte {
    val dat = "01234567"
    val cs = OctalLSBF3BitCharset
    val bytes = BitsBitte.toBytes(BitsBitte.rtl(BitsBitte.rtl("11"), BitsBitte.encode3(dat)))
    val dis = ByteBufferDataInputStream(bytes)
    dis.setDecoder(cs.newDecoder)
    dis.setBitLimit0b(MaybeULong(12))
    val cb = CharBuffer.allocate(8)
    dis.skip(2)
    val ml = dis.fillCharBuffer(cb)
    cb.flip
    assertTrue(ml.isDefined)
    assertEquals(3, ml.get)
    assertEquals("012", cb.toString())
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
  @Test def testFillCharBufferDataEndsMidByte2 {
    val dat = "01234567"
    val cs = OctalLSBF3BitCharset
    val bytes = BitsBitte.toBytes(BitsBitte.rtl(BitsBitte.rtl("1"), BitsBitte.encode3(dat)))
    val dis = ByteBufferDataInputStream(bytes)
    dis.setDecoder(cs.newDecoder)
    dis.setBitLimit0b(MaybeULong(8))
    val cb = CharBuffer.allocate(8)
    dis.skip(1)
    val ml = dis.fillCharBuffer(cb)
    cb.flip
    assertTrue(ml.isDefined)
    assertEquals(2, ml.get)
    assertEquals("01", cb.toString())
    assertEquals(7, dis.bitPos0b)
  }

  /**
   * Similar to above test, except the remaining partial byte does not provide
   * enough bits to finish a character.
   */

  @Test def testFillCharBufferDataEndsMidByte3 {
    val dat = "56701234"
    val cs = OctalLSBF3BitCharset
    val bytes = BitsBitte.toBytes(BitsBitte.rtl(BitsBitte.encode3(dat)))
    val dis = ByteBufferDataInputStream(bytes)
    dis.setDecoder(cs.newDecoder)
    dis.setBitLimit0b(MaybeULong(5))
    val cb = CharBuffer.allocate(8)
    val ml = dis.fillCharBuffer(cb)
    cb.flip
    assertTrue(ml.isDefined)
    assertEquals(1, ml.get)
    assertEquals("5", cb.toString())
    assertEquals(3, dis.bitPos0b)
  }

  @Test def testFillCharBufferDataEndsMidByte3a {
    val dat = "77756701234"
    val cs = OctalLSBF3BitCharset
    val bytes = BitsBitte.toBytes(BitsBitte.rtl(BitsBitte.encode3(dat)))
    val dis = ByteBufferDataInputStream(bytes)
    dis.setDecoder(cs.newDecoder)
    dis.setBitLimit0b(MaybeULong(14))
    val cb = CharBuffer.allocate(8)
    val ml = dis.fillCharBuffer(cb)
    cb.flip
    assertTrue(ml.isDefined)
    assertEquals(4, ml.get)
    assertEquals("7775", cb.toString())
    assertEquals(12, dis.bitPos0b)
  }

  @Test def testFillCharBufferDataEndsMidByte3b {
    val dat = "56701234"
    val cs = OctalLSBF3BitCharset
    val bytes = BitsBitte.toBytes(BitsBitte.rtl(BitsBitte.rtl("1"), BitsBitte.encode3(dat)))
    val dis = ByteBufferDataInputStream(bytes)
    dis.setDecoder(cs.newDecoder)
    dis.setBitLimit0b(MaybeULong(6))
    val cb = CharBuffer.allocate(8)
    dis.skip(1)
    val ml = dis.fillCharBuffer(cb)
    cb.flip
    assertTrue(ml.isDefined)
    assertEquals(1, ml.get)
    assertEquals("5", cb.toString())
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

  @Test def testCharIteratorWithInterruptingBitSkips1 {
    val dis = ByteBufferDataInputStream(BitsBitte.enc("01234567"))
    dis.setDecoder(OctalLSBF3BitCharset.newDecoder)
    dis.setBitLimit0b(MaybeULong(24))
    val iter = dis.asIteratorChar
    dis.skip(3)
    assertTrue(iter.hasNext)
    assertEquals(3, dis.bitPos0b)
    assertEquals('1', iter.next)
    assertEquals(6, dis.bitPos0b)
    dis.skip(3)
    assertTrue(iter.hasNext)
    assertEquals(9, dis.bitPos0b)
    assertEquals('3', iter.next)
    assertEquals(12, dis.bitPos0b)
    dis.skip(1)
    dis.skip(2)
    assertTrue(iter.hasNext)
    assertEquals('5', iter.next)
    assertEquals(18, dis.bitPos0b)
    assertTrue(dis.skip(3))
    assertEquals('7', iter.next)
    assertFalse(iter.hasNext)
  }

  @Test def test3BitEncoderOverflowError {
    val encoder = OctalLSBF3BitCharset.newEncoder
    val bb = ByteBuffer.allocate(1) // only big enough for a single byte
    val cb = CharBuffer.wrap("123") // 3 octal digits will cause overflow
    val coderResult = encoder.encode(cb, bb, true)
    assertTrue(coderResult == CoderResult.OVERFLOW)
  }

  @Test def test3BitEncoderMalformedError {
    val encoder = OctalLSBF3BitCharset.newEncoder
    val bb = ByteBuffer.allocate(3)
    val cb = CharBuffer.wrap("12?") // ? is not encodable in octal
    val coderResult = encoder.encode(cb, bb, true)
    assertTrue(coderResult.isUnmappable())
    assertEquals(coderResult.length, 1)
  }

}
