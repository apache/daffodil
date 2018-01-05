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
import java.nio.charset.MalformedInputException
import java.util.regex.Pattern

import scala.BigInt
import scala.math.BigInt.int2bigInt

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

import org.apache.daffodil.Implicits.intercept
import org.apache.daffodil.equality.TypeEqual
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.util.Misc
import passera.unsigned.ULong
import java.nio.charset.CodingErrorAction
import org.apache.daffodil.processors.charset.StandardBitsCharsets

class TestByteBufferDataInputStream {
  val tenDigits = "1234567890"
  val ten = tenDigits.getBytes("utf-8")
  val twentyDigits = tenDigits * 2
  val twenty = twentyDigits.getBytes("utf-8")

  val finfo = FormatInfoForUnitTest()

  implicit class ExactTypeEqual[L](left: L) {
    def assertEqualsTyped[R, D >: L <: R, D2 >: R <: L](right: R): Unit = assertEquals(left, right)
  }

  /**
   * I am so sick of getting JUnit test-time errors where it says the two things are unequal, when
   * they're different types.... I want a strongly typed assertEquals that won't compile if
   * the two args are different types.
   *
   * However, I couldn't figure out how to do that. This form doesn't achieve that. Need to use an implicit type parameter or
   * something like that.
   */
  def assertEqualsTyped[T](left: T, right: T): Unit = assertEquals(left, right)

  def assertEqualsTyped(expected: Float, actual: Float, threshold: Float) = assertEquals(expected, actual, threshold)
  def assertEqualsTyped(expected: Double, actual: Double, threshold: Double) = assertEquals(expected, actual, threshold)

  @Test def testBitAndBytePos0 {
    val dis = ByteBufferDataInputStream(ten)
    0L assertEqualsTyped (dis.bitPos0b)
    80L assertEqualsTyped (dis.bitLimit0b.get)
    1L assertEqualsTyped (dis.bitPos1b)
    81L assertEqualsTyped (dis.bitLimit1b.get)
    0L assertEqualsTyped (dis.bytePos0b)
  }

  @Test def testBitAndBytePos1 {
    val dis = ByteBufferDataInputStream(ten)
    val arr = dis.getByteArray(8, finfo)
    assertEqualsTyped[Long](1, arr.size)
    assertEqualsTyped[Long](0x31.toByte, arr(0))
    assertEqualsTyped[Long](8, dis.bitPos0b)
    assertEqualsTyped[Long](80, dis.bitLimit0b.get)
    assertEqualsTyped[Long](9, dis.bitPos1b)
    assertEqualsTyped[Long](81, dis.bitLimit1b.get)
    assertEqualsTyped[Long](1, dis.bytePos0b)
  }

  @Test def testBitAndBytePos10 {
    val dis = ByteBufferDataInputStream(ten)
    val arr = dis.getByteArray(80, finfo)
    assertEqualsTyped[Long](10, arr.size)
    assertEqualsTyped[Long](0x30.toByte, arr(9))
    assertEqualsTyped[Long](80, dis.bitPos0b)
    assertEqualsTyped[Long](80, dis.bitLimit0b.get)
    assertEqualsTyped[Long](81, dis.bitPos1b)
    assertEqualsTyped[Long](81, dis.bitLimit1b.get)
    assertEqualsTyped[Long](10, dis.bytePos0b)
  }

  @Test def testBitAndBytePosNotEnoughData1 {
    val dis = ByteBufferDataInputStream(ten)
    intercept[DataInputStream.NotEnoughDataException] {
      dis.getByteArray(81, finfo)
    }
    assertEqualsTyped[Long](0, dis.bitPos0b)
    assertEqualsTyped[Long](80, dis.bitLimit0b.get)
  }

  @Test def testBitAndBytePosMoreThanEnoughData1 {
    val dis = ByteBufferDataInputStream(twenty)
    val arr = dis.getByteArray(80, finfo)
    assertEqualsTyped[Long](10, arr.size)
    assertEqualsTyped[Long](0x30.toByte, arr(9))
    assertEqualsTyped[Long](80, dis.bitPos0b)
    assertEqualsTyped[Long](160, dis.bitLimit0b.get)
    assertEqualsTyped[Long](10, dis.bytePos0b)
  }

  // TODO Fix this
  @Test def testBitLengthLimit1 {
    val dis = ByteBufferDataInputStream(twenty)
    val isLimitOk = dis.withBitLengthLimit(80) {
      val arr = dis.getByteArray(80, finfo)
      assertEqualsTyped[Long](80, dis.bitLimit0b.get)
      assertEqualsTyped[Long](80, dis.bitPos0b)
      assertEqualsTyped[Long](10, arr.size)
      assertEqualsTyped[Long](0x30.toByte, arr(9))
    }
    assertTrue(isLimitOk)
    assertEqualsTyped[Long](80, dis.bitPos0b)
    assertEqualsTyped[Long](160, dis.bitLimit0b.get)
    assertEqualsTyped[Long](10, dis.bytePos0b)
  }

  @Test def testBinaryDouble1 {
    val dis = ByteBufferDataInputStream("123".getBytes("utf-8"))
    intercept[DataInputStream.NotEnoughDataException] {
      dis.getBinaryDouble(finfo)
    }
    assertEqualsTyped[Long](0, dis.bitPos0b)
  }

  @Test def testBinaryDouble2 {
    val dis = ByteBufferDataInputStream(twenty)
    val expected = ByteBuffer.wrap(twenty).asDoubleBuffer().get()
    val d = dis.getBinaryDouble(finfo)
    assertEqualsTyped(expected, d, 0.0)
  }

  @Test def testBinaryDouble3 {
    val dis = ByteBufferDataInputStream(twenty)
    dis.setBitLimit1b(MaybeULong(63)) // not enough bits
    intercept[DataInputStream.NotEnoughDataException] {
      dis.getBinaryDouble(finfo)
    }
  }

  @Test def testBinaryDouble4 {
    val dis = ByteBufferDataInputStream(twenty)
    dis.getByteArray(8, finfo)
    dis.setBitLimit1b(MaybeULong(71)) // not enough bits
    intercept[DataInputStream.NotEnoughDataException] {
      dis.getBinaryDouble(finfo)
    }
  }

  @Test def testBinaryFloat1 {
    val dis = ByteBufferDataInputStream("123".getBytes("utf-8"))
    intercept[DataInputStream.NotEnoughDataException] {
      dis.getBinaryFloat(finfo)
    }
    assertEqualsTyped[Long](0, dis.bitPos0b)
  }

  @Test def testBinaryFloat2 {
    val dis = ByteBufferDataInputStream(twenty)
    val expected = ByteBuffer.wrap(twenty).asFloatBuffer().get()
    val d = dis.getBinaryFloat(finfo)
    assertEqualsTyped(expected, d, 0.0)
  }

  @Test def testBinaryFloat3 {
    val dis = ByteBufferDataInputStream(twenty)
    dis.setBitLimit1b(MaybeULong(31)) // not enough bits
    assertFalse(dis.isDefinedForLength(32))
    intercept[DataInputStream.NotEnoughDataException] {
      dis.getBinaryFloat(finfo)
    }
  }

  @Test def testBinaryFloat4 {
    val dis = ByteBufferDataInputStream(twenty)
    dis.getByteArray(8, finfo)
    dis.setBitLimit1b(MaybeULong(39)) // not enough bits
    assertFalse(dis.isDefinedForLength(32))
    intercept[DataInputStream.NotEnoughDataException] {
      dis.getBinaryFloat(finfo)
    }
  }

  @Test def testSignedLong1 {
    val dis = ByteBufferDataInputStream(twenty)
    dis.setBitLimit1b(MaybeULong(1)) // 1b so 1 means no data
    intercept[DataInputStream.NotEnoughDataException] {
      dis.getSignedLong(1, finfo)
    }
  }

  @Test def testSignedLong2 {
    val dis = ByteBufferDataInputStream(twenty)
    dis.setBitLimit1b(MaybeULong(63))
    intercept[DataInputStream.NotEnoughDataException] {
      dis.getSignedLong(64, finfo)
    }
  }

  @Test def testSignedLong3 {
    val dis = ByteBufferDataInputStream(twenty)
    // buffer has 0x3132 in first 16 bits
    // binary that is 00110001 00110010
    var sl = dis.getSignedLong(1, finfo)
    assertEqualsTyped[Long](0L, sl)
    assertEqualsTyped[Long](1, dis.bitPos0b)
    sl = dis.getSignedLong(9, finfo)
    // those bits are 0110001 00 which is 0x0C4 and sign bit is 0 (positive)
    assertEqualsTyped[Long](0x0C4L, sl)
    assertEqualsTyped[Long](10, dis.bitPos0b)
  }

  @Test def testSignedLong4 {
    val dis = ByteBufferDataInputStream(twenty)
    var ml = dis.getSignedLong(1, finfo)
    assertEqualsTyped[Long](1, dis.bitPos0b)
    ml = dis.getSignedLong(64, finfo)
    assertEqualsTyped[Long](65, dis.bitPos0b)
    assertEqualsTyped[Long](0x3132333435363738L << 1, ml)
  }

  @Test def testSignedLong5 {
    val dis = ByteBufferDataInputStream(List(0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xC0).map { _.toByte }.toArray)
    var ml = dis.getSignedLong(1, finfo)
    assertEqualsTyped[Long](1, dis.bitPos0b)
    assertEqualsTyped[Long](1, ml)
    ml = dis.getSignedLong(64, finfo)
    assertEqualsTyped[Long](65, dis.bitPos0b)
    assertEqualsTyped[Long]((0xC1C2C3C4C5C6C7C8L << 1) + (0xC9 >>> 7), ml)
  }

  @Test def testSignedLong6 {
    val dis = ByteBufferDataInputStream(List(0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xC0).map { _.toByte }.toArray)
    var ml = dis.getSignedLong(1, finfo)
    assertEqualsTyped[Long](1, dis.bitPos0b)
    assertEqualsTyped[Long](1, ml)
    ml = dis.getSignedLong(32, finfo)
    assertEqualsTyped[Long](33, dis.bitPos0b)
    val expected = (((0xC1C2C3C4C5L >> 7) & 0xFFFFFFFFL) << 32) >> 32 // sign extend
    assertEqualsTyped[Long](expected, ml)
  }

  @Test def testSignedLong7 {
    val dis = ByteBufferDataInputStream(List(0xC1, 0xC2, 0xC3, 0xC4, 0xC5).map { _.toByte }.toArray)
    var ml = dis.getSignedLong(2, finfo)
    assertEqualsTyped[Long](2, dis.bitPos0b)
    assertEqualsTyped[Long](-1, ml)
    ml = dis.getSignedLong(32, finfo)
    assertEqualsTyped[Long](34, dis.bitPos0b)
    val expected = (0xC1C2C3C4C5L >> 6) & 0xFFFFFFFFL // will be positive, no sign extend
    assertEqualsTyped[Long](expected, ml)
  }

  @Test def testUnsignedLong1 {
    val dis = ByteBufferDataInputStream(List(0xC1, 0xC2, 0xC3, 0xC4, 0xC5).map { _.toByte }.toArray)
    val ml = dis.getUnsignedLong(32, finfo)
    assertEqualsTyped[Long](32, dis.bitPos0b)
    val expected = ULong(0xC1C2C3C4L)
    assertEqualsTyped[ULong](expected, ml)
  }

  @Test def testUnsignedLong2 {
    val dis = ByteBufferDataInputStream(List(0xA5, 0xA5, 0xA5, 0xA5, 0xA5).map { _.toByte }.toArray)
    dis.getSignedLong(1, finfo)
    assertEqualsTyped[Long](1, dis.bitPos0b)
    val ml = dis.getUnsignedLong(32, finfo)
    assertEqualsTyped[Long](33, dis.bitPos0b)
    val expected = ULong(0x4b4b4b4bL)
    assertEqualsTyped[ULong](expected, ml)
  }

  @Test def testUnsignedLong3 {
    val dis = ByteBufferDataInputStream(List(0xFF).map { _.toByte }.toArray)
    val ml = dis.getUnsignedLong(1, finfo)
    assertEqualsTyped[Long](1, dis.bitPos0b)
    val expected = ULong(1)
    assertEqualsTyped[ULong](expected, ml)
  }

  @Test def testSignedBigInt1 {
    val dis = ByteBufferDataInputStream(List(0xFF).map { _.toByte }.toArray)
    val ml = dis.getSignedBigInt(1, finfo)
    assertEqualsTyped[Long](1, dis.bitPos0b)
    val expected = BigInt(1)
    assertTrue(expected =:= ml)
  }

  @Test def testSignedBigInt2 {
    val dis = ByteBufferDataInputStream(List(0xC1, 0xC2, 0xC3, 0xC4, 0xC5).map { _.toByte }.toArray)
    val ml = dis.getSignedBigInt(40, finfo)
    assertEqualsTyped[Long](40, dis.bitPos0b)
    val expected = BigInt(0xFFFFFFC1C2C3C4C5L)
    assertEqualsTyped[BigInt](expected, ml)
  }

  @Test def testUnsignedBigInt1 {
    val dis = ByteBufferDataInputStream(List(0xFF).map { _.toByte }.toArray)
    val ml = dis.getUnsignedBigInt(2, finfo)
    assertEqualsTyped(2, dis.bitPos0b)
    val expected = BigInt(3)
    assertEqualsTyped[BigInt](expected, ml)
  }

  @Test def testUnsignedBigInt2 {
    val dis = ByteBufferDataInputStream(List(0xC1, 0xC2, 0xC3, 0xC4, 0xC5).map { _.toByte }.toArray)
    val ml = dis.getUnsignedBigInt(40, finfo)
    assertEqualsTyped(40, dis.bitPos0b)
    val expected = BigInt(0xC1C2C3C4C5L)
    assertEqualsTyped[BigInt](expected, ml)
  }

  @Test def testUnsignedBigInt3 {
    val dat = "7766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100"
    val dats = dat.sliding(2, 2).toList.flatMap { Misc.hex2Bytes(_) }.toArray
    val dis = ByteBufferDataInputStream(dats)
    val finfo = FormatInfoForUnitTest()
    finfo.byteOrder = ByteOrder.LittleEndian
    finfo.bitOrder = BitOrder.LeastSignificantBitFirst
    val mbi = dis.getUnsignedBigInt(dat.length * 4, finfo)
    val expected = BigInt(dat.reverse, 16)
    val expectedHex = "%x".format(expected)
    val actualHex = "%x".format(mbi)
    assertEqualsTyped[String](expectedHex, actualHex)
  }

  @Test def testUnsignedBigInt4 {
    val expectedHex = "00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff0011223344556677"
    assertEqualsTyped(720, expectedHex.length)
    val expected = BigInt(expectedHex, 16)
    val valueWithExtraLSByte = expected << 8
    assertEqualsTyped[Long](0, (valueWithExtraLSByte & 0xFF).toInt)
    assertEqualsTyped[Long](0x77, (valueWithExtraLSByte.toByteArray.dropRight(1).last & 0xFF).toInt)
    val valueWith3BitsInLSByte = valueWithExtraLSByte >> 3
    assertEqualsTyped[Long](0xE0, (valueWith3BitsInLSByte & 0xFF).toInt)
    val valueWith3BitsInLSByteAsHexAsHexBytesLittleEndian = valueWith3BitsInLSByte.toByteArray.toList.reverse :+ 0.toByte
    val dat = valueWith3BitsInLSByteAsHexAsHexBytesLittleEndian.toArray
    assertEqualsTyped[Long](361, dat.length)
    assertEqualsTyped[Long](0xE0.toByte, dat.head)
    val dis = ByteBufferDataInputStream(dat, 5)
    val finfo = FormatInfoForUnitTest()
    finfo.byteOrder = ByteOrder.LittleEndian
    finfo.bitOrder = BitOrder.LeastSignificantBitFirst
    val nBits = (expectedHex.length * 4)
    val mbi = dis.getUnsignedBigInt(nBits, finfo)
    val actual = mbi
    val actualHex = "%x".format(actual)
    val expectedHexNoLeadingZeros = "%x".format(expected)
    assertEqualsTyped[String](expectedHexNoLeadingZeros, actualHex)
  }

  @Test def testAlignAndSkip1 {
    val dis = ByteBufferDataInputStream(List(0xC1, 0xC2, 0xC3, 0xC4, 0xC5).map { _.toByte }.toArray)
    assertTrue(dis.isAligned(1))
    assertTrue(dis.isAligned(43))
    dis.getSignedLong(1, finfo)
    dis.align(4, finfo)
    assertEqualsTyped[Long](4, dis.bitPos0b)
    assertTrue(dis.isAligned(2))
    assertFalse(dis.isAligned(8))
    dis.skip(3, finfo)
    assertEqualsTyped[Long](7, dis.bitPos0b)
  }

  @Test def testFillCharBuffer1 {
    val dis = ByteBufferDataInputStream("1".getBytes())
    val cb = CharBuffer.allocate(1)
    val ml = dis.fillCharBuffer(cb, finfo)
    assertTrue(ml.isDefined)
    assertEqualsTyped[Long](1, ml.get.toLong)
    assertEqualsTyped[Long](8, dis.bitPos0b)
    assertEqualsTyped[Char]('1', cb.get(0))
  }

  @Test def testFillCharBuffer2 {
    val dis = ByteBufferDataInputStream("年月日".getBytes("utf-8"))
    val cb = CharBuffer.allocate(3)
    val ml = dis.fillCharBuffer(cb, finfo)
    assertTrue(ml.isDefined)
    assertEqualsTyped[Long](3, ml.get.toLong)
    assertEqualsTyped[Char]('年', cb.get(0))
    assertEqualsTyped[Char]('月', cb.get(1))
    assertEqualsTyped[Char]('日', cb.get(2))
    assertEqualsTyped[Long](72, dis.bitPos0b)
  }

  @Test def testFillCharBuffer3 {
    val dis = ByteBufferDataInputStream("年月日".getBytes("utf-8"))
    dis.setBitLimit0b(MaybeULong(8 * 6))
    val cb = CharBuffer.allocate(3)
    val ml = dis.fillCharBuffer(cb, finfo)
    assertTrue(ml.isDefined)
    assertEqualsTyped[Long](2, ml.get.toLong)
    assertEqualsTyped[Char]('年', cb.get(0))
    assertEqualsTyped[Char]('月', cb.get(1))
    assertEqualsTyped[Long](8 * 6, dis.bitPos0b)
  }

  def unicodeReplacementCharacter = '\uFFFD'

  @Test def testFillCharBufferErrors1 {
    val data = List(0xFF.toByte).toArray ++ "年月日".getBytes("utf-8")
    assertEqualsTyped(10, data.length)
    val dis = ByteBufferDataInputStream(data)
    val finfo = FormatInfoForUnitTest()
    finfo.decoder = finfo.replacingDecoder
    val cb = CharBuffer.allocate(20)
    val ml = dis.fillCharBuffer(cb, finfo)
    assertTrue(ml.isDefined)
    assertEqualsTyped[Long](4, ml.get.toLong)
    assertEqualsTyped[Char](unicodeReplacementCharacter, cb.get(0))
    assertEqualsTyped[Char]('年', cb.get(1))
    assertEqualsTyped[Char]('月', cb.get(2))
    assertEqualsTyped[Char]('日', cb.get(3))
    assertEqualsTyped[Long](80, dis.bitPos0b)
  }

  @Test def testFillCharBufferErrors2 {
    val badByte = List(0xFF.toByte).toArray
    val data = "abc".getBytes("utf-8") ++ badByte ++ "123".getBytes("utf-8") ++ badByte ++ badByte ++ "drm".getBytes("utf-8")
    val dis = ByteBufferDataInputStream(data)
    val finfo = FormatInfoForUnitTest()
    finfo.decoder = finfo.replacingDecoder
    val cb = CharBuffer.allocate(20)
    var ml = dis.fillCharBuffer(cb, finfo)
    cb.flip
    assertTrue(ml.isDefined)
    assertEqualsTyped[Long](3, ml.get.toLong)
    var str: String = Misc.csToString(cb)
    assertEqualsTyped[String]("abc", str)
    cb.clear
    ml = dis.fillCharBuffer(cb, finfo)
    cb.flip
    assertTrue(ml.isDefined)
    assertEqualsTyped(9, ml.get)
    str = Misc.csToString(cb)
    assertEqualsTyped[String]("\uFFFD123\uFFFD\uFFFDdrm", str)
    assertEqualsTyped[Long](data.length * 8, dis.bitPos0b)
  }

  @Test def testFillCharBufferErrors3 {
    val badByte = List(0xFF.toByte).toArray
    val data = "abc".getBytes("utf-8") ++ badByte ++ "123".getBytes("utf-8") ++ badByte ++ badByte ++ "drm".getBytes("utf-8")
    val dis = ByteBufferDataInputStream(data)
    val finfo = FormatInfoForUnitTest()
    finfo.encodingErrorPolicy = EncodingErrorPolicy.Error
    finfo.decoder.onMalformedInput(CodingErrorAction.REPORT)
    finfo.decoder.onUnmappableCharacter(CodingErrorAction.REPORT)
    val cb = CharBuffer.allocate(20)
    var ml = dis.fillCharBuffer(cb, finfo)
    cb.flip
    assertTrue(ml.isDefined)
    assertEqualsTyped[Long](3, ml.get.toLong)
    val str: String = Misc.csToString(cb)
    assertEqualsTyped("abc", str)
    cb.clear
    val e = intercept[MalformedInputException] {
      ml = dis.fillCharBuffer(cb, finfo)
    }
    assertEqualsTyped[Long](1, e.getInputLength())
  }

  @Test def testCharIterator1 {
    val dis = ByteBufferDataInputStream("年月日".getBytes("utf-8"))
    val iter = dis.asIteratorChar
    iter.setFormatInfo(finfo)
    dis.setBitLimit0b(MaybeULong(8 * 6))
    assertTrue(iter.hasNext)
    assertEqualsTyped[Long](0, dis.bitPos0b)
    assertEqualsTyped('年', iter.next)
    assertEqualsTyped[Long](24, dis.bitPos0b)
    assertTrue(iter.hasNext)
    assertEqualsTyped[Long](24, dis.bitPos0b)
    assertEqualsTyped('月', iter.next)
    assertEqualsTyped[Long](48, dis.bitPos0b)
    assertFalse(iter.hasNext)
  }

  @Test def testCharIteratorErrors1 {
    val badByte = List(0xFF.toByte).toArray
    val data = "abc".getBytes("utf-8") ++ badByte ++ "123".getBytes("utf-8") ++ badByte ++ badByte ++ "drm".getBytes("utf-8")
    val dis = ByteBufferDataInputStream(data)
    val finfo = FormatInfoForUnitTest()
    finfo.encodingErrorPolicy = EncodingErrorPolicy.Error
    finfo.decoder.onMalformedInput(CodingErrorAction.REPORT)
    finfo.decoder.onUnmappableCharacter(CodingErrorAction.REPORT)
    finfo.encodingErrorPolicy = EncodingErrorPolicy.Error
    val iter = dis.asIteratorChar
    iter.setFormatInfo(finfo)
    dis.setBitLimit0b(MaybeULong(8 * 6))
    assertTrue(iter.hasNext)
    assertEqualsTyped[Long](0, dis.bitPos0b)
    val sb = new StringBuilder
    sb + iter.next
    sb + iter.next
    sb + iter.next
    assertEqualsTyped("abc", sb.mkString)
    assertEqualsTyped[Long](24, dis.bitPos0b)
    intercept[MalformedInputException] {
      iter.hasNext
    }
    assertEqualsTyped[Long](24, dis.bitPos0b)
  }

  @Test def testLookingAt1 {
    val data = "abc".getBytes("utf-8")
    val dis = ByteBufferDataInputStream(data)
    val pattern = Pattern.compile("a")
    val matcher = pattern.matcher("")
    val isMatch = dis.lookingAt(matcher, finfo)
    assertTrue(isMatch)
    assertEqualsTyped[Long](0, matcher.start)
    assertEqualsTyped[Long](1, matcher.end)
    assertEqualsTyped("a", matcher.group())
    assertEqualsTyped[Long](8, dis.bitPos0b)
  }

  /**
   * Illustrates that you cannot restart a match that is
   * partway through the regex.
   */
  @Test def testCharacterizeMatcherAfterHitEndRequireEnd1 {
    val pat = Pattern.compile("a*")
    val m = pat.matcher("")
    val cb = CharBuffer.wrap("aaaaa")
    cb.limit(1) // allow just first character
    m.reset(cb)
    var isMatch = m.lookingAt()
    assertTrue(isMatch)
    var hitEnd = m.hitEnd
    assertTrue(hitEnd)
    var requireEnd = m.requireEnd()
    assertTrue(!requireEnd)
    cb.limit(2) // allow one char more data
    isMatch = m.lookingAt()
    assertTrue(isMatch)
    hitEnd = m.hitEnd
    assertTrue(hitEnd)
    requireEnd = m.requireEnd()
    assertTrue(!requireEnd)
    val start = m.start
    assertEqualsTyped[Long](0, start)
    val end = m.end
    // we want this to be 2. Bzzt. We were hoping it would be
    // because the matcher picked up where it left off and now
    // has "aa" as the match. But the matchers just don't
    // work that way.
    assertEqualsTyped[Long](1, end)
  }

  @Test def testCharacterizeMatcherAfterHitEndRequireEnd2 {
    val pat = Pattern.compile("a*b")
    val m = pat.matcher("")
    val cb = CharBuffer.wrap("aaab")
    m.reset(cb)
    val sb = new StringBuilder
    val isMatch = m.lookingAt()
    assertTrue(isMatch)
    val hitEnd = m.hitEnd
    assertTrue(!hitEnd) // because we matched a b (not a b* or b+) we didn't have to look further so did not hit end.
    val requireEnd = m.requireEnd()
    assertTrue(!requireEnd)
    val start = m.start
    assertEqualsTyped[Long](0, start)
    val end = m.end
    assertEqualsTyped[Long](4, end)
    val group = m.group
    assertEqualsTyped("aaab", group)
    sb + group
  }

  /**
   * Based on the behavior of this test, it appears a matcher
   * isn't restartable after a hitEnd or requireEnd. Rather
   * these advise you to get more data, and then retry the *ENTIRE*
   * match again.
   *
   * There doesn't appear to be a mechanism for suspending
   * the match when it is partway through matching the regex, and
   * then resuming with more data because of hitEnd or requireEnd
   *
   * An algorithm that is sensible is one that tries to find the match
   * and then if characters are variable width, on a success it takes
   * the match text, and re-does the scan, but with a limit such
   * that it will only grab that same number of characters. Then
   * the nBytesConsumed by the fillCharBuffer gives the right
   * length.
   */
  @Test def testCharacterizeMatcherAfterHitEndRequireEnd2a {
    val pat = Pattern.compile("aaa*b")
    val m = pat.matcher("")
    val cb = CharBuffer.wrap("aaab")
    cb.limit(2)
    m.reset(cb)
    var isMatch = m.lookingAt()
    assertTrue(!isMatch) // not a match for the whole regex yet
    var hitEnd = m.hitEnd
    assertTrue(hitEnd)
    cb.limit(4) // allow more data
    cb.position(0)
    isMatch = m.lookingAt()
    assertTrue(!isMatch)
    m.reset(cb)
    isMatch = m.lookingAt()
    assertTrue(isMatch)
    val group = m.group
    assertEqualsTyped("aaab", group)
    hitEnd = m.hitEnd
    assertTrue(!hitEnd)
  }

  @Test def testLookingAt2 {
    val data = "abc".getBytes("utf-8")
    val dis = ByteBufferDataInputStream(data)
    val pattern = Pattern.compile("a*b+c")
    val matcher = pattern.matcher("")
    val isMatch = dis.lookingAt(matcher, finfo)
    assertTrue(isMatch)
    assertEqualsTyped[Long](0, matcher.start)
    assertEqualsTyped[Long](3, matcher.end)
    assertEqualsTyped("abc", matcher.group())
    assertEqualsTyped[Long](24, dis.bitPos0b)
  }

  @Test def testLookingAtManyMultibyteCharsAndDecodeError1 {
    val dataString1 = "abc年de月fg日"
    val data1 = dataString1.getBytes("utf-8")
    val badByte = List(0xFF.toByte).toArray
    val dataString3 = "хив"
    val data3 = dataString3.getBytes("utf-8")
    val data = data1 ++ badByte ++ data3
    val dis = ByteBufferDataInputStream(data)
    val pattern = Pattern.compile(".*")
    val matcher = pattern.matcher("")
    val isMatch = dis.lookingAt(matcher, finfo)
    assertTrue(isMatch)
    assertEqualsTyped[Long](0, matcher.start)
    assertEqualsTyped[Long](14, matcher.end)
    val actual = matcher.group()
    assertEqualsTyped("abc年de月fg日\uFFFDхив", actual)
    assertEqualsTyped[Long](data.length * 8, dis.bitPos0b)
  }

  @Test def testLookingAtManyMultibyteCharsAndDecodeError2 {
    val dataString1 = "abc年de月fg日"
    val data1 = dataString1.getBytes("utf-8")
    val badByte = List(0xFF.toByte).toArray
    val dataString3 = "хив"
    val data3 = dataString3.getBytes("utf-8")
    val data = data1 ++ badByte ++ data3
    val dis = ByteBufferDataInputStream(data)
    val pattern = Pattern.compile(".{1,13}")
    val matcher = pattern.matcher("")
    val isMatch = dis.lookingAt(matcher, finfo)
    assertTrue(isMatch)
    assertEqualsTyped[Long](0, matcher.start)
    assertEqualsTyped[Long](13, matcher.end)
    val actual = matcher.group()
    assertEqualsTyped("abc年de月fg日\uFFFDхи", actual)
    val expectedByteLength = data1.length + 1 + "хи".getBytes("utf-8").length
    assertEqualsTyped[Long](expectedByteLength * 8, dis.bitPos0b)
  }

  @Test def testLookingAtManyMultibyteCharsAndDecodeError3 {
    val enc = "utf-16BE"
    val dataString1 = "abc年de月fg日"
    val data1 = dataString1.getBytes(enc)
    val badByte = List(0xDC.toByte, 0x00.toByte).toArray
    val dataString3 = "хив"
    val data3 = dataString3.getBytes(enc)
    val data = data1 ++ badByte ++ data3
    val dis = ByteBufferDataInputStream(data)
    val finfo = FormatInfoForUnitTest()
    finfo.reset(StandardBitsCharsets.UTF_16BE)
    val pattern = Pattern.compile(".{1,13}")
    val matcher = pattern.matcher("")
    val isMatch = dis.lookingAt(matcher, finfo)
    assertTrue(isMatch)
    assertEqualsTyped[Long](0, matcher.start)
    assertEqualsTyped[Long](13, matcher.end)
    val actual = matcher.group()
    assertEqualsTyped("abc年de月fg日\uFFFDхи", actual)
    val expectedByteLength = data1.length + badByte.length + "хи".getBytes(enc).length
    assertEqualsTyped[Long](expectedByteLength * 8, dis.bitPos0b)
  }

  @Test def testDotMatchesNewline1 {
    // val enc = "iso-8859-1"
    val dataURI = Misc.getRequiredResource("iso8859.doc.dat")
    val dataInput = dataURI.toURL.openStream()
    val dis = ByteBufferDataInputStream(dataInput, 0)
    val regex = """(?x)(?s) # free spacing mode, dot matches newlines too
            .{1,8192}?                # up to 8K of front matter page content
            \f?                       # a form-feed
            (?=                       # lookahead (followed by but not including...)
              (?> \s | \x08 ){1,100}? # whitespace or backspace (x08)
              MESSAGE\ DESCRIPTION\r
              \s{1,100}?
              -{19}\r                 # exactly 19 hyphens and a CR
            )                         # end lookahead """
    val pattern = Pattern.compile(regex)
    val matcher = pattern.matcher("")
    val isMatch = dis.lookingAt(matcher, finfo)
    assertFalse(isMatch)
    assertTrue(matcher.hitEnd)
  }

}
