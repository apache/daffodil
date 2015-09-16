package edu.illinois.ncsa.daffodil.io

import org.junit.Test
import org.junit.Assert._
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import passera.unsigned.ULong
import java.nio.CharBuffer
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import edu.illinois.ncsa.daffodil.Implicits._
import java.nio.charset.CharacterCodingException
import java.nio.charset.MalformedInputException
import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer
import java.nio.charset.StandardCharsets
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder

class TestByteBufferDataInputStream {
  val tenDigits = "1234567890"
  val ten = tenDigits.getBytes("utf-8")
  val twentyDigits = tenDigits * 2
  val twenty = twentyDigits.getBytes("utf-8")

  @Test def testBitAndBytePos0 {
    val dis = ByteBufferDataInputStream(ten)
    assertEquals(0, dis.bitPos0b)
    assertEquals(80, dis.bitLimit0b.get)
    assertEquals(1, dis.bitPos1b)
    assertEquals(81, dis.bitLimit1b.get)
    assertEquals(0, dis.bytePos0b)
  }

  @Test def testBitAndBytePos1 {
    val dis = ByteBufferDataInputStream(ten)
    val bb = ByteBuffer.allocate(1)
    val n = dis.fillByteBuffer(bb)
    assertTrue(n.isDefined)
    assertEquals(1, n.get)
    bb.flip()
    assertEquals(0x31.toByte, bb.get())
    assertEquals(8, dis.bitPos0b)
    assertEquals(80, dis.bitLimit0b.get)
    assertEquals(9, dis.bitPos1b)
    assertEquals(81, dis.bitLimit1b.get)
    assertEquals(1, dis.bytePos0b)
  }

  @Test def testBitAndBytePos10 {
    val dis = ByteBufferDataInputStream(ten)
    val bb = ByteBuffer.allocate(10)
    val n = dis.fillByteBuffer(bb)
    assertTrue(n.isDefined)
    assertEquals(10, n.get)
    bb.flip()
    1 to 9 foreach { _ => bb.get() }
    assertEquals(0x30.toByte, bb.get())
    assertEquals(80, dis.bitPos0b)
    assertEquals(80, dis.bitLimit0b.get)
    assertEquals(81, dis.bitPos1b)
    assertEquals(81, dis.bitLimit1b.get)
    assertEquals(10, dis.bytePos0b)
  }

  @Test def testBitAndBytePosNotEnoughData1 {
    val dis = ByteBufferDataInputStream(ten)
    val bb = ByteBuffer.allocate(11)
    val n = dis.fillByteBuffer(bb)
    assertTrue(n.isDefined)
    assertEquals(10, n.get)
    bb.flip()
    1 to 9 foreach { _ => bb.get() }
    assertEquals(0x30.toByte, bb.get())
    assertEquals(80, dis.bitPos0b)
    assertEquals(80, dis.bitLimit0b.get)
    assertEquals(10, dis.bytePos0b)
  }

  @Test def testBitAndBytePosMoreThanEnoughData1 {
    val dis = ByteBufferDataInputStream(twenty)
    val bb = ByteBuffer.allocate(10)
    val n = dis.fillByteBuffer(bb)
    assertTrue(n.isDefined)
    assertEquals(10, n.get)
    bb.flip()
    1 to 9 foreach { _ => bb.get() }
    assertEquals(0x30.toByte, bb.get())
    assertEquals(80, dis.bitPos0b)
    assertEquals(160, dis.bitLimit0b.get)
    assertEquals(10, dis.bytePos0b)
  }

  @Test def testBitLengthLimit1 {
    val dis = ByteBufferDataInputStream(twenty)
    val bb = ByteBuffer.allocate(20)
    val isLimitOk = dis.withBitLengthLimit(80) {
      val maybeNBytes = dis.fillByteBuffer(bb)
      assertEquals(80, dis.bitLimit0b.get)
      assertEquals(80, dis.bitPos0b)
      assertTrue(maybeNBytes.isDefined)
      assertEquals(10, maybeNBytes.get)
    }
    assertTrue(isLimitOk)
    bb.flip()
    1 to 9 foreach { _ => bb.get() }
    assertEquals(0x30.toByte, bb.get())
    assertEquals(80, dis.bitPos0b)
    assertEquals(160, dis.bitLimit0b.get)
    assertEquals(10, dis.bytePos0b)
  }

  @Test def testBinaryDouble1 {
    val dis = ByteBufferDataInputStream("123".getBytes("utf-8"))
    val expected = Nope
    val md = dis.getBinaryDouble()
    assertEquals(expected, md)
    assertEquals(0, dis.bitPos0b)
  }

  @Test def testBinaryDouble2 {
    val dis = ByteBufferDataInputStream(twenty)
    val expected = ByteBuffer.wrap(twenty).asDoubleBuffer().get()
    val md = dis.getBinaryDouble()
    assertEquals(expected, md.get, 0.0)
  }

  @Test def testBinaryDouble3 {
    val dis = ByteBufferDataInputStream(twenty)
    dis.setBitLimit1b(One(63)) // not enough bits
    val expected = Nope
    val md = dis.getBinaryDouble()
    assertEquals(expected, md)
  }

  @Test def testBinaryDouble4 {
    val dis = ByteBufferDataInputStream(twenty)
    val bb = ByteBuffer.allocate(1)
    dis.fillByteBuffer(bb)
    dis.setBitLimit1b(One(71)) // not enough bits
    val expected = Nope
    val md = dis.getBinaryDouble()
    assertEquals(expected, md)
  }

  @Test def testBinaryFloat1 {
    val dis = ByteBufferDataInputStream("123".getBytes("utf-8"))
    val expected = Nope
    val md = dis.getBinaryFloat()
    assertEquals(expected, md)
    assertEquals(0, dis.bitPos0b)
  }

  @Test def testBinaryFloat2 {
    val dis = ByteBufferDataInputStream(twenty)
    val expected = ByteBuffer.wrap(twenty).asFloatBuffer().get()
    val md = dis.getBinaryFloat()
    assertEquals(expected, md.get, 0.0)
  }

  @Test def testBinaryFloat3 {
    val dis = ByteBufferDataInputStream(twenty)
    dis.setBitLimit1b(One(31)) // not enough bits
    val expected = Nope
    val md = dis.getBinaryFloat()
    assertEquals(expected, md)
  }

  @Test def testBinaryFloat4 {
    val dis = ByteBufferDataInputStream(twenty)
    val bb = ByteBuffer.allocate(1)
    dis.fillByteBuffer(bb)
    dis.setBitLimit1b(One(39)) // not enough bits
    val expected = Nope
    val md = dis.getBinaryFloat()
    assertEquals(expected, md)
  }

  @Test def testSignedLong1 {
    val dis = ByteBufferDataInputStream(twenty)
    dis.setBitLimit1b(One(1))
    var ml = dis.getSignedLong(1)
    assertEquals(Nope, ml)
  }

  @Test def testSignedLong2 {
    val dis = ByteBufferDataInputStream(twenty)
    dis.setBitLimit1b(One(63))
    var ml = dis.getSignedLong(64)
    assertEquals(Nope, ml)
  }

  @Test def testSignedLong3 {
    val dis = ByteBufferDataInputStream(twenty)
    // buffer has 0x3132 in first 16 bits
    // binary that is 00110001 00110010
    var ml = dis.getSignedLong(1)
    assertEquals(0L, ml.get)
    assertEquals(1, dis.bitPos0b)
    ml = dis.getSignedLong(9)
    // those bits are 0110001 00 which is 0x0C4 and sign bit is 0 (positive)
    assertEquals(0x0C4L, ml.get)
    assertEquals(10, dis.bitPos0b)
  }

  @Test def testSignedLong4 {
    val dis = ByteBufferDataInputStream(twenty)
    var ml = dis.getSignedLong(1)
    assertEquals(1, dis.bitPos0b)
    ml = dis.getSignedLong(64)
    assertEquals(65, dis.bitPos0b)
    assertEquals(0x3132333435363738L << 1, ml.get)
  }

  @Test def testSignedLong5 {
    val dis = ByteBufferDataInputStream(List(0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xC0).map { _.toByte }.toArray)
    var ml = dis.getSignedLong(1)
    assertEquals(1, dis.bitPos0b)
    assertEquals(1, ml.get)
    ml = dis.getSignedLong(64)
    assertEquals(65, dis.bitPos0b)
    assertEquals((0xC1C2C3C4C5C6C7C8L << 1) + (0xC9 >>> 7), ml.get)
  }

  @Test def testSignedLong6 {
    val dis = ByteBufferDataInputStream(List(0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xC0).map { _.toByte }.toArray)
    var ml = dis.getSignedLong(1)
    assertEquals(1, dis.bitPos0b)
    assertEquals(1, ml.get)
    ml = dis.getSignedLong(32)
    assertEquals(33, dis.bitPos0b)
    val expected = (((0xC1C2C3C4C5L >> 7) & 0xFFFFFFFFL) << 32) >> 32 // sign extend
    assertEquals(expected, ml.get)
  }

  @Test def testSignedLong7 {
    val dis = ByteBufferDataInputStream(List(0xC1, 0xC2, 0xC3, 0xC4, 0xC5).map { _.toByte }.toArray)
    var ml = dis.getSignedLong(2)
    assertEquals(2, dis.bitPos0b)
    assertEquals(-1, ml.get)
    ml = dis.getSignedLong(32)
    assertEquals(34, dis.bitPos0b)
    val expected = (0xC1C2C3C4C5L >> 6) & 0xFFFFFFFFL // will be positive, no sign extend
    assertEquals(expected, ml.get)
  }

  @Test def testUnsignedLong1 {
    val dis = ByteBufferDataInputStream(List(0xC1, 0xC2, 0xC3, 0xC4, 0xC5).map { _.toByte }.toArray)
    var ml = dis.getUnsignedLong(32)
    assertEquals(32, dis.bitPos0b)
    val expected = ULong(0xC1C2C3C4L)
    assertEquals(expected, ml.get)
  }

  @Test def testUnsignedLong2 {
    val dis = ByteBufferDataInputStream(List(0xA5, 0xA5, 0xA5, 0xA5, 0xA5).map { _.toByte }.toArray)
    dis.getSignedLong(1)
    assertEquals(1, dis.bitPos0b)
    var ml = dis.getUnsignedLong(32)
    assertEquals(33, dis.bitPos0b)
    val expected = ULong(0x4b4b4b4bL)
    assertEquals(expected, ml.get)
  }

  @Test def testUnsignedLong3 {
    val dis = ByteBufferDataInputStream(List(0xFF).map { _.toByte }.toArray)
    var ml = dis.getUnsignedLong(1)
    assertEquals(1, dis.bitPos0b)
    val expected = ULong(1)
    assertEquals(expected, ml.get)
  }

  @Test def testSignedBigInt1 {
    val dis = ByteBufferDataInputStream(List(0xFF).map { _.toByte }.toArray)
    var ml = dis.getSignedBigInt(1)
    assertEquals(1, dis.bitPos0b)
    val expected = BigInt(1)
    assertEquals(expected, ml.get)
  }

  @Test def testSignedBigInt2 {
    val dis = ByteBufferDataInputStream(List(0xC1, 0xC2, 0xC3, 0xC4, 0xC5).map { _.toByte }.toArray)
    var ml = dis.getSignedBigInt(40)
    assertEquals(40, dis.bitPos0b)
    val expected = BigInt(0xFFFFFFC1C2C3C4C5L)
    assertEquals(expected, ml.get)
  }

  @Test def testUnsignedBigInt1 {
    val dis = ByteBufferDataInputStream(List(0xFF).map { _.toByte }.toArray)
    var ml = dis.getUnsignedBigInt(2)
    assertEquals(2, dis.bitPos0b)
    val expected = BigInt(3)
    assertEquals(expected, ml.get)
  }

  @Test def testUnsignedBigInt2 {
    val dis = ByteBufferDataInputStream(List(0xC1, 0xC2, 0xC3, 0xC4, 0xC5).map { _.toByte }.toArray)
    var ml = dis.getUnsignedBigInt(40)
    assertEquals(40, dis.bitPos0b)
    val expected = BigInt(0xC1C2C3C4C5L)
    assertEquals(expected, ml.get)
  }

  @Test def testUnsignedBigInt3 {
    val dat = "7766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100"
    val dats = dat.sliding(2, 2).toList.flatMap { Misc.hex2Bytes(_) }.toArray
    val dis = ByteBufferDataInputStream(dats)
    dis.setByteOrder(ByteOrder.LittleEndian)
    dis.setBitOrder(BitOrder.LeastSignificantBitFirst)
    val mbi = dis.getUnsignedBigInt(dat.length * 4)
    assertTrue(mbi.isDefined)
    val expected = BigInt(dat.reverse, 16)
    val expectedHex = "%x".format(expected)
    val actualHex = "%x".format(mbi.get)
    assertEquals(expectedHex, actualHex)
  }

  @Test def testUnsignedBigInt4 {
    val expectedHex = "00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff0011223344556677"
    assertEquals(720, expectedHex.length)
    val expected = BigInt(expectedHex, 16)
    val valueWithExtraLSByte = expected << 8
    assertEquals(0, (valueWithExtraLSByte & 0xFF).toInt)
    assertEquals(0x77, (valueWithExtraLSByte.toByteArray.dropRight(1).last & 0xFF).toInt)
    val valueWith3BitsInLSByte = valueWithExtraLSByte >> 3
    assertEquals(0xE0, (valueWith3BitsInLSByte & 0xFF).toInt)
    val valueWith3BitsInLSByteAsHexAsHexBytesLittleEndian = valueWith3BitsInLSByte.toByteArray.toList.reverse :+ 0.toByte
    val dat = valueWith3BitsInLSByteAsHexAsHexBytesLittleEndian.toArray
    assertEquals(361, dat.length)
    assertEquals(0xE0.toByte, dat.head)
    val dis = ByteBufferDataInputStream(dat, 5)
    dis.setByteOrder(ByteOrder.LittleEndian)
    dis.setBitOrder(BitOrder.LeastSignificantBitFirst)
    val nBits = (expectedHex.length * 4)
    val mbi = dis.getUnsignedBigInt(nBits)
    assertTrue(mbi.isDefined)
    val actual = mbi.get
    val actualHex = "%x".format(actual)
    val expectedHexNoLeadingZeros = "%x".format(expected)
    assertEquals(expectedHexNoLeadingZeros, actualHex)
  }

  @Test def testAlignAndSkip1 {
    val dis = ByteBufferDataInputStream(List(0xC1, 0xC2, 0xC3, 0xC4, 0xC5).map { _.toByte }.toArray)
    assertTrue(dis.isAligned(1))
    assertTrue(dis.isAligned(43))
    dis.getSignedLong(1)
    dis.align(4)
    assertEquals(4, dis.bitPos0b)
    assertTrue(dis.isAligned(2))
    assertFalse(dis.isAligned(8))
    dis.skip(3)
    assertEquals(7, dis.bitPos0b)
  }

  @Test def testFillCharBuffer1 {
    val dis = ByteBufferDataInputStream("1".getBytes())
    val cb = CharBuffer.allocate(1)
    val ml = dis.fillCharBuffer(cb)
    assertTrue(ml.isDefined)
    assertEquals(1, ml.get)
    assertEquals(8, dis.bitPos0b)
    assertEquals('1', cb.get(0))
  }

  @Test def testFillCharBuffer2 {
    val dis = ByteBufferDataInputStream("年月日".getBytes("utf-8"))
    val cb = CharBuffer.allocate(3)
    val ml = dis.fillCharBuffer(cb)
    assertTrue(ml.isDefined)
    assertEquals(3, ml.get)
    assertEquals('年', cb.get(0))
    assertEquals('月', cb.get(1))
    assertEquals('日', cb.get(2))
    assertEquals(72, dis.bitPos0b)
  }

  @Test def testFillCharBuffer3 {
    val dis = ByteBufferDataInputStream("年月日".getBytes("utf-8"))
    dis.setBitLimit0b(One(8 * 6))
    val cb = CharBuffer.allocate(3)
    val ml = dis.fillCharBuffer(cb)
    assertTrue(ml.isDefined)
    assertEquals(2, ml.get)
    assertEquals('年', cb.get(0))
    assertEquals('月', cb.get(1))
    assertEquals(8 * 6, dis.bitPos0b)
  }

  def unicodeReplacementCharacter = '\uFFFD'

  @Test def testFillCharBufferErrors1 {
    val data = List(0xFF.toByte).toArray ++ "年月日".getBytes("utf-8")
    assertEquals(10, data.length)
    val dis = ByteBufferDataInputStream(data)
    dis.setEncodingErrorPolicy(EncodingErrorPolicy.Replace)
    val cb = CharBuffer.allocate(20)
    val ml = dis.fillCharBuffer(cb)
    assertTrue(ml.isDefined)
    assertEquals(4, ml.get)
    assertEquals(unicodeReplacementCharacter, cb.get(0))
    assertEquals('年', cb.get(1))
    assertEquals('月', cb.get(2))
    assertEquals('日', cb.get(3))
    assertEquals(80, dis.bitPos0b)
  }

  @Test def testFillCharBufferErrors2 {
    val badByte = List(0xFF.toByte).toArray
    val data = "abc".getBytes("utf-8") ++ badByte ++ "123".getBytes("utf-8") ++ badByte ++ badByte ++ "drm".getBytes("utf-8")
    val dis = ByteBufferDataInputStream(data)
    dis.setEncodingErrorPolicy(EncodingErrorPolicy.Replace)
    val cb = CharBuffer.allocate(20)
    var ml = dis.fillCharBuffer(cb)
    cb.flip
    assertTrue(ml.isDefined)
    assertEquals(3, ml.get)
    var str: String = Misc.csToString(cb)
    assertEquals("abc", str)
    cb.clear
    ml = dis.fillCharBuffer(cb)
    cb.flip
    assertTrue(ml.isDefined)
    assertEquals(9, ml.get)
    str = Misc.csToString(cb)
    assertEquals("\uFFFD123\uFFFD\uFFFDdrm", str)
    assertEquals(data.length * 8, dis.bitPos0b)
  }

  @Test def testFillCharBufferErrors3 {
    val badByte = List(0xFF.toByte).toArray
    val data = "abc".getBytes("utf-8") ++ badByte ++ "123".getBytes("utf-8") ++ badByte ++ badByte ++ "drm".getBytes("utf-8")
    val dis = ByteBufferDataInputStream(data)
    dis.setEncodingErrorPolicy(EncodingErrorPolicy.Error)
    val cb = CharBuffer.allocate(20)
    var ml = dis.fillCharBuffer(cb)
    cb.flip
    assertTrue(ml.isDefined)
    assertEquals(3, ml.get)
    var str: String = Misc.csToString(cb)
    assertEquals("abc", str)
    cb.clear
    val e = intercept[MalformedInputException] {
      ml = dis.fillCharBuffer(cb)
    }
    assertEquals(1, e.getInputLength())
  }

  @Test def testCharIterator1 {
    val dis = ByteBufferDataInputStream("年月日".getBytes("utf-8"))
    val iter = dis.asIteratorChar
    dis.setBitLimit0b(One(8 * 6))
    assertTrue(iter.hasNext)
    assertEquals(0, dis.bitPos0b)
    assertEquals('年', iter.next)
    assertEquals(24, dis.bitPos0b)
    assertTrue(iter.hasNext)
    assertEquals(24, dis.bitPos0b)
    assertEquals('月', iter.next)
    assertEquals(48, dis.bitPos0b)
    assertFalse(iter.hasNext)
  }

  @Test def testCharIteratorErrors1 {
    val badByte = List(0xFF.toByte).toArray
    val data = "abc".getBytes("utf-8") ++ badByte ++ "123".getBytes("utf-8") ++ badByte ++ badByte ++ "drm".getBytes("utf-8")
    val dis = ByteBufferDataInputStream(data)
    dis.setEncodingErrorPolicy(EncodingErrorPolicy.Error)
    val iter = dis.asIteratorChar
    dis.setBitLimit0b(One(8 * 6))
    assertTrue(iter.hasNext)
    assertEquals(0, dis.bitPos0b)
    val sb = new StringBuilder
    sb + iter.next
    sb + iter.next
    sb + iter.next
    assertEquals("abc", sb.mkString)
    assertEquals(24, dis.bitPos0b)
    val e = intercept[MalformedInputException] {
      iter.hasNext
    }
    assertEquals(24, dis.bitPos0b)
  }

  @Test def testLookingAt1 {
    val data = "abc".getBytes("utf-8")
    val dis = ByteBufferDataInputStream(data)
    val pattern = Pattern.compile("a")
    val matcher = pattern.matcher("")
    val isMatch = dis.lookingAt(matcher)
    assertTrue(isMatch)
    assertEquals(0, matcher.start)
    assertEquals(1, matcher.end)
    assertEquals("a", matcher.group())
    assertEquals(8, dis.bitPos0b)
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
    var start = m.start
    assertEquals(0, start)
    var end = m.end
    // we want this to be 2. Bzzt. We were hoping it would be
    // because the matcher picked up where it left off and now
    // has "aa" as the match. But the matchers just don't 
    // work that way.
    assertEquals(1, end)
  }

  @Test def testCharacterizeMatcherAfterHitEndRequireEnd2 {
    val pat = Pattern.compile("a*b")
    val m = pat.matcher("")
    val cb = CharBuffer.wrap("aaab")
    m.reset(cb)
    val sb = new StringBuilder
    var isMatch = m.lookingAt()
    assertTrue(isMatch)
    var hitEnd = m.hitEnd
    assertTrue(!hitEnd) // because we matched a b (not a b* or b+) we didn't have to look further so did not hit end.
    var requireEnd = m.requireEnd()
    assertTrue(!requireEnd)
    var start = m.start
    assertEquals(0, start)
    var end = m.end
    assertEquals(4, end)
    var group = m.group
    assertEquals("aaab", group)
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
    var group = m.group
    assertEquals("aaab", group)
    hitEnd = m.hitEnd
    assertTrue(!hitEnd)
  }

  @Test def testLookingAt2 {
    val data = "abc".getBytes("utf-8")
    val dis = ByteBufferDataInputStream(data)
    val pattern = Pattern.compile("a*b+c")
    val matcher = pattern.matcher("")
    val isMatch = dis.lookingAt(matcher)
    assertTrue(isMatch)
    assertEquals(0, matcher.start)
    assertEquals(3, matcher.end)
    assertEquals("abc", matcher.group())
    assertEquals(24, dis.bitPos0b)
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
    val isMatch = dis.lookingAt(matcher)
    assertTrue(isMatch)
    assertEquals(0, matcher.start)
    assertEquals(14, matcher.end)
    val actual = matcher.group()
    assertEquals("abc年de月fg日\uFFFDхив", actual)
    assertEquals(data.length * 8, dis.bitPos0b)
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
    val isMatch = dis.lookingAt(matcher)
    assertTrue(isMatch)
    assertEquals(0, matcher.start)
    assertEquals(13, matcher.end)
    val actual = matcher.group()
    assertEquals("abc年de月fg日\uFFFDхи", actual)
    val expectedByteLength = data1.length + 1 + "хи".getBytes("utf-8").length
    assertEquals(expectedByteLength * 8, dis.bitPos0b)
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
    dis.setDecoder(StandardCharsets.UTF_16BE.newDecoder())
    val pattern = Pattern.compile(".{1,13}")
    val matcher = pattern.matcher("")
    val isMatch = dis.lookingAt(matcher)
    assertTrue(isMatch)
    assertEquals(0, matcher.start)
    assertEquals(13, matcher.end)
    val actual = matcher.group()
    assertEquals("abc年de月fg日\uFFFDхи", actual)
    val expectedByteLength = data1.length + badByte.length + "хи".getBytes(enc).length
    assertEquals(expectedByteLength * 8, dis.bitPos0b)
  }

  @Test def testDotMatchesNewline1 {
    val enc = "iso-8859-1"
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
    val isMatch = dis.lookingAt(matcher)
    assertFalse(isMatch)
    assertTrue(matcher.hitEnd)
  }

}
