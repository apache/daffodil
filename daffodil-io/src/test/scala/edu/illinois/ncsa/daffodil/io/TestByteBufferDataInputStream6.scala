package edu.illinois.ncsa.daffodil.io

import org.junit.Test
import org.junit.Assert._
import edu.illinois.ncsa.daffodil.util.Misc
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import edu.illinois.ncsa.daffodil.util.Bits
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import java.nio.CharBuffer
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.EncodingErrorPolicy
import java.nio.charset.MalformedInputException
import edu.illinois.ncsa.daffodil.util.MaybeULong

class TestByteBufferDataInputStream6 {

  @Test def testUnalignedByteBufferMSBFirst() {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.BIG_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val data = 0x0102030405060708L
    val expected = data << 1
    fb.put(data)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.BigEndian)
    dis.setBitOrder(BitOrder.MostSignificantBitFirst)
    dis.skip(1) // move over one bit
    val resultBuf = ByteBuffer.allocate(8)
    val mNBytes = dis.fillByteBuffer(resultBuf)
    resultBuf.flip
    assertEquals(8, mNBytes.get)
    val actual = resultBuf.asLongBuffer().get()
    assertEquals(expected, actual)
    assertEquals(64, dis.bitPos0b)
  }

  @Test def testUnalignedByteBufferLittleEndianMSBFirst() {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val data = 0x0102030405060708L
    val expected = 0x0807060504030201L << 1
    fb.put(data)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    dis.setBitOrder(BitOrder.MostSignificantBitFirst)
    dis.skip(1)
    assertEquals(1, dis.bitPos0b)
    val resultBuf = ByteBuffer.allocate(8)
    val mNBytes = dis.fillByteBuffer(resultBuf)
    resultBuf.flip
    assertEquals(8, mNBytes.get)
    val actual = resultBuf.asLongBuffer().get()
    assertEquals(expected, actual)
    assertEquals(64, dis.bitPos0b)
  }

  @Test def testUnalignedByteBufferLittleEndianLSBFirst() {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val data = 0x0102030405060708L
    val expected = 0x8403830282018100L
    fb.put(data)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    dis.setBitOrder(BitOrder.LeastSignificantBitFirst)
    dis.skip(1)
    val resultBuf = ByteBuffer.allocate(8)
    val mNBytes = dis.fillByteBuffer(resultBuf)
    resultBuf.flip
    assertEquals(8, mNBytes.get)
    val actual = resultBuf.asLongBuffer().get()
    assertEquals(expected, actual)
    assertEquals(64, dis.bitPos0b)
  }

  /**
   * Tests of unaligned char buffer - when charset has mandatory 8-bit alignment
   *
   * These just insure that we move over to the mandatory alignment before decoding
   * any characters.
   */
  @Test def testFillCharBuffer1 {
    val dis = ByteBufferDataInputStream("01".getBytes())
    val cb = CharBuffer.allocate(1)
    dis.getSignedLong(1)
    val ml = dis.fillCharBuffer(cb)
    assertTrue(ml.isDefined)
    assertEquals(1, ml.get)
    assertEquals(16, dis.bitPos0b)
    assertEquals('1', cb.get(0))
  }

  @Test def testFillCharBuffer2 {
    val dis = ByteBufferDataInputStream("0年月日".getBytes("utf-8"))
    val cb = CharBuffer.allocate(3)
    dis.getSignedLong(4)
    val ml = dis.fillCharBuffer(cb)
    assertTrue(ml.isDefined)
    assertEquals(3, ml.get)
    assertEquals('年', cb.get(0))
    assertEquals('月', cb.get(1))
    assertEquals('日', cb.get(2))
    assertEquals(80, dis.bitPos0b)
  }

  @Test def testFillCharBufferDataEndsMidByte {
    val dis = ByteBufferDataInputStream("年月日".getBytes("utf-8"))
    dis.setBitLimit0b(MaybeULong((8 * 6) + 2)) // 2 extra bits after first 2 chars
    val cb = CharBuffer.allocate(3)
    val ml = dis.fillCharBuffer(cb)
    assertTrue(ml.isDefined)
    assertEquals(2, ml.get)
    assertEquals('年', cb.get(0))
    assertEquals('月', cb.get(1))
    assertEquals(8 * 6, dis.bitPos0b)
  }

  @Test def testFillCharBufferDataEndsMidByte2 {
    val dis = ByteBufferDataInputStream("年月日".getBytes("utf-8"))
    dis.setEncodingErrorPolicy(EncodingErrorPolicy.Replace)
    dis.setBitLimit0b(MaybeULong((8 * 6) + 2)) // 2 extra bits after first 2 chars
    val cb = CharBuffer.allocate(3)
    val ml = dis.fillCharBuffer(cb)
    assertTrue(ml.isDefined)
    assertEquals(2, ml.get)
    assertEquals('年', cb.get(0))
    assertEquals('月', cb.get(1))
    assertEquals(8 * 6, dis.bitPos0b)
    cb.clear()
    val ml2 = dis.fillCharBuffer(cb) // ask for next character
    assertEquals(MaybeULong.Nope, ml2)
  }

  @Test def testFillCharBufferDataEndsMidByte3 {
    val dis = ByteBufferDataInputStream("年月日".getBytes("utf-8"))
    dis.setEncodingErrorPolicy(EncodingErrorPolicy.Replace)
    dis.setBitLimit0b(MaybeULong((8 * 6) + 10)) // 1 more byte plus 2 extra bits after first 2 chars
    val cb = CharBuffer.allocate(3)
    val ml = dis.fillCharBuffer(cb)
    assertTrue(ml.isDefined)
    assertEquals(2, ml.get)
    assertEquals('年', cb.get(0))
    assertEquals('月', cb.get(1))
    assertEquals(8 * 6, dis.bitPos0b)
    cb.clear()
    val ml2 = dis.fillCharBuffer(cb) // ask for next character
    //
    // because it has 1 more byte available, it doesn't stop the attempt to decode
    // and that attempt fails and we replace it.
    //
    // Note that if there aren't enough bits for a single byte, then
    // we won't even decode at all. It will just return Nope.
    //
    assertEquals(MaybeULong(1), ml2)
    assertEquals(this.unicodeReplacementCharacter, cb.get(0))
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

  @Test def testCharIteratorWithInterruptingBitSkips1 {
    val dis = ByteBufferDataInputStream("0年1月2日".getBytes("utf-8"))
    val iter = dis.asIteratorChar
    dis.skip(1)
    assertTrue(iter.hasNext) // examining a character here requires aligning to mandatory alignment of 8 bit boundary.
    assertEquals(1, dis.bitPos0b)
    assertEquals('年', iter.next)
    assertEquals(32, dis.bitPos0b)
    dis.skip(1)
    assertTrue(iter.hasNext)
    assertEquals(33, dis.bitPos0b)
    assertEquals('月', iter.next)
    assertEquals(64, dis.bitPos0b)
    dis.skip(1)
    assertTrue(iter.hasNext)
    assertEquals('日', iter.next)
    assertEquals(96, dis.bitPos0b)
    assertFalse(dis.skip(1))
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
  @Test def testCharIteratorWithInterruptingBitSkipsBetweenHasNextAndNext {
    val dis = ByteBufferDataInputStream("0年1月2日".getBytes("utf-8"))
    val iter = dis.asIteratorChar
    dis.skip(1)
    assertTrue(iter.hasNext) // examining a character here requires aligning to mandatory alignment of 8 bit boundary.
    assertEquals(1, dis.bitPos0b)
    dis.skip(1) // this skip should invalidate the character cached by hasNext.
    assertEquals(2, dis.bitPos0b)
    assertTrue(iter.hasNext)
    assertEquals(2, dis.bitPos0b)
    val c = iter.next
    assertEquals(32, dis.bitPos0b) // has next doesn't cause movement even to align to mandatory.
    assertEquals('年', c)
    assertTrue(iter.hasNext)
    dis.skip(4)
    assertEquals(36, dis.bitPos0b)
    val d = iter.next
    assertEquals(64, dis.bitPos0b)
    assertEquals('月', d)
  }

}
