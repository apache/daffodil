package edu.illinois.ncsa.daffodil.io

import org.junit.Test
import org.junit.Assert._
import edu.illinois.ncsa.daffodil.util.Misc
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import edu.illinois.ncsa.daffodil.util.Bits
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder

class TestByteBufferDataInputStream5 {

  @Test def testLittleEndianLSBFirstLong1() {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val expected = 0x0102030405060708L
    fb.put(expected)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    dis.setBitOrder(BitOrder.LeastSignificantBitFirst)
    val md = dis.getSignedLong(64)
    assertTrue(md.isDefined)
    assertEquals(expected, md.get)
    assertEquals(64, dis.bitPos0b)
  }

  @Test def testLittleEndianLSBFirstLong2() {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val expected = 0x01020304L
    fb.put(expected)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    dis.setBitOrder(BitOrder.LeastSignificantBitFirst)
    val md = dis.getSignedLong(32)
    assertTrue(md.isDefined)
    assertEquals(expected, md.get)
    assertEquals(32, dis.bitPos0b)
  }

  @Test def testLittleEndianLSBFirstLong3() {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val expected = 0x8070605040302010L // which is negative. Test is that we get sign right
    fb.put(expected)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    dis.setBitOrder(BitOrder.LeastSignificantBitFirst)
    val md = dis.getSignedLong(64)
    assertTrue(md.isDefined)
    assertEquals(expected, md.get)
    assertEquals(64, dis.bitPos0b)
  }

  @Test def testLittleEndianLong4() {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val data = 0x0000000080706050L // 50 is most significant byte. sign bit is 0.
    fb.put(data)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    dis.setBitOrder(BitOrder.LeastSignificantBitFirst)
    val md = dis.getSignedLong(32)
    assertTrue(md.isDefined)
    assertEquals(0x80706050.toInt, md.get)
    assertEquals(32, dis.bitPos0b)
  }

  @Test def testLittleEndianLong5() {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.BIG_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val data = 0x0100000000000000L
    fb.put(data)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    dis.setBitOrder(BitOrder.LeastSignificantBitFirst)
    val md = dis.getSignedLong(1)
    assertTrue(md.isDefined)
    assertEquals(1, md.get)
    assertEquals(1, dis.bitPos0b)
  }
}