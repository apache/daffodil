package edu.illinois.ncsa.daffodil.io

import org.junit.Test
import org.junit.Assert._
import edu.illinois.ncsa.daffodil.util.Misc
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder

class TestByteBufferDataInputStream4 {

  @Test def testLittleEndianFloat1() {
    val bb = ByteBuffer.allocate(4)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asFloatBuffer()
    fb.position(0)
    fb.put(1.125.toFloat)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    val expected = 1.125.toFloat
    val md = dis.getBinaryFloat()
    assertTrue(md.isDefined)
    assertEquals(expected, md.get, 0.01)
    assertEquals(32, dis.bitPos0b)
  }

  @Test def testLittleEndianDouble1() {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asDoubleBuffer()
    fb.position(0)
    fb.put(1.125)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    val expected = 1.125
    val md = dis.getBinaryDouble()
    assertTrue(md.isDefined)
    assertEquals(expected, md.get, 0.01)
    assertEquals(64, dis.bitPos0b)
  }

  @Test def testLittleEndianLong1() {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val expected = 0x0102030405060708L
    fb.put(expected)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    val md = dis.getSignedLong(64)
    assertTrue(md.isDefined)
    assertEquals(expected, md.get)
    assertEquals(64, dis.bitPos0b)
  }

  @Test def testLittleEndianLong2() {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val expected = 0x01020304L
    fb.put(expected)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    val md = dis.getSignedLong(32)
    assertTrue(md.isDefined)
    assertEquals(expected, md.get)
    assertEquals(32, dis.bitPos0b)
  }

  @Test def testLittleEndianLong3() {
    val bb = ByteBuffer.allocate(8)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    val fb = bb.asLongBuffer()
    fb.position(0)
    val expected = 0x8070605040302010L
    fb.put(expected)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
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
    val data = 0x0000000080706050L
    fb.put(data)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
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
    val data = 0x8000000000000000L
    fb.put(data)
    val bytes = bb.array()
    val dis = ByteBufferDataInputStream(bytes)
    dis.setByteOrder(ByteOrder.LittleEndian)
    val md = dis.getSignedLong(1)
    assertTrue(md.isDefined)
    assertEquals(1, md.get)
    assertEquals(1, dis.bitPos0b)
  }
}