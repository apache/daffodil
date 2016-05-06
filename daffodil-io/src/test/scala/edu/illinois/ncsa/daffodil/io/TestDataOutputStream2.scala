package edu.illinois.ncsa.daffodil.io

import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.ByteOrder
import org.junit.Test
import java.nio.ByteBuffer

class TestDataOutputStream2 {

  @Test def testPutBitsDirect0_BE_MSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = DirectOrBufferedDataOutputStream(baos)

    out.setBitOrder(BitOrder.MostSignificantBitFirst)
    out.setByteOrder(ByteOrder.BigEndian)

    out.putBits(List(0xA5.toByte, 0xBE.toByte, 0xEF.toByte).toArray, 1, 16)

    out.setFinished()

    val buf = baos.getBuf()

    assertEquals(0xBE.toByte, buf(0))
    assertEquals(0xEF.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  @Test def testPutBitsDirect1_BE_MSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = DirectOrBufferedDataOutputStream(baos)

    out.setBitOrder(BitOrder.MostSignificantBitFirst)
    out.setByteOrder(ByteOrder.BigEndian)

    out.putBits(List(0xA5.toByte, 0xBE.toByte, 0xEF.toByte).toArray, 1, 9)

    out.setFinished()

    val buf = baos.getBuf()

    assertEquals(0xBE.toByte, buf(0))
    assertEquals(0x80.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  @Test def testPutBitsDirect7_BE_MSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = DirectOrBufferedDataOutputStream(baos)

    out.setBitOrder(BitOrder.MostSignificantBitFirst)
    out.setByteOrder(ByteOrder.BigEndian)

    out.putBits(List(0xA5.toByte, 0xBE.toByte, 0xEF.toByte).toArray, 1, 15)

    out.setFinished()

    val buf = baos.getBuf()

    assertEquals(0xBE.toByte, buf(0))
    assertEquals(0xEE.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  /*
   * BitBuffer tests
   */

  @Test def testPutBitBufferDirect0_BE_MSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = DirectOrBufferedDataOutputStream(baos)

    out.setBitOrder(BitOrder.MostSignificantBitFirst)
    out.setByteOrder(ByteOrder.BigEndian)

    val bb = ByteBuffer.wrap(List(0xA5.toByte, 0xBE.toByte, 0xEF.toByte).toArray)
    bb.position(1)
    out.putBitBuffer(bb, 16)

    out.setFinished()

    val buf = baos.getBuf()

    assertEquals(0xBE.toByte, buf(0))
    assertEquals(0xEF.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  @Test def testPutBitBufferDirect1_BE_MSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = DirectOrBufferedDataOutputStream(baos)

    out.setBitOrder(BitOrder.MostSignificantBitFirst)
    out.setByteOrder(ByteOrder.BigEndian)

    val bb = ByteBuffer.wrap(List(0xA5.toByte, 0xBE.toByte, 0xEF.toByte).toArray)
    bb.position(1)
    out.putBitBuffer(bb, 9)

    out.setFinished()

    val buf = baos.getBuf()

    assertEquals(0xBE.toByte, buf(0))
    assertEquals(0x80.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }

  @Test def testPutBitBufferDirect7_BE_MSBF {

    val baos = new ByteArrayOutputStreamWithGetBuf()
    val out = DirectOrBufferedDataOutputStream(baos)

    out.setBitOrder(BitOrder.MostSignificantBitFirst)
    out.setByteOrder(ByteOrder.BigEndian)

    val bb = ByteBuffer.wrap(List(0xA5.toByte, 0xBE.toByte, 0xEF.toByte).toArray)
    bb.position(1)
    out.putBitBuffer(bb, 15)

    out.setFinished()

    val buf = baos.getBuf()

    assertEquals(0xBE.toByte, buf(0))
    assertEquals(0xEE.toByte, buf(1))
    if (buf.length > 2)
      assertEquals(0, buf(2))

  }
}
