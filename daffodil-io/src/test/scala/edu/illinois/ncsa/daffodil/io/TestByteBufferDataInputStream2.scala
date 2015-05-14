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
import edu.illinois.ncsa.daffodil.exceptions.Abort

class TestByteBufferDataInputStream2 {
  val tenDigits = "1234567890"
  val ten = tenDigits.getBytes("utf-8")
  val twentyDigits = tenDigits * 2
  val twenty = twentyDigits.getBytes("utf-8")

  @Test def testMark1 {
    val dis = ByteBufferDataInputStream(ten)
    var bb = ByteBuffer.allocate(10)
    val m1 = dis.mark
    var n = dis.fillByteBuffer(bb)
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
    dis.reset(m1)
    bb = ByteBuffer.allocate(10)
    n = dis.fillByteBuffer(bb)
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

  @Test def testMark2 {
    val dis = ByteBufferDataInputStream(twenty)
    var bb = ByteBuffer.allocate(20)
    dis.setBitLimit0b(One(5 * 8))
    var n = dis.fillByteBuffer(bb)
    assertTrue(n.isDefined)
    assertEquals(5, n.get)
    val m1 = dis.mark
    assertFalse(dis.asIteratorChar.hasNext)
    dis.setBitLimit0b(One(10 * 8))
    n = dis.fillByteBuffer(bb)
    val m2 = dis.mark
    assertEquals(One(5), n)
    bb.flip()
    1 to 9 foreach { _ => bb.get() }
    assertEquals(0x30.toByte, bb.get())
    assertEquals(80, dis.bitPos0b)
    assertEquals(80, dis.bitLimit0b.get)
    assertEquals(81, dis.bitPos1b)
    assertEquals(81, dis.bitLimit1b.get)
    assertEquals(10, dis.bytePos0b)
    dis.reset(m1)
    bb = ByteBuffer.allocate(10)
    n = dis.fillByteBuffer(bb)
    assertFalse(n.isDefined)
    dis.setBitLimit0b(One(10 * 8))
    n = dis.fillByteBuffer(bb)
    assertEquals(5, n.get)
    bb.flip()
    1 to 4 foreach { _ => bb.get() }
    assertEquals(0x30.toByte, bb.get())
    assertEquals(80, dis.bitPos0b)
    assertEquals(80, dis.bitLimit0b.get)
    assertEquals(81, dis.bitPos1b)
    assertEquals(81, dis.bitLimit1b.get)
    assertEquals(10, dis.bytePos0b)
  }

  @Test def testMark3 {
    val dis = ByteBufferDataInputStream(twenty)
    var bb = ByteBuffer.allocate(20)
    dis.setBitLimit0b(One(5 * 8))
    var n = dis.fillByteBuffer(bb)
    assertTrue(n.isDefined)
    assertEquals(5, n.get)
    val m1 = dis.mark
    assertFalse(dis.asIteratorChar.hasNext)
    dis.setBitLimit0b(One(10 * 8))
    n = dis.fillByteBuffer(bb)
    val m2 = dis.mark
    assertEquals(One(5), n)
    bb.flip()
    1 to 9 foreach { _ => bb.get() }
    assertEquals(0x30.toByte, bb.get())
    assertEquals(80, dis.bitPos0b)
    assertEquals(80, dis.bitLimit0b.get)
    assertEquals(81, dis.bitPos1b)
    assertEquals(81, dis.bitLimit1b.get)
    assertEquals(10, dis.bytePos0b)
    dis.reset(m1)
    val e = intercept[Abort] {
      dis.reset(m2)
    }
  }

}