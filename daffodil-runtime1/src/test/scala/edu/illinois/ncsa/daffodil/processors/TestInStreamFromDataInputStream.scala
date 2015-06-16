package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.io.ByteBufferDataInputStream
import org.junit.Test
import org.junit.Assert._
import scala.Array.canBuildFrom
import edu.illinois.ncsa.daffodil.Implicits._

//class TestInStream {
//
//  @deprecated("2015-06-23", "Delete when DFDLCharIterator is replaced by direct calls to DataInputStream")
//  @Test def testCharIteratorAdvancesWhenCharsAreProducedNotBeforeThen {
//    val badByte = List(0xFF.toByte).toArray
//    val data = "年月".getBytes("utf-8") ++ badByte ++ "1".getBytes("utf-8")
//    val dis = ByteBufferDataInputStream(data)
//    val is = InStream(null, dis)
//    var rdr = DFDLCharReaderFromDataInputStream(is)
//    assertEquals(0, rdr.bitPos0b)
//    var c = rdr.first
//    assertEquals('年', c)
//    assertEquals(24, rdr.bitPos0b)
//    rdr = rdr.rest
//    assertEquals(24, rdr.bitPos0b)
//    c = rdr.first
//    assertEquals('月', c)
//    assertEquals(48, rdr.bitPos0b)
//    rdr = rdr.rest
//    assertEquals(48, rdr.bitPos0b)
//    c = rdr.first
//    assertEquals('\uFFFD', c)
//    assertEquals(56, rdr.bitPos0b)
//    rdr = rdr.rest
//    assertEquals(56, rdr.bitPos0b)
//    c = rdr.first
//    assertEquals(64, rdr.bitPos0b)
//    assertEquals('1', c)
//    assertEquals(64, rdr.bitPos0b)
//    rdr = rdr.rest
//    assertEquals(64, rdr.bitPos0b)
//    c = rdr.first
//    assertEquals(-1.toChar, c)
//    assertEquals(64, rdr.bitPos0b)
//    assertTrue(rdr.atEnd)
//    intercept[NoSuchElementException] {
//      rdr.rest
//    }
//  }
//}