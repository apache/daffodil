package edu.illinois.ncsa.daffodil.processors.charset

import org.scalatest.junit.JUnitSuite
import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import org.junit.Test
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.tdml.Document

class TestUSASCII7BitPacked extends JUnitSuite {

  @Test def testOne7Bit() {
    val dec = USASCII7BitPackedCharset.newDecoder()
    val doc = new Document(<document><documentPart type="bits">0110100</documentPart></document>, null)
    val bytes = doc.documentBytes
    val in = java.nio.ByteBuffer.wrap(bytes)
    val out = dec.decode(in)
    val outstr = out.toString
    // println(outstr)
    assertEquals("4", outstr)
  }

  @Test def test7Bit42() {
    val dec = USASCII7BitPackedCharset.newDecoder()
    val doc = new Document(<document><documentPart type="bits">0110100 0 110010 00</documentPart></document>, null)
    val bytes = doc.documentBytes
    val in = java.nio.ByteBuffer.wrap(bytes)
    val out = dec.decode(in)
    val outstr = out.toString
    // println(outstr)
    assertEquals("42", outstr)
  }

  @Test def test7Bit1234567890() {
    val dec = USASCII7BitPackedCharset.newDecoder()
    val doc = new Document(<document><documentPart type="bits"><![CDATA[
       0110001 0110010 0110011 0110100 0110101 0110110 0110111 0111000 0111001 0110000
    ]]></documentPart></document>, null)
    val bytes = doc.documentBytes
    val in = java.nio.ByteBuffer.wrap(bytes)
    val out = dec.decode(in)
    val outstr = out.toString
    // println(outstr)
    assertEquals("1234567890", outstr)
  }

}