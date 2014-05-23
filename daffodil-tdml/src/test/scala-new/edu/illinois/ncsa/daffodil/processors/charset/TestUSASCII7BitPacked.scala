package edu.illinois.ncsa.daffodil.processors.charset

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import junit.framework.Assert.assertEquals
import junit.framework.Assert.assertTrue
import org.junit.Test
import edu.illinois.ncsa.daffodil.debugger.Debugger
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.tdml.Document

class TestUSASCII7BitPacked {

  @Test def testOne7Bit() {
    val dec = USASCII7BitPackedCharset.newDecoder()
    val doc = new Document(
      <document>
        <documentPart type="bits" bitOrder="LSBFirst">
          0110100
        </documentPart>
      </document>, null)
    val bytes = doc.documentBytes
    val in = java.nio.ByteBuffer.wrap(bytes)
    val out = dec.decode(in)
    val outstr = out.toString
    assertEquals("4", outstr)
  }

  @Test def test7Bit42() {
    val dec = USASCII7BitPackedCharset.newDecoder()
    val doc = new Document(
      <document>
        <documentPart type="bits" bitOrder="LSBFirst" byteOrder="RTL">
          00 0110010 0110100
        </documentPart>
      </document>, null)
    val bytes = doc.documentBytes
    val in = java.nio.ByteBuffer.wrap(bytes)
    val out = dec.decode(in)
    val outstr = out.toString
    assertEquals("42", outstr)
  }

  @Test def test7Bit1234567890() {
    val dec = USASCII7BitPackedCharset.newDecoder()
    val doc = new Document(
      <document>
        <documentPart type="bits" bitOrder="LSBFirst" byteOrder="RTL"><![CDATA[
      0110000 0111001 0111000 0110111 0110110 0110101 0110100 0110011 0110010 0110001
    ]]></documentPart>
      </document>, null)
    val bytes = doc.documentBytes
    val in = java.nio.ByteBuffer.wrap(bytes)
    val out = dec.decode(in)
    val outstr = out.toString
    assertEquals("1234567890", outstr)
  }

  @Test def testEncode7Bit12345678() {
    val enc = USASCII7BitPackedCharset.newEncoder()
    val doc = new Document(
      <document>
        <documentPart type="text" encoding="us-ascii-7-bit-packed" bitOrder="LSBFirst"><![CDATA[12345678]]></documentPart>
      </document>, null)
    val hex = Misc.bytes2Hex(doc.documentBytes)
    assertEquals("0x31D98C56B3DD70", hex)
  }

  @Test def testEncode7Bit123456789() {
    val enc = USASCII7BitPackedCharset.newEncoder()
    val doc = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="text" encoding="us-ascii-7-bit-packed"><![CDATA[123456789]]></documentPart>
      </document>, null)
    val hex = Misc.bytes2Hex(doc.documentBytes)
    assertEquals("0x31D98C56B3DD7039", hex)
  }

  @Test def testEncode7Bit1234567899() {
    val enc = USASCII7BitPackedCharset.newEncoder()
    val doc = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="text" encoding="us-ascii-7-bit-packed" bitOrder="LSBFirst"><![CDATA[1234567899]]></documentPart>
      </document>, null)
    val hex = Misc.bytes2Hex(doc.documentBytes)
    assertEquals("0x31D98C56B3DD70B91C", hex)
  }

  @Test def testEncode7Bit1234567899RTL() {
    val enc = USASCII7BitPackedCharset.newEncoder()
    val doc = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="bits" bitOrder="LSBFirst" byteOrder="RTL"><![CDATA[
            011100|1 0111001 |0111000 0|110111 01|10110 011|0101 0110|100 01100|11 011001|0 0110001
      ]]></documentPart>
      </document>, null)
    val hex = Misc.bytes2Hex(doc.documentBytes)
    assertEquals("0x31D98C56B3DD70B91C", hex)
  }

  @Test def testEncode7Bit1234567899RTLHex() {
    val enc = USASCII7BitPackedCharset.newEncoder()
    val doc = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="byte" bitOrder="LSBFirst" byteOrder="RTL"><![CDATA[
            1C B9 | 70 DD B3 56 8C D9 31
      ]]></documentPart>
      </document>, null)
    val hex = Misc.bytes2Hex(doc.documentBytes)
    assertEquals("0x31D98C56B3DD70B91C", hex)
  }

  @Test def test7Bit42LSBFirst() {
    val dec = USASCII7BitPackedCharset.newDecoder()
    val doc = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="bits" bitOrder="LSBFirst">
          0	0110100 011001
        </documentPart>
      </document>, null)
    val bytes = doc.documentBytes
    val in = java.nio.ByteBuffer.wrap(bytes)
    val out = dec.decode(in)
    val outstr = out.toString
    assertEquals("42", outstr)
  }

  @Test def test7Bit1234567890LSBFirst() {
    val dec = USASCII7BitPackedCharset.newDecoder()
    val doc = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="bits" bitOrder="LSBFirst"><![CDATA[
       00110001 11011001 10001100 01010110  10110011 11011101 01110000 00111001 011000
    ]]></documentPart>
      </document>, null)
    val bytes = doc.documentBytes
    val in = java.nio.ByteBuffer.wrap(bytes)
    val out = dec.decode(in)
    val outstr = out.toString
    assertEquals("1234567890", outstr)
  }

  @Test def testEncode7Bit12345678LSBFirst() {
    val enc = USASCII7BitPackedCharset.newEncoder()
    val doc = new Document(
      <document>
        <documentPart type="text" encoding="us-ascii-7-bit-packed" bitOrder="LSBFirst"><![CDATA[12345678]]></documentPart>
      </document>, null)
    val hex = Misc.bytes2Hex(doc.documentBytes)
    assertEquals("0x31D98C56B3DD70", hex)
  }

  @Test def testEncode7Bit123456789LSBFirst() {
    val enc = USASCII7BitPackedCharset.newEncoder()
    val doc = new Document(
      <document>
        <documentPart type="text" encoding="us-ascii-7-bit-packed" bitOrder="LSBFirst"><![CDATA[123456789]]></documentPart>
      </document>, null)
    val hex = Misc.bytes2Hex(doc.documentBytes)
    assertEquals("0x31D98C56B3DD7039", hex)
  }

  @Test def testMix1() {
    val doc = new Document(
      <document>
        <documentPart type="bits" bitOrder="LSBFirst" byteOrder="RTL"><![CDATA[110]]></documentPart>
        <documentPart type="text" encoding="us-ascii-7-bit-packed" bitOrder="LSBFirst"><![CDATA[123]]></documentPart>
        <documentPart type="bits" bitOrder="LSBFirst" byteOrder="RTL"><![CDATA[1011]]></documentPart>
      </document>, null)
    val doc1bits = doc.documentBits.mkString
    val lengthInBits = doc1bits.length
    val doc2 = new Document(
      <document>
        <documentPart type="bits" bitOrder="LSBFirst" byteOrder="RTL"><![CDATA[
            1011 0110011 0110010 0110001 110
         ]]></documentPart>
      </document>, null)
    val doc2bits = doc2.documentBits.mkString
    assertEquals(doc2bits, doc1bits)
  }

  @Test def testMIL2045_47001D_Page70_TableB_I() {
    val doc = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="bits" byteOrder="RTL">Version                         XXXX 0011</documentPart>
        <documentPart type="bits" byteOrder="RTL">FPI                             XXX0 XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">Compression                     NA       </documentPart>
        <documentPart type="bits" byteOrder="RTL">GPI for Originator Address      XX1X XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">FPI for URN                     X1XX XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">URN  X0000000 00000000 01100111 1XXX XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">FPI for Unit Name               1XXX XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">Unit Name                       X101 0101</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                0XXX XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                XX10 0111</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                01XX XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                XXX1 0010</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                100X XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                XXXX 1010</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                0001 XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                XXXX X100</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                1111 1XXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">                                XXXX XX11</documentPart>
        <documentPart type="bits" byteOrder="RTL">GPI for Recip. Addr Group       XXXX X1XX</documentPart>
        <documentPart type="bits" byteOrder="RTL">GRI for R_ONE                   XXXX 0XXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">FPI for URN                     XXX1 XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">URN XXXX00000 00000000 00000000 011X XXXX</documentPart>
      </document>, null)
    val doc1bits = doc.documentBits
    val lengthInBits = doc1bits.length
    val doc2 = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="byte"><![CDATA[
            E3 67 00 80 55 67 92 1A FC 77 00 00 00
         ]]></documentPart>
      </document>, null)
    val doc2bits = doc2.documentBits
    assertEquals(doc2bits, doc1bits)
  }

  @Test def testMIL2045_47001D_Page70_TableB_I_with_string() {
    val doc = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="bits" byteOrder="RTL">Version                         XXXX 0011</documentPart>
        <documentPart type="bits" byteOrder="RTL">FPI                             XXX0 XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">Compression                     NA       </documentPart>
        <documentPart type="bits" byteOrder="RTL">GPI for Originator Address      XX1X XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">FPI for URN                     X1XX XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">URN  X0000000 00000000 01100111 1XXX XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">FPI for Unit Name               1XXX XXXX</documentPart>
        <documentPart type="text" encoding="us-ascii-7-bit-packed">UNITA&#x7F;</documentPart>
        <documentPart type="bits" byteOrder="RTL">GPI for Recip. Addr Group       XXXX X1XX</documentPart>
        <documentPart type="bits" byteOrder="RTL">GRI for R_ONE                   XXXX 0XXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">FPI for URN                     XXX1 XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">URN XXXX00000 00000000 00000000 011X XXXX</documentPart>
      </document>, null)
    val doc1bits = doc.documentBits
    val lengthInBits = doc1bits.length
    val doc2 = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="byte"><![CDATA[
            E3 67 00 80 55 67 92 1A FC 77 00 00 00
         ]]></documentPart>
      </document>, null)
    val doc2bits = doc2.documentBits
    assertEquals(doc2bits, doc1bits)
  }

}