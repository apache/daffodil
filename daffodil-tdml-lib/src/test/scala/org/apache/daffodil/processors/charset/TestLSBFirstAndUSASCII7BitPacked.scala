/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.processors.charset

import java.nio.CharBuffer

import org.junit.Assert.assertEquals

import org.junit.Test
import org.apache.daffodil.util.Misc
import org.apache.daffodil.tdml.Document
import org.apache.daffodil.io.FormatInfoForUnitTest
import org.apache.daffodil.io.InputSourceDataInputStream
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.schema.annotation.props.gen.ByteOrder

class TestLSBFirstAndUSASCII7BitPacked {

  val lsbfFinfo = FormatInfoForUnitTest()
  lsbfFinfo.byteOrder = ByteOrder.BigEndian
  lsbfFinfo.bitOrder = BitOrder.LeastSignificantBitFirst

  @Test def testOne7Bit(): Unit = {
    val dec = BitsCharsetUSASCII7BitPacked.newDecoder()
    val doc = new Document(
      <document>
        <documentPart type="bits" bitOrder="LSBFirst">
          0110100
        </documentPart>
      </document>, null)
    val bytes = doc.documentBytes
    val in = java.nio.ByteBuffer.wrap(bytes)
    val dis = InputSourceDataInputStream(in)
    val cb = CharBuffer.allocate(10)
    dec.decode(dis, lsbfFinfo, cb)
    val outstr = cb.flip.toString
    assertEquals("4", outstr)
  }

  @Test def test7Bit42(): Unit = {
    val dec = BitsCharsetUSASCII7BitPacked.newDecoder()
    val doc = new Document(
      <document>
        <documentPart type="bits" bitOrder="LSBFirst" byteOrder="RTL">
          00 0110010 0110100
        </documentPart>
      </document>, null)
    val bytes = doc.documentBytes
    val in = java.nio.ByteBuffer.wrap(bytes)
    val dis = InputSourceDataInputStream(in)
    val cb = CharBuffer.allocate(10)
    dec.decode(dis, lsbfFinfo, cb)
    val outstr = cb.flip.toString
    assertEquals("42", outstr)
  }

  @Test def test7Bit1234567890(): Unit = {
    val dec = BitsCharsetUSASCII7BitPacked.newDecoder()
    val doc = new Document(
      <document>
        <documentPart type="bits" bitOrder="LSBFirst" byteOrder="RTL"><![CDATA[
      0110000 0111001 0111000 0110111 0110110 0110101 0110100 0110011 0110010 0110001
    ]]></documentPart>
      </document>, null)
    val bytes = doc.documentBytes
    val in = java.nio.ByteBuffer.wrap(bytes)
    val dis = InputSourceDataInputStream(in)
    val cb = CharBuffer.allocate(10)
    dec.decode(dis, lsbfFinfo, cb)
    val outstr = cb.flip.toString
    assertEquals("1234567890", outstr)
  }

  @Test def testEncode7Bit12345678(): Unit = {
    val doc = new Document(
      <document>
        <documentPart type="text" encoding="X-DFDL-US-ASCII-7-BIT-PACKED" bitOrder="LSBFirst"><![CDATA[12345678]]></documentPart>
      </document>, null)
    val hex = Misc.bytes2Hex(doc.documentBytes)
    assertEquals("31D98C56B3DD70", hex)
  }

  @Test def testEncode7Bit123456789(): Unit = {
    val doc = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="text" encoding="X-DFDL-US-ASCII-7-BIT-PACKED"><![CDATA[123456789]]></documentPart>
      </document>, null)
    val hex = Misc.bytes2Hex(doc.documentBytes)
    assertEquals("31D98C56B3DD7039", hex)
  }

  @Test def testEncode7Bit1234567899(): Unit = {
    val doc = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="text" encoding="X-DFDL-US-ASCII-7-BIT-PACKED" bitOrder="LSBFirst"><![CDATA[1234567899]]></documentPart>
      </document>, null)
    val hex = Misc.bytes2Hex(doc.documentBytes)
    assertEquals("31D98C56B3DD70B91C", hex)
  }

  @Test def testEncode7Bit1234567899RTL(): Unit = {
    val doc = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="bits" bitOrder="LSBFirst" byteOrder="RTL"><![CDATA[
            011100|1 0111001 |0111000 0|110111 01|10110 011|0101 0110|100 01100|11 011001|0 0110001
      ]]></documentPart>
      </document>, null)
    val hex = Misc.bytes2Hex(doc.documentBytes)
    assertEquals("31D98C56B3DD70B91C", hex)
  }

  @Test def testEncode7Bit1234567899RTLHex(): Unit = {
    val doc = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="byte" bitOrder="LSBFirst" byteOrder="RTL"><![CDATA[
            1C B9 | 70 DD B3 56 8C D9 31
      ]]></documentPart>
      </document>, null)
    val hex = Misc.bytes2Hex(doc.documentBytes)
    assertEquals("31D98C56B3DD70B91C", hex)
  }

  @Test def test7Bit42LSBFirst(): Unit = {
    val dec = BitsCharsetUSASCII7BitPacked.newDecoder()
    val doc = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="bits" bitOrder="LSBFirst">
          0	0110100 011001
        </documentPart>
      </document>, null)
    val bytes = doc.documentBytes
    val in = java.nio.ByteBuffer.wrap(bytes)
    val dis = InputSourceDataInputStream(in)
    val cb = CharBuffer.allocate(10)
    dec.decode(dis, lsbfFinfo, cb)
    val outstr = cb.flip.toString
    assertEquals("42", outstr)
  }

  @Test def test7Bit1234567890LSBFirst(): Unit = {
    val dec = BitsCharsetUSASCII7BitPacked.newDecoder()
    val doc = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="bits" bitOrder="LSBFirst"><![CDATA[
       00110001 11011001 10001100 01010110  10110011 11011101 01110000 00111001 011000
    ]]></documentPart>
      </document>, null)
    val bytes = doc.documentBytes
    val in = java.nio.ByteBuffer.wrap(bytes)
    val dis = InputSourceDataInputStream(in)
    val cb = CharBuffer.allocate(10)
    dec.decode(dis, lsbfFinfo, cb)
    val outstr = cb.flip.toString
    assertEquals("1234567890", outstr)
  }

  @Test def testEncode7Bit12345678LSBFirst(): Unit = {
    val doc = new Document(
      <document>
        <documentPart type="text" encoding="X-DFDL-US-ASCII-7-BIT-PACKED" bitOrder="LSBFirst"><![CDATA[12345678]]></documentPart>
      </document>, null)
    val hex = Misc.bytes2Hex(doc.documentBytes)
    assertEquals("31D98C56B3DD70", hex)
  }

  @Test def testEncode7Bit123456789LSBFirst(): Unit = {
    val doc = new Document(
      <document>
        <documentPart type="text" encoding="X-DFDL-US-ASCII-7-BIT-PACKED" bitOrder="LSBFirst"><![CDATA[123456789]]></documentPart>
      </document>, null)
    val hex = Misc.bytes2Hex(doc.documentBytes)
    assertEquals("31D98C56B3DD7039", hex)
  }

  @Test def testMix1(): Unit = {
    val doc = new Document(
      <document>
        <documentPart type="bits" bitOrder="LSBFirst" byteOrder="RTL"><![CDATA[110]]></documentPart>
        <documentPart type="text" encoding="X-DFDL-US-ASCII-7-BIT-PACKED" bitOrder="LSBFirst"><![CDATA[123]]></documentPart>
        <documentPart type="bits" bitOrder="LSBFirst" byteOrder="RTL"><![CDATA[1011]]></documentPart>
      </document>, null)
    val doc1bits = doc.documentBits.mkString
    doc1bits.length
    val doc2 = new Document(
      <document>
        <documentPart type="bits" bitOrder="LSBFirst" byteOrder="RTL"><![CDATA[
            1011 0110011 0110010 0110001 110
         ]]></documentPart>
      </document>, null)
    val doc2bits = doc2.documentBits.mkString
    assertEquals(doc2bits, doc1bits)
  }

  @Test def testMIL2045_47001D_Page70_TableB_I(): Unit = {
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
    doc1bits.length
    val doc2 = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="byte"><![CDATA[
            E3 67 00 80 55 67 92 1A FC 77 00 00 00
         ]]></documentPart>
      </document>, null)
    val doc2bits = doc2.documentBits
    assertEquals(doc2bits, doc1bits)
  }

  @Test def testMIL2045_47001D_Page70_TableB_I_with_string(): Unit = {
    val doc = new Document(
      <document bitOrder="LSBFirst">
        <documentPart type="bits" byteOrder="RTL">Version                         XXXX 0011</documentPart>
        <documentPart type="bits" byteOrder="RTL">FPI                             XXX0 XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">Compression                     NA       </documentPart>
        <documentPart type="bits" byteOrder="RTL">GPI for Originator Address      XX1X XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">FPI for URN                     X1XX XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">URN  X0000000 00000000 01100111 1XXX XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">FPI for Unit Name               1XXX XXXX</documentPart>
        <documentPart type="text" encoding="X-DFDL-US-ASCII-7-BIT-PACKED">UNITA&#x7F;</documentPart>
        <documentPart type="bits" byteOrder="RTL">GPI for Recip. Addr Group       XXXX X1XX</documentPart>
        <documentPart type="bits" byteOrder="RTL">GRI for R_ONE                   XXXX 0XXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">FPI for URN                     XXX1 XXXX</documentPart>
        <documentPart type="bits" byteOrder="RTL">URN XXXX00000 00000000 00000000 011X XXXX</documentPart>
      </document>, null)
    val doc1bits = doc.documentBits
    doc1bits.length
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
