package edu.illinois.ncsa.daffodil.tdml

import org.junit.Test

import junit.framework.Assert.assertEquals

class TestMoreEncodings {

  @Test def testBitsEncoding1() {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-BITS-LSBF">1</documentPart>
              </document>
    val doc = new Document(xml, null)
    val actual = doc.documentBytes.toList
    val expected = List(0x01).map { _.toByte }
    assertEquals(expected, actual)
    assertEquals(1, doc.nBits)
  }

  @Test def testBitsEncoding2() {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-BITS-LSBF">10</documentPart>
              </document>
    val doc = new Document(xml, null)
    val actual = doc.documentBytes.toList
    val expected = List(0x01).map { _.toByte }
    assertEquals(expected, actual)
    assertEquals(2, doc.nBits)
  }

  @Test def testBitsEncoding8() {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-BITS-LSBF">00110101</documentPart>
              </document>
    val doc = new Document(xml, null)
    val actual = doc.documentBytes.toList
    val expected = List(0xAC).map { _.toByte }
    assertEquals(expected, actual)
    assertEquals(8, doc.nBits)
  }

  @Test def testBitsEncoding9() {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-BITS-LSBF">001101011</documentPart>
              </document>
    val doc = new Document(xml, null)
    val actual = doc.documentBytes.toList
    val expected = List(0xAC, 0x01).map { _.toByte }
    assertEquals(expected, actual)
    assertEquals(9, doc.nBits)
  }

  @Test def testSixBitEncoding1() {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-6-BIT-DFI-264-DUI-001"><![CDATA[0]]></documentPart>
              </document>
    val doc = new Document(xml, null)
    val actual = doc.documentBytes.toList
    val expected = List(0x3F).map { _.toByte }
    assertEquals(expected, actual)
    assertEquals(6, doc.nBits)
  }

  @Test def testSixBitEncoding2() {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-6-BIT-DFI-264-DUI-001"><![CDATA[00]]></documentPart>
              </document>
    val doc = new Document(xml, null)
    val actual = doc.documentBytes.toList
    val expected = List(0xFF, 0x0F).map { _.toByte }
    assertEquals(expected, actual)
    assertEquals(12, doc.nBits)
  }

  @Test def testSixBitEncoding40() {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-6-BIT-DFI-264-DUI-001"><![CDATA[0123456789 ABCDEFGHIJKLMNOPQRSTUVWXYZ012]]></documentPart>
              </document>
    val doc = new Document(xml, null)
    val actual = doc.documentBytes.toList
    assertEquals(30, actual.length)
  }

  @Test def testSixBitEncoding39() {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-6-BIT-DFI-264-DUI-001"><![CDATA[0123456789 ABCDEFGHIJKLMNOPQRSTUVWXYZ00]]></documentPart>
              </document>
    val doc = new Document(xml, null)
    val actual = doc.documentBytes.toList
    assertEquals(30, actual.length)
    val lastByte = actual.last
    assertEquals(0x03, lastByte)
  }
}
