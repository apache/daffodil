/* Copyright (c) 2017 Tresys Technology, LLC. All rights reserved.
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
