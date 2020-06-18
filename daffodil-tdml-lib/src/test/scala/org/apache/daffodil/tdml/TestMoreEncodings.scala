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

package org.apache.daffodil.tdml

import org.junit.Test

import org.junit.Assert.assertEquals

class TestMoreEncodings {

  @Test def testBitsEncoding1(): Unit = {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-BITS-LSBF">1</documentPart>
              </document>
    val doc = new Document(xml, null)
    val actual = doc.documentBytes.toList
    val expected = List(0x01).map { _.toByte }
    assertEquals(expected, actual)
    assertEquals(1, doc.nBits)
  }

  @Test def testBitsEncoding2(): Unit = {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-BITS-LSBF">10</documentPart>
              </document>
    val doc = new Document(xml, null)
    val actual = doc.documentBytes.toList
    val expected = List(0x01).map { _.toByte }
    assertEquals(expected, actual)
    assertEquals(2, doc.nBits)
  }

  @Test def testBitsEncoding8(): Unit = {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-BITS-LSBF">00110101</documentPart>
              </document>
    val doc = new Document(xml, null)
    val actual = doc.documentBytes.toList
    val expected = List(0xAC).map { _.toByte }
    assertEquals(expected, actual)
    assertEquals(8, doc.nBits)
  }

  @Test def testBitsEncoding9(): Unit = {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-BITS-LSBF">001101011</documentPart>
              </document>
    val doc = new Document(xml, null)
    val actual = doc.documentBytes.toList
    val expected = List(0xAC, 0x01).map { _.toByte }
    assertEquals(expected, actual)
    assertEquals(9, doc.nBits)
  }

  @Test def testSixBitEncoding1(): Unit = {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-6-BIT-DFI-264-DUI-001"><![CDATA[0]]></documentPart>
              </document>
    val doc = new Document(xml, null)
    val actual = doc.documentBytes.toList
    val expected = List(0x3F).map { _.toByte }
    assertEquals(expected, actual)
    assertEquals(6, doc.nBits)
  }

  @Test def testSixBitEncoding2(): Unit = {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-6-BIT-DFI-264-DUI-001"><![CDATA[00]]></documentPart>
              </document>
    val doc = new Document(xml, null)
    val actual = doc.documentBytes.toList
    val expected = List(0xFF, 0x0F).map { _.toByte }
    assertEquals(expected, actual)
    assertEquals(12, doc.nBits)
  }

  @Test def testSixBitEncoding40(): Unit = {
    val xml = <document>
                <documentPart type="text" bitOrder="LSBFirst" encoding="X-DFDL-6-BIT-DFI-264-DUI-001"><![CDATA[0123456789 ABCDEFGHIJKLMNOPQRSTUVWXYZ012]]></documentPart>
              </document>
    val doc = new Document(xml, null)
    val actual = doc.documentBytes.toList
    assertEquals(30, actual.length)
  }

  @Test def testSixBitEncoding39(): Unit = {
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
