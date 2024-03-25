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

package org.apache.daffodil.tdml.scalaxb

import org.junit.Assert._
import org.junit.Test

class ScalaxbTests {

  @Test def testReadingNoTests(): Unit = {
    val testSuite =
      scalaxb.fromXML[TestSuite](
        scala.xml.XML.load(
          getClass
            .getClassLoader()
            .getResourceAsStream("test-suite/ibm-contributed/dpaext1-2.tdml"),
        ),
      )

    assertNotNull(testSuite)
    assertEquals(Some("dpaext"), testSuite.suiteName)
  }

  @Test def testReading(): Unit = {
    val testSuite =
      scalaxb.fromXML[TestSuite](
        scala.xml.Utility.trim(
          scala.xml.XML.load(
            getClass
              .getClassLoader()
              .getResourceAsStream("test-suite/IPv4.tdml"),
          ),
        ),
      )

    assertNotNull(testSuite)
    assertEquals(None, testSuite.suiteName)

    val expectedParserTestCase =
      scalaxb.fromXML[ParserTestCaseType](
        scala.xml.Utility.trim(
          <tdml:parserTestCase name="IPv4_1" root="IPv4Header" model="org/apache/daffodil/layers/IPv4.dfdl.xsd"
    roundTrip="none"
    xmlns:tns="http://example.com"
    xmlns:ex="http://example.com"
    xmlns:chksum="urn:org.apache.daffodil.layers.IPv4Checksum"
    xmlns:ipv4="urn:org.apache.daffodil.layers.IPv4"
    xmlns:dfdlx="http://www.ogf.org/dfdl/dfdl-1.0/extensions"
    xmlns:fn="http://www.w3.org/2005/xpath-functions"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:tdml="http://www.ibm.com/xmlns/dfdl/testData"
  >
          <tdml:document>
            {
            scala.xml.Comment("""
      see https://en.wikipedia.org/wiki/IPv4_header_checksum
      for working through the details of this IPv4 Header

      The first line of 5 words is the part 1 of the header
      The second line is the checksum
      The third line of 4 words is the part 2 of the header (src, dest addresses)
      """)
          }
      <tdml:documentPart type="byte">{
            scala.xml.PCData("\n4500 0073 0000 4000 4011\nb861\nc0a8 0001 c0a8 00c7\n       ")
          }</tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <ipv4:IPv4Header>
                <Version>4</Version>
                <IHL>5</IHL>
                <DSCP>0</DSCP>
                <ECN>0</ECN>
                <Length>115</Length>
                <Identification>0</Identification>
                <Flags>2</Flags>
                <FragmentOffset>0</FragmentOffset>
                <TTL>64</TTL>
                <Protocol>17</Protocol>
                <Checksum>47201</Checksum>
                <IPSrc>C0A80001</IPSrc>
                <IPDest>C0A800C7</IPDest>
                <FakeData>115</FakeData>
                <ComputedChecksum>47201</ComputedChecksum>
              </ipv4:IPv4Header>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>,
        ),
      )
    val actualParserTestCase: ParserTestCaseType =
      testSuite.testSuiteChoices.headOption
        .map(_.as[ParserTestCaseType])
        .getOrElse(
          throw new AssertionError("first test case is missing"),
        ) // fail() returns Unit so mimic JUnit

    assertEquals(expectedParserTestCase, actualParserTestCase)
  }
}
