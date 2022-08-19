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

import java.io.File
import org.apache.daffodil.Implicits.using
import org.apache.daffodil.xml.XMLUtils
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.apache.daffodil.util._
import org.junit.Test
import org.apache.daffodil.Implicits._

class TestTDMLRunner {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example
  // val sub = XMLUtils.DFDL_XMLSCHEMASUBSET_NAMESPACE

  @Test def test3(): Unit = {
    // This tests when there are parseTestCases in the same suite that use the
    // same DFDL schema but have different validation modes. The non-validation
    // test should compile the processor, serialize it, and cache it.
    // Validation="on" does not work with serialized parsers, so that
    // parserTestCase should recompile the schema, and cache a non-serialized
    // processor. So the two tests, although they use the same schema, should
    // use different processors.

    val testSuite = <testSuite xmlns={ tdml } suiteName="theSuiteName" xmlns:xs={ xsd } xmlns:dfdl={ dfdl } xmlns:tns={ example }>
                      <defineSchema name="mySchema">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="tns:GeneralFormat"/>
                        <xs:element name="data" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>
                      </defineSchema>
                      <parserTestCase name="test3a" root="data" model="mySchema" validation="off">
                        <document>37</document>
                        <infoset>
                          <dfdlInfoset>
                            <data xmlns={ example }>37</data>
                          </dfdlInfoset>
                        </infoset>
                      </parserTestCase>
                      <parserTestCase name="test3b" root="data" model="mySchema" validation="on">
                        <document>37</document>
                        <infoset>
                          <dfdlInfoset>
                            <data xmlns={ example }>37</data>
                          </dfdlInfoset>
                        </infoset>
                      </parserTestCase>
                    </testSuite>

    val runner = new Runner(testSuite)

    // Run the non-validation test first, this will compile, serialize, and
    // cache the schema
    runner.runOneTest("test3a")

    // Run the validation test second. This should recompile and cache the
    // schema separately from from the first test. If it ended up using the
    // non-validation cached Processor, an exception would be thrown since it
    // was serialized and that does not work with validation="on"
    runner.runOneTest("test3b")

    runner.reset
  }

  @Test def testTDMLParseSuccess(): Unit = {

    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName" xmlns:xs={ xsd } xmlns:dfdl={ dfdl } xmlns:tns={ example }>
                      <ts:defineSchema name="mySchema">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="tns:GeneralFormat"/>
                        <xs:element name="data" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>
                      </ts:defineSchema>
                      <ts:parserTestCase ID="some identifier" name="testTDMLParseSuccess" root="data" model="mySchema">
                        <ts:document>37</ts:document>
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <data xmlns={ example }>37</data>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val runner = new Runner(testSuite)
    runner.runOneTest("testTDMLParseSuccess")
    runner.reset
  }

  @Test def testTDMLParseDetectsErrorWithSpecificMessage(): Unit = {

    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName" xmlns:xs={ xsd } xmlns:dfdl={ dfdl } xmlns:tns={ example }>
                      <ts:defineSchema name="mySchema">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="tns:GeneralFormat"/>
                        <xs:element name="data" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>
                      </ts:defineSchema>
                      <ts:parserTestCase ID="some identifier" name="testTDMLParseDetectsErrorWithSpecificMessage" root="data" model="mySchema">
                        <ts:document>AA</ts:document>
                        <ts:errors>
                          <ts:error>AA</ts:error><!-- can have several substrings of message -->
                          <ts:error>xs:int</ts:error><!-- all are checked against the message -->
                        </ts:errors>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val runner = new Runner(testSuite)
    runner.runOneTest("testTDMLParseDetectsErrorWithSpecificMessage")
    runner.reset
  }

  @Test def testTDMLParseDetectsErrorWithPartMessage(): Unit = {

    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName" xmlns:xs={ xsd } xmlns:dfdl={ dfdl } xmlns:tns={ example }>
                      <ts:defineSchema name="mySchema">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="tns:GeneralFormat"/>
                        <xs:element name="data" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>
                      </ts:defineSchema>
                      <ts:parserTestCase ID="some identifier" name="testTDMLParseDetectsErrorWithPartMessage" root="data" model="mySchema">
                        <ts:document>AA</ts:document>
                        <ts:errors>
                          <ts:error>AA</ts:error>
                          <ts:error>xs:float</ts:error><!-- Detect this mismatch. It will say xs:int -->
                        </ts:errors>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val runner = new Runner(testSuite)
    val exc = intercept[Exception] {
      runner.runOneTest("testTDMLParseDetectsErrorWithPartMessage")
    }
    runner.reset
    assertTrue(exc.getMessage().contains("message \"xs:float\""))
  }

  @Test def testTDMLParseDetectsErrorAnyMessage(): Unit = {

    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName" xmlns:xs={ xsd } xmlns:dfdl={ dfdl } xmlns:tns={ example }>
                      <ts:defineSchema name="mySchema">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="tns:GeneralFormat"/>
                        <xs:element name="data" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>
                      </ts:defineSchema>
                      <ts:parserTestCase ID="some identifier" name="testTDMLParseDetectsErrorAnyMessage" root="data" model="mySchema">
                        <ts:document>AA</ts:document>
                        <ts:errors>
                          <ts:error/><!-- don't care what message is -->
                        </ts:errors>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val runner = new Runner(testSuite)
    runner.runOneTest("testTDMLParseDetectsErrorAnyMessage")
    runner.reset
  }

  @Test def testTDMLParseDetectsNoError(): Unit = {

    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName" xmlns:xs={ xsd } xmlns:dfdl={ dfdl } xmlns:tns={ example }>
                      <ts:defineSchema name="mySchema">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="tns:GeneralFormat"/>
                        <xs:element name="data" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>
                      </ts:defineSchema>
                      <ts:parserTestCase ID="some identifier" name="testTDMLParseDetectsNoError" root="data" model="mySchema">
                        <ts:document>37</ts:document>
                        <ts:errors>
                          <ts:error/><!-- don't care what message is -->
                        </ts:errors>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val runner = new Runner(testSuite)
    val exc = intercept[Exception] {
      runner.runOneTest("testTDMLParseDetectsNoError")
    }
    runner.reset
    assertTrue(exc.getMessage().contains("Expected error"))
  }

  @Test def testTDMLParseDetectsNoWarning(): Unit = {
  
    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName" xmlns:xs={ xsd } xmlns:dfdl={ dfdl } xmlns:tns={ example }>
                      <ts:defineSchema name="mySchema">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="tns:GeneralFormat"/>
                        <xs:element name="data" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>
                      </ts:defineSchema>
                      <ts:parserTestCase ID="some identifier" name="testTDMLParseDetectsNoWarning" root="data" model="mySchema">
                        <ts:document>37</ts:document>
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <data xmlns={ example }>37</data>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                        <ts:warnings>
                          <ts:warning/><!-- don't care what message is -->
                        </ts:warnings>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val runner = new Runner(testSuite)
    val exc = intercept[Exception] {
      runner.runOneTest("testTDMLParseDetectsNoWarning")
    }
    assertTrue(exc.getMessage().contains("expected but not found"))
    runner.reset
  }

  @Test def testTDMLParseRunAll(): Unit = {
    val testSuite = <testSuite xmlns={ tdml } suiteName="theSuiteName" xmlns:xs={ xsd } xmlns:dfdl={ dfdl } xmlns:tns={ example }>
                      <defineSchema name="mySchema">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="tns:GeneralFormat"/>
                        <xs:element name="data" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>
                      </defineSchema>
                      <parserTestCase name="testTDMLParseRunAll1" root="data" model="mySchema">
                        <document>37</document>
                        <infoset>
                          <dfdlInfoset>
                            <tns:data>37</tns:data>
                          </dfdlInfoset>
                        </infoset>
                      </parserTestCase>
                      <parserTestCase name="testTDMLParseRunAll2" root="data" model="mySchema">
                        <document>92</document>
                        <infoset>
                          <dfdlInfoset xmlns:tns={ example } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xsi={ xsi }>
                            <tns:data>92</tns:data>
                          </dfdlInfoset>
                        </infoset>
                      </parserTestCase>
                    </testSuite>
    val runner = new Runner(testSuite)
    runner.runAllTests()
    runner.reset
  }

  @Test def testInfosetFromFile(): Unit = {
    val xml = <testSuite xmlns={ tdml } ID="suite identifier" suiteName="theSuiteName" description="Some Test Suite Description">
                <parserTestCase name="test1" root="byte1" model="test-suite/ibm-contributed/dpanum.dfdl.xsd" description="Some test case description.">
                  <document>
                    <documentPart type="file">test/tdml/test.bin</documentPart>
                  </document>
                  <infoset>
                    <dfdlInfoset type="file">test/tdml/test.xml</dfdlInfoset>
                  </infoset>
                </parserTestCase>
              </testSuite>
    val runner = new Runner(xml)
    val ptc = runner.getTS.parserTestCases(0)
    val infoset = ptc.optExpectedOrInputInfoset.get
    val actual = infoset.contents
    val expected = <byte1>123</byte1>
    assertEquals(expected, XMLUtils.removeAttributes(actual))
    runner.reset
  }

  @Test def testRunModelFile(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="data" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>)

    val tmpSchemaFile = File.createTempFile("daffodil-tdml-", ".dfdl.xsd")
    using(new java.io.FileWriter(tmpSchemaFile)) {
      fileWriter =>
        fileWriter.write(testSchema.toString())
    }

    val testSuite = <testSuite xmlns={ tdml } suiteName="theSuiteName">
                      <parserTestCase name="testRunModelFile" root="data" model={ tmpSchemaFile.toURI.toString }>
                        <document>37</document>
                        <infoset>
                          <dfdlInfoset>
                            <data xmlns={ example }>37</data>
                          </dfdlInfoset>
                        </infoset>
                      </parserTestCase>
                    </testSuite>

    try {
      val runner = new Runner(testSuite)
      runner.runOneTest("testRunModelFile")
      runner.reset
    } finally {
      tmpSchemaFile.delete()
    }
  }

  @Test def testRunTDMLFileReferencingModelFile(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="data" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>)

    val tmpSchemaFile = File.createTempFile("daffodil-tdml-", ".dfdl.xsd")
    using(new java.io.FileWriter(tmpSchemaFile)) {
      fileWriter =>
        fileWriter.write(testSchema.toString())
    }

    val testSuite = <testSuite xmlns={ tdml } suiteName="theSuiteName">
                      <parserTestCase name="testRunTDMLFileReferencingModelFile" root="data" model={ tmpSchemaFile.toURI.toString }>
                        <document>37</document>
                        <infoset>
                          <dfdlInfoset>
                            <data xmlns={ example }>37</data>
                          </dfdlInfoset>
                        </infoset>
                      </parserTestCase>
                    </testSuite>

    val tmpTDMLFile = File.createTempFile("daffodil-tdml-", ".dfdl.xsd")
    try {
      using(new java.io.FileWriter(tmpSchemaFile)) {
        fw =>
          fw.write(testSchema.toString())
      }
      using(new java.io.FileWriter(tmpTDMLFile)) {
        fw =>
          fw.write(testSuite.toString())
      }
      val runner = new Runner(tmpTDMLFile)
      runner.runAllTests()
      runner.reset
    } finally {
      try {
        tmpTDMLFile.delete()
      } finally {
        tmpSchemaFile.delete()
      }
    }
  }

  val tdmlWithEmbeddedSchema =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema">
        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
        <dfdl:format ref="tns:GeneralFormat"/>
        <xsd:element name="data" type="xsd:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>
      </tdml:defineSchema>
      <parserTestCase xmlns={ tdml } name="testEmbeddedSchemaWorks" root="data" model="mySchema">
        <document>37</document>
        <infoset>
          <dfdlInfoset>
            <data xmlns={ example }>37</data>
          </dfdlInfoset>
        </infoset>
      </parserTestCase>
    </tdml:testSuite>

  @Test def testEmbeddedSchemaWorks(): Unit = {
    val testSuite = tdmlWithEmbeddedSchema
    val runner = new Runner(testSuite)
    runner.runOneTest("testEmbeddedSchemaWorks")
    runner.reset
  }

  @Test def testRunTDMLSelfContainedFile(): Unit = {
    val tmpTDMLFileName = getClass.getName() + ".tdml"
    val testSuite = tdmlWithEmbeddedSchema
    try {
      using(new java.io.FileWriter(tmpTDMLFileName)) {
        fw =>
          fw.write(testSuite.toString())
      }
      val runner = new Runner(new java.io.File(tmpTDMLFileName))
      runner.runAllTests()
      runner.reset
    } finally {
      val t = new java.io.File(tmpTDMLFileName)
      t.delete()
    }
  }

  val tdmlWithUnicode2028 =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema">
        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
        <dfdl:format ref="tns:GeneralFormat"/>
        <xsd:element name="data" type="xsd:string" dfdl:encoding="utf-8" dfdl:lengthKind="delimited" dfdl:terminator="!"/>
      </tdml:defineSchema>
      <parserTestCase xmlns={ tdml } name="testMultiByteUnicodeWorks" root="data" model="mySchema">
        <document>a&#x2028;!</document>
        <infoset>
          <dfdlInfoset>
            <data xmlns={ tns }>a&#x2028;</data>
          </dfdlInfoset>
        </infoset>
      </parserTestCase>
    </tdml:testSuite>

  @Test def testMultiByteUnicodeWorks(): Unit = {
    val testSuite = tdmlWithUnicode2028
    val runner = new Runner(testSuite)
    runner.runOneTest("testMultiByteUnicodeWorks")
    runner.reset
  }

  val tdmlWithUnicode5E74AndCDATA =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema">
        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
        <dfdl:format ref="tns:GeneralFormat"/>
        <xsd:element name="data" type="xsd:string" dfdl:encoding="utf-8" dfdl:lengthKind="delimited" dfdl:terminator=""/>
      </tdml:defineSchema>
      <parserTestCase xmlns={ tdml } name="testMultiByteUnicodeWithCDATAWorks" root="data" model="mySchema">
        <document><documentPart type="text"><![CDATA[a年]]></documentPart></document>
        <infoset>
          <dfdlInfoset>
            <data xmlns={ tns }>a&#x5E74;</data>
          </dfdlInfoset>
        </infoset>
      </parserTestCase>
    </tdml:testSuite>

  @Test def testMultiByteUnicodeWithCDATAWorks(): Unit = {
    val testSuite = tdmlWithUnicode5E74AndCDATA
    val runner = new Runner(testSuite)
    runner.runOneTest("testMultiByteUnicodeWithCDATAWorks")
    runner.reset
  }

  @Test def testNilCompare() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="tns:GeneralFormat" nilKind="literalValue" nilValueDelimiterPolicy="terminator"/>
          <xsd:element name="data" type="xsd:int" nillable="true" dfdl:lengthKind="delimited" dfdl:nilValue="nil" dfdl:terminator=";"/>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={ tdml } name="testNilCompare" root="data" model="mySchema">
          <tdml:document>nil;</tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <tns:data xsi:nil="true"/>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = new Runner(testSuite)
    runner.runOneTest("testNilCompare")
    runner.reset
  }

  @Test def testNilCompare2() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="tns:GeneralFormat" nilKind="literalValue" nilValueDelimiterPolicy="terminator"/>
          <xsd:element name="data" type="xsd:int" nillable="true" dfdl:lengthKind="delimited" dfdl:nilValue="nil" dfdl:terminator=";"/>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={ tdml } name="testNilCompare" root="data" model="mySchema">
          <tdml:document>0;</tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <tns:data xsi:nil='true'/>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = new Runner(testSuite)
    val e = intercept[Exception] {
      runner.runOneTest("testNilCompare")
    }
    runner.reset
    val msg = e.getMessage()
    assertTrue(msg.contains("Comparison failed"))
    assertTrue(msg.contains("xsi:nil=\"true\""))
  }

  /**
   * This test shows that we can import any byte at all as an iso-8859-1 character.
   */
  @Test def testAllBytesISO8859() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <xsd:element name="data" type="xsd:string" dfdl:lengthKind="delimited" dfdl:encoding="iso-8859-1"/>
          <dfdl:format ref="tns:GeneralFormat"/>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={ tdml } name="testAllBytesISO8859" root="data" model="mySchema">
          <tdml:document>
            <!-- leave out the characters for &, ", < and > because they cause us trouble in constructing the expected string result. -->
            <tdml:documentPart type="byte"><![CDATA[
00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f
10 11 12 13 14 15 16 17 18 19 1a 1b 1c 1d 1e 1f
20 21    23 24 25    27 28 29 2a 2b 2c 2d 2e 2f
30 31 32 33 34 35 36 37 38 39 3a 3b    3d    3f
40 41 42 43 44 45 46 47 48 49 4a 4b 4c 4d 4e 4f
50 51 52 53 54 55 56 57 58 59 5a 5b 5c 5d 5e 5f
60 61 62 63 64 65 66 67 68 69 6a 6b 6c 6d 6e 6f
70 71 72 73 74 75 76 77 78 79 7a 7b 7c 7d 7e 7f
80 81 82 83 84 85 86 87 88 89 8a 8b 8c 8d 8e 8f
90 91 92 93 94 95 96 97 98 99 9a 9b 9c 9d 9e 9f
a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 aa ab ac ad ae af
b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf
c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 ca cb cc cd ce cf
d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 da db dc dd de df
e0 e1 e2 e3 e4 e5 e6 e7 e8 e9 ea eb ec ed ee ef
f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 fa fb fc fd fe ff

]]></tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <!-- Note below that 0xD aka CR gets translated into 0xA or LF. -->
              <tns:data><![CDATA[]]>&#x9;&#xA;<![CDATA[]]>&#xA;<![CDATA[]]>&#x20;<![CDATA[!#$%'()*+,-./0123456789:;=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~]]>&#xA0;<![CDATA[¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ]]></tns:data>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = new Runner(testSuite)
    val bytes = runner.getTS.parserTestCases(0).document.get.documentBytes
    assertEquals(252, bytes.length)
    val tsData = (testSuite \\ "data").text
    assertEquals(252, tsData.length)
    runner.runOneTest("testAllBytesISO8859")
  }

  /**
   * This test is about a corner case when a byte containing 0xFF is encountered.
   * (There was a bug with PagedSeq[Byte] returning -1 for byte 255, making things
   * behave like end-of-data when a byte of 0xFF was encountered.
   */
  @Test def testISO8859_FF_Byte() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <xsd:element name="data" type="xsd:string" dfdl:lengthKind="delimited" dfdl:encoding="iso-8859-1"/>
          <dfdl:format ref="tns:GeneralFormat"/>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={ tdml } name="testNilCompare" root="data" model="mySchema">
          <tdml:document>
            <tdml:documentPart type="byte"><![CDATA[
31 32 33 ff ff ff 34 35 36
]]></tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <tns:data>123ÿÿÿ456</tns:data>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = new Runner(testSuite)
    val bytes = runner.getTS.parserTestCases(0).document.get.documentBytes
    assertEquals(9, bytes.length)
    runner.runOneTest("testNilCompare")
  }

  @Test def test_tdmlNamespaces1(): Unit = {
    val testDir = "/test/tdml/"
    val t0 = testDir + "tdmlNamespaces.tdml"
    val r = new Runner(t0)
    //
    // This is going to write in the default charset.
    //
    val out = new java.io.ByteArrayOutputStream()
    val e = intercept[TDMLException] {
      Console.withErr(out) {
        r.runOneTest("tdmlNamespaces1")
      }
    }
    val msgs = e.getMessage()
    // val msgs = out.toString
    // println(msgs)
    assertTrue(msgs.contains("incorrect/path/on/purpose/tdml.xsd"))
    assertTrue(msgs.contains("Unable to resolve"))
  }


  @Test def testTDMLHexBinaryTypeAwareSuccess_01(): Unit = {
    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName" xmlns:xs={ xsd } xmlns:dfdl={ dfdl } xmlns:tns={ example }>
                      <ts:defineSchema name="mySchema">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="tns:GeneralFormat"/>
                        <xs:element name="data" type="xs:hexBinary" dfdl:lengthKind="explicit" dfdl:length="4"/>
                      </ts:defineSchema>
                      <ts:parserTestCase ID="some identifier" name="testTDMLHexBinaryTypeAwareSuccess"
                        root="data" model="mySchema">
                        <ts:document>
                          <ts:documentPart type="byte">A1B2C3D4</ts:documentPart>
                        </ts:document>
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <data xmlns={ example } xmlns:xsi={ xsi } xmlns:xs={ xsd }
                              xsi:type="xs:hexBinary">a1b2c3d4</data>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val runner = new Runner(testSuite)
    runner.runOneTest("testTDMLHexBinaryTypeAwareSuccess")
  }

  @Test def testTDMLHexBinaryTypeAwareSuccess_02(): Unit = {
    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName" xmlns:xs={ xsd } xmlns:dfdl={ dfdl } xmlns:tns={ example }>
                      <ts:defineSchema name="mySchema">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="tns:GeneralFormat"/>
                        <xs:element name="data" type="xs:hexBinary" dfdl:lengthKind="explicit" dfdl:length="4"/>
                      </ts:defineSchema>
                      <ts:parserTestCase ID="some identifier" name="testTDMLHexBinaryTypeAwareSuccess"
                        root="data" model="mySchema">
                        <ts:document>
                          <ts:documentPart type="byte">A1B2C3D4</ts:documentPart>
                        </ts:document>
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <data xmlns={ example } xmlns:xsi={ xsi } xmlns:xs={ xsd }
                              xsi:type="xs:hexBinary">A1B2C3D4</data>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val runner = new Runner(testSuite)
    runner.runOneTest("testTDMLHexBinaryTypeAwareSuccess")
  }

  @Test def testTDMLHexBinaryTypeAwareFailure(): Unit = {
    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName" xmlns:xs={ xsd } xmlns:dfdl={ dfdl } xmlns:tns={ example }>
                      <ts:defineSchema name="mySchema">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="tns:GeneralFormat"/>
                        <xs:element name="data" type="xs:hexBinary" dfdl:lengthKind="explicit" dfdl:length="4"/>
                      </ts:defineSchema>
                      <ts:parserTestCase ID="some identifier" name="testTDMLHexBinaryTypeAwareFailure"
                        root="data" model="mySchema">
                        <ts:document>
                          <ts:documentPart type="byte">A1B2C3D4</ts:documentPart>
                        </ts:document>
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <data xmlns={ example }>a1b2c3d4</data>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val runner = new Runner(testSuite)
    val e = intercept[Exception] {
      runner.runOneTest("testTDMLHexBinaryTypeAwareFailure")
    }
    val msg = e.getMessage()
    assertTrue(msg.contains("Comparison failed"))
    assertTrue(msg.contains("a1b2c3d4"))
    assertTrue(msg.contains("A1B2C3D4"))
  }



  @Test def testTDMLDateTimeTypeAwareSuccess_01(): Unit = {
    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName" xmlns:xs={ xsd } xmlns:dfdl={ dfdl } xmlns:tns={ example }>
                      <ts:defineSchema name="mySchema">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="tns:GeneralFormat"/>
                        <xs:element name="data" type="xs:dateTime" dfdl:lengthKind="explicit" dfdl:length="32"
                          dfdl:calendarPatternKind="explicit"
                          dfdl:calendarPattern="uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxxxx" />
                      </ts:defineSchema>
                      <ts:parserTestCase ID="some identifier" name="testTDMLDateTimeTypeAwareSuccess"
                        root="data" model="mySchema">
                        <ts:document>1995-03-24T01:30:00.000000+00:00</ts:document>
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <data xmlns={ example } xmlns:xsi={ xsi } xmlns:xs={ xsd }
                              xsi:type="xs:dateTime">1995-03-24T01:30:00Z</data>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val runner = new Runner(testSuite)
    runner.runOneTest("testTDMLDateTimeTypeAwareSuccess")
  }

  @Test def testTDMLDateTimeTypeAwareSuccess_02(): Unit = {
    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName" xmlns:xs={ xsd } xmlns:dfdl={ dfdl } xmlns:tns={ example }>
                      <ts:defineSchema name="mySchema">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="tns:GeneralFormat"/>
                        <xs:element name="data" type="xs:dateTime" dfdl:lengthKind="explicit" dfdl:length="32"
                          dfdl:calendarPatternKind="explicit"
                          dfdl:calendarPattern="uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxxxx" />
                      </ts:defineSchema>
                      <ts:parserTestCase ID="some identifier" name="testTDMLDateTimeTypeAwareSuccess"
                        root="data" model="mySchema">
                        <ts:document>1995-03-24T01:30:00.000000+00:00</ts:document>
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <data xmlns={ example } xmlns:xsi={ xsi } xmlns:xs={ xsd }
                              xsi:type="xs:dateTime">1995-03-24T01:30:00+00:00</data>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val runner = new Runner(testSuite)
    runner.runOneTest("testTDMLDateTimeTypeAwareSuccess")
  }

  @Test def testTDMLDateTimeTypeAwareSuccess_03(): Unit = {
    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName" xmlns:xs={ xsd } xmlns:dfdl={ dfdl } xmlns:tns={ example }>
                      <ts:defineSchema name="mySchema">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="tns:GeneralFormat"/>
                        <xs:element name="data" type="xs:dateTime" dfdl:lengthKind="explicit" dfdl:length="32"
                          dfdl:calendarPatternKind="explicit"
                          dfdl:calendarPattern="uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxxxx" />
                      </ts:defineSchema>
                      <ts:parserTestCase ID="some identifier" name="testTDMLDateTimeTypeAwareSuccess"
                        root="data" model="mySchema">
                        <ts:document>1995-03-24T01:30:00.000000+00:00</ts:document>
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <data xmlns={ example } xmlns:xsi={ xsi } xmlns:xs={ xsd }
                              xsi:type="xs:dateTime">1995-03-24T01:30:00.000+00:00</data>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val runner = new Runner(testSuite)
    runner.runOneTest("testTDMLDateTimeTypeAwareSuccess")
  }

  @Test def testTDMLDateTimeTypeAwareSuccess_04(): Unit = {
    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName" xmlns:xs={ xsd } xmlns:dfdl={ dfdl } xmlns:tns={ example }>
                      <ts:defineSchema name="mySchema">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="tns:GeneralFormat"/>
                        <xs:element name="data" type="xs:dateTime" dfdl:lengthKind="explicit" dfdl:length="32"
                          dfdl:calendarPatternKind="explicit"
                          dfdl:calendarPattern="uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxxxx" />
                      </ts:defineSchema>
                      <ts:parserTestCase ID="some identifier" name="testTDMLDateTimeTypeAwareSuccess"
                        root="data" model="mySchema">
                        <ts:document>1995-03-24T01:30:00.000000+00:00</ts:document>
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <data xmlns={ example } xmlns:xsi={ xsi } xmlns:xs={ xsd }
                              xsi:type="xs:dateTime">1995-03-24T01:30:00.000000Z</data>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val runner = new Runner(testSuite)
    runner.runOneTest("testTDMLDateTimeTypeAwareSuccess")
  }

  @Test def testTDMLDateTimeTypeAwareFailure(): Unit = {
    val testSuite = <ts:testSuite xmlns:ts={ tdml } suiteName="theSuiteName" xmlns:xs={ xsd } xmlns:dfdl={ dfdl } xmlns:tns={ example }>
                      <ts:defineSchema name="mySchema">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="tns:GeneralFormat"/>
                        <xs:element name="data" type="xs:dateTime" dfdl:lengthKind="explicit" dfdl:length="32"
                          dfdl:calendarPatternKind="explicit"
                          dfdl:calendarPattern="uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxxxx" />
                      </ts:defineSchema>
                      <ts:parserTestCase ID="some identifier" name="testTDMLDateTimeTypeAwareFailure"
                        root="data" model="mySchema">
                        <ts:document>1995-03-24T01:30:00.000000+00:00</ts:document>
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <data xmlns={ example }>1995-03-24T01:30:00Z</data>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val runner = new Runner(testSuite)
    val e = intercept[Exception] {
      runner.runOneTest("testTDMLDateTimeTypeAwareFailure")
    }
    val msg = e.getMessage()
    assertTrue(msg.contains("Comparison failed"))
    assertTrue(msg.contains("1995-03-24T01:30:00Z"))
    assertTrue(msg.contains("1995-03-24T01:30:00+00:00"))
  }

  /**
   * Test illustrates problem with multiple document parts having a RTL byte order
   * not being assembled properly.
   *
   * That or the document bitOrder is causing the parts bitOrders to be assembled incorrectly.
   *
   * There are many other tests that use RTL byte order to assemble bits together, so it is
   * something about mixing byteOrder RTL with LTR that is causing the problem.
   *
   * Bug DAFFODIL-1898
   */
  //  @Test def testMixedBigEndianMSBFWithLittleEndianLSBF() {
  //    val xml = <document bitOrder="MSBFirst" xmlns="http://www.ibm.com/xmlns/dfdl/testData">
  //                <documentPart type="byte" bitOrder="MSBFirst" byteOrder="LTR">AA                  </documentPart>
  //                <documentPart type="bits" bitOrder="LSBFirst" byteOrder="RTL">XXXX X001</documentPart>
  //                <documentPart type="bits" bitOrder="LSBFirst" byteOrder="RTL">1111 1XXX</documentPart>
  //                <!-- The above is AAF9 -->
  //              </document>
  //    val doc = new Document(xml, null)
  //    val bytes = doc.documentBytes
  //    val hexDigits = Misc.bytes2Hex(bytes)
  //    val expected = "AAF9".replace(" ", "")
  //    assertEquals(expected, hexDigits)
  //  }

  @Test def testDuplicateDefineSchema(): Unit = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="dupSchema">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="tns:GeneralFormat"/>
          <xsd:element name="data" type="xsd:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>
        </tdml:defineSchema>
        <tdml:defineSchema name="dupSchema">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="tns:GeneralFormat"/>
          <xsd:element name="data" type="xsd:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>
        </tdml:defineSchema>
        <parserTestCase xmlns={ tdml } name="testCase" root="data" model="dupSchema">
          <document>37</document>
          <infoset>
            <dfdlInfoset>
              <data xmlns={ example }>37</data>
            </dfdlInfoset>
          </infoset>
        </parserTestCase>
      </tdml:testSuite>
    val runner = new Runner(testSuite)
    val exc = intercept[Exception] {
      runner.runOneTest("testCase")
    }
    runner.reset
    assertTrue(exc.getMessage().contains("Duplicate definitions found for defineSchema: dupSchema"))
  }

  @Test def testDuplicateTestCase(): Unit = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="tns:GeneralFormat"/>
          <xsd:element name="data" type="xsd:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>
        </tdml:defineSchema>
        <parserTestCase xmlns={ tdml } name="dupTestCase" root="data" model="mySchema">
          <document>37</document>
          <infoset>
            <dfdlInfoset>
              <data xmlns={ example }>37</data>
            </dfdlInfoset>
          </infoset>
        </parserTestCase>
        <unparserTestCase xmlns={ tdml } name="dupTestCase" root="data" model="mySchema">
          <document>37</document>
          <infoset>
            <dfdlInfoset>
              <data xmlns={ example }>37</data>
            </dfdlInfoset>
          </infoset>
        </unparserTestCase>
      </tdml:testSuite>
    val runner = new Runner(testSuite)
    val exc = intercept[Exception] {
      runner.runOneTest("testCase")
    }
    runner.reset
    assertTrue(exc.getMessage().contains("Duplicate definitions found for parser or unparser test cases: dupTestCase"))
  }

  @Test def testDuplicateDefineConfig(): Unit = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="tns:GeneralFormat"/>
          <xsd:element name="data" type="xsd:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>
        </tdml:defineSchema>
        <tdml:defineConfig name="dupConfig" />
        <tdml:defineConfig name="dupConfig" />
        <parserTestCase xmlns={ tdml } name="testCase" root="data" model="mySchema" config="dupConfig">
          <document>37</document>
          <infoset>
            <dfdlInfoset>
              <data xmlns={ example }>37</data>
            </dfdlInfoset>
          </infoset>
        </parserTestCase>
      </tdml:testSuite>
    val runner = new Runner(testSuite)
    val exc = intercept[Exception] {
      runner.runOneTest("testCase")
    }
    runner.reset
    assertTrue(exc.getMessage().contains("Duplicate definitions found for defineConfig: dupConfig"))
  }

  @Test def testNonXSDModelNegativeBadCompiledFile(): Unit = {
    val testSuite =
       <tdml:testSuite suiteName="theSuiteName"
                       xmlns:tns={ tns }
                       xmlns:tdml={ tdml }
                       xmlns:dfdl={ dfdl }
                       xmlns:xsd={ xsd }
                       xmlns:xs={ xsd }
                       xmlns:xsi={ xsi }>
         <parserTestCase xmlns={ tdml } name="testCase" root="data"
                         model="test/tdml/fake-precompiled-dfdl-schema.bin" >
           <document/>
           <infoset><dfdlInfoset><data/></dfdlInfoset></infoset>
           </parserTestCase>
       </tdml:testSuite>
    val runner = new Runner(testSuite)
    val exc = intercept[Exception] {
      runner.runOneTest("testCase")
    }
    runner.reset()
    assertTrue(exc.getMessage.contains("only compatible with Daffodil 300.400.500"))
  }
}
