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

import org.apache.daffodil.xml.XMLUtils
import org.junit.Assert.assertTrue
import org.junit.Test
import org.apache.daffodil.Implicits._

/**
 * Tests for bug DAFFODIL-1868
 */
class TestTDMLRunnerConfig {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val daf = XMLUtils.EXT_NS_APACHE.toString
  val tns = example

  @Test def testNonsenseDefaultConfig() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi } defaultConfig="nonsense">
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <xs:element name="dummy" type="xs:string"/>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={ tdml } name="test1" root="dummy" model="mySchema">
          <tdml:document/>
          <tdml:errors>
            <tdml:error>defaultConfig</tdml:error>
            <tdml:error>nonsense</tdml:error>
            <tdml:error>not found</tdml:error>
          </tdml:errors>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = new Runner(testSuite)
    val e = intercept[Exception] {
      runner.runOneTest("test1")
    }
    runner.reset
    val msg = e.getMessage()
    assertTrue(msg.contains("defaultConfig"))
    assertTrue(msg.contains("nonsense"))
    assertTrue(msg.contains("not found"))
  }

  @Test def testNonsenseConfig() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="tns:GeneralFormat"/>
          <xs:element name="dummy" type="xs:string" dfdl:lengthKind="delimited"/>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={ tdml } name="test1" root="dummy" model="mySchema" config="nonsense">
          <tdml:document/>
          <tdml:errors>
            <tdml:error>config</tdml:error>
            <tdml:error>nonsense</tdml:error>
            <tdml:error>not found</tdml:error>
          </tdml:errors>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = new Runner(testSuite)
    val e = intercept[Exception] {
      runner.runOneTest("test1")
    }
    runner.reset
    val msg = e.getMessage()
    assertTrue(msg.contains("config"))
    assertTrue(msg.contains("nonsense"))
    assertTrue(msg.contains("not found"))
  }

  @Test def testGoodConfigNonsenseDefaultConfig() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:daf={ daf } xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi } defaultConfig="nonsense">
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="tns:GeneralFormat"/>
          <xs:element name="dummy" type="xs:string" dfdl:lengthKind="delimited"/>
        </tdml:defineSchema>
        <tdml:defineConfig name="myConfig">
          <daf:tunables xmlns="http://www.w3.org/2001/XMLSchema" xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <daf:requireBitOrderProperty>true</daf:requireBitOrderProperty>
          </daf:tunables>
        </tdml:defineConfig>
        <tdml:parserTestCase xmlns={ tdml } name="test1" root="dummy" model="mySchema" config="myConfig">
          <tdml:document/>
          <tdml:errors>
            <tdml:error>defaultConfig</tdml:error>
            <tdml:error>nonsense</tdml:error>
            <tdml:error>not found</tdml:error>
          </tdml:errors>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = new Runner(testSuite)
    val e = intercept[Exception] {
      runner.runOneTest("test1")
    }
    val msg = e.getMessage()
    assertTrue(msg.contains("defaultConfig"))
    assertTrue(msg.contains("nonsense"))
    assertTrue(msg.contains("not found"))
  }

  @Test def testGoodConfigNoBitOrderProp() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:daf={ daf } xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format representation="binary" binaryNumberRep="binary" lengthKind="implicit" lengthUnits="bytes" alignmentUnits="bytes" alignment="implicit" leadingSkip="0" trailingSkip="0" byteOrder="bigEndian" textOutputMinLength="0" initiator="" terminator="" textPadKind="none" encoding="ascii"/>
          <xs:element name="dummy" type="xs:int"/>
        </tdml:defineSchema>
        <tdml:defineConfig name="myConfig">
          <daf:tunables xmlns="http://www.w3.org/2001/XMLSchema" xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <daf:requireBitOrderProperty>true</daf:requireBitOrderProperty>
          </daf:tunables>
        </tdml:defineConfig>
        <tdml:parserTestCase xmlns={ tdml } name="test1" root="dummy" model="mySchema" config="myConfig">
          <tdml:document/>
          <tdml:errors>
            <tdml:error>Schema Definition Error</tdml:error>
            <tdml:error>Property bitOrder is not defined</tdml:error>
          </tdml:errors>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = new Runner(testSuite)
    runner.runOneTest("test1")
    runner.reset
  }

  @Test def testGoodDefaultConfigNoBitOrderProp() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:daf={ daf } xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi } defaultConfig="myConfig">
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format representation="binary" binaryNumberRep="binary" lengthKind="implicit" lengthUnits="bytes" alignmentUnits="bytes" alignment="implicit" leadingSkip="0" trailingSkip="0" byteOrder="bigEndian" textOutputMinLength="0" initiator="" terminator="" textPadKind="none" encoding="ascii"/>
          <xs:element name="dummy" type="xs:int"/>
        </tdml:defineSchema>
        <tdml:defineConfig name="myConfig">
          <daf:tunables xmlns="http://www.w3.org/2001/XMLSchema" xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <daf:requireBitOrderProperty>true</daf:requireBitOrderProperty>
          </daf:tunables>
        </tdml:defineConfig>
        <tdml:parserTestCase xmlns={ tdml } name="test1" root="dummy" model="mySchema">
          <tdml:document/>
          <tdml:errors>
            <tdml:error>Schema Definition Error</tdml:error>
            <tdml:error>Property bitOrder is not defined</tdml:error>
          </tdml:errors>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = new Runner(testSuite)
    runner.runOneTest("test1")
    runner.reset
  }

  @Test def testAmbiguousFileAndDefaultConfig() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:daf={ daf } xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi } defaultConfig="testConfigFile.xml">
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format representation="binary" binaryNumberRep="binary" lengthKind="implicit" lengthUnits="bytes" alignmentUnits="bytes" alignment="implicit" leadingSkip="0" trailingSkip="0" byteOrder="bigEndian" textOutputMinLength="0" initiator="" terminator="" textPadKind="none" encoding="ascii"/>
          <xs:element name="dummy" type="xs:int"/>
        </tdml:defineSchema>
        <tdml:defineConfig name="testConfigFile.xml">
          <!-- name ambiguous with file for test purposes -->
          <daf:tunables xmlns="http://www.w3.org/2001/XMLSchema" xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <daf:requireBitOrderProperty>true</daf:requireBitOrderProperty>
          </daf:tunables>
        </tdml:defineConfig>
        <tdml:parserTestCase xmlns={ tdml } name="test1" root="dummy" model="mySchema">
          <tdml:document/>
          <tdml:errors>
            <tdml:error>Schema Definition Error</tdml:error>
            <tdml:error>Property bitOrder is not defined</tdml:error>
          </tdml:errors>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = new Runner(testSuite)
    val e = intercept[Exception] {
      runner.runOneTest("test1")
    }
    runner.reset
    val msg = e.getMessage()
    assertTrue(msg.contains("ambiguous"))
    assertTrue(msg.contains("testConfigFile.xml"))
  }
}
