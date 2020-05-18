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

package org.apache.daffodil.section13.validation_errors

import org.apache.daffodil.tdml.Runner
import org.apache.daffodil.xml.XMLUtils
import org.junit.Test

class PadCharacter {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val tns = XMLUtils.EXAMPLE_NAMESPACE

  @Test def test_short_form_pad_char() = {
    // This test demonstrates that you cannot use a literal whitespace character
    // in the short form of the pad character's property binding syntax
    val testSuite =
      <tdml:testSuite suiteName="PadCharTests" description="Section 13 - Pad Character Tests"
                      xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:tns={ tns }>
        <tdml:defineSchema name="padCharSchema">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd" />
          <dfdl:format ref="tns:GeneralFormat" textTrimKind="padChar" textPadKind="padChar" />
          <xs:element name="shortForm" type="xs:string" dfdl:textStringPadCharacter=" " />
        </tdml:defineSchema>

        <tdml:parserTestCase name="short_form_pad_char" root="shortForm" model="padCharSchema">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[      hello]]></tdml:documentPart>
          </tdml:document>
          <tdml:errors>
            <tdml:error>facet-valid</tdml:error>
            <tdml:error>NonEmptyStringLiteral</tdml:error>
          </tdml:errors>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = Runner(testSuite, validateTDMLFile = false)
    runner.runOneTest("short_form_pad_char")
    runner.reset
  }

  @Test def test_long_form_pad_char() = {
    // This test demonstrates that you cannot use a literal whitespace character
    // in the attribute (long) form of the pad character's property binding syntax
    val testSuite =
      <tdml:testSuite suiteName="PadCharTests" description="Section 13 - Pad Character Tests"
                      xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:tns={ tns }>
        <tdml:defineSchema name="padCharSchema">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd" />
          <dfdl:format ref="tns:GeneralFormat" textTrimKind="padChar" textPadKind="padChar" />
          <xs:element name="longForm" type="xs:string">
            <xs:annotation>
              <xs:appinfo source="http://www.ogf.org/dfdl/">
                <dfdl:format textStringPadCharacter=" "/>
              </xs:appinfo>
            </xs:annotation>
          </xs:element>
        </tdml:defineSchema>

        <tdml:parserTestCase name="long_form_pad_char" root="longForm" model="padCharSchema">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[      hello]]></tdml:documentPart>
          </tdml:document>
          <tdml:errors>
            <tdml:error>facet-valid</tdml:error>
            <tdml:error>NonEmptyStringLiteral</tdml:error>
          </tdml:errors>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = Runner(testSuite, validateTDMLFile = false)
    runner.runOneTest("long_form_pad_char")
    runner.reset
  }

  @Test def test_property_form_pad_char() = {
    // This test demonstrates that you can use a literal whitespace character
    // in the element (property) form of the pad character's property binding
    // syntax but internal validation logic will throw an error
    val testSuite =
      <tdml:testSuite suiteName="PadCharTests" description="Section 13 - Pad Character Tests"
                      xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:tns={ tns }>
        <tdml:defineSchema name="padCharSchema">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd" />
          <dfdl:format ref="tns:GeneralFormat" textTrimKind="padChar" textPadKind="padChar" />
          <xs:element name="propertyForm" type="xs:string">
            <xs:annotation>
              <xs:appinfo source="http://www.ogf.org/dfdl/">
                <dfdl:element>
                  <dfdl:property name="textStringPadCharacter"><![CDATA[ ]]></dfdl:property>
                </dfdl:element>
              </xs:appinfo>
            </xs:annotation>
          </xs:element>
        </tdml:defineSchema>

        <tdml:parserTestCase name="property_form_pad_char" root="propertyForm" model="padCharSchema">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[      hello]]></tdml:documentPart>
          </tdml:document>
          <tdml:errors>
            <tdml:error>Schema Definition Error</tdml:error>
            <tdml:error>whitespace</tdml:error>
          </tdml:errors>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = Runner(testSuite)
    runner.runOneTest("property_form_pad_char")
    runner.reset
  }
}
