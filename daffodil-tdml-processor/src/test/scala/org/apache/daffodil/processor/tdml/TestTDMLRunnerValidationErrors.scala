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

package org.apache.daffodil.processor.tdml

import org.apache.daffodil.lib.Implicits.intercept
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.tdml.Runner
import org.apache.daffodil.tdml.TDMLException

import org.junit.Assert.assertTrue
import org.junit.Test

class TestTDMLRunnerValidationErrors {

  lazy val testSuite1 =
    <ts:testSuite xmlns:dfdl={XMLUtils.DFDL_NAMESPACE} xmlns:xs={
      XMLUtils.XSD_NAMESPACE
    } xmlns:ex={XMLUtils.EXAMPLE_NAMESPACE} xmlns:ts={
      XMLUtils.TDML_NAMESPACE
    } suiteName="theSuiteName"
                  defaultValidation="on"
                  defaultIgnoreUnexpectedValidationErrors="false">
      <ts:defineSchema name="schema1">
        <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
        <dfdl:format ref="ex:GeneralFormat"/>
        <xs:element name="r" dfdl:lengthKind="delimited">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="YES"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:element>
      </ts:defineSchema>

      <ts:parserTestCase name="expectsNoValidationErrorGetsValidationError" root="r" model="schema1" roundTrip="onePass">
        <ts:infoset>
          <ts:dfdlInfoset>
            <ex:r>foo</ex:r>
          </ts:dfdlInfoset>
        </ts:infoset>
        <ts:document>foo</ts:document>
      </ts:parserTestCase>

      <ts:parserTestCase name="expectsValidationErrorGetsNoValidationError" root="r" model="schema1" roundTrip="onePass">
        <ts:infoset>
          <ts:dfdlInfoset>
            <ex:r>YES</ex:r>
          </ts:dfdlInfoset>
        </ts:infoset>
        <ts:document>YES</ts:document>

        <ts:validationErrors>
          <ts:error>Validation Error</ts:error>
          <ts:error>r</ts:error>
          <ts:error>not valid</ts:error>
          <ts:error>foo</ts:error>
          <ts:error>not facet-valid</ts:error>
          <ts:error>[YES]</ts:error>
        </ts:validationErrors>
      </ts:parserTestCase>

      <ts:parserTestCase name="expectsValidationErrorGetsValidationError" root="r" model="schema1" roundTrip="onePass">
        <ts:infoset>
          <ts:dfdlInfoset>
            <ex:r>foo</ex:r>
          </ts:dfdlInfoset>
        </ts:infoset>
        <ts:document>foo</ts:document>

        <ts:validationErrors>
          <ts:error>Validation Error</ts:error>
          <ts:error>r</ts:error>
          <ts:error>not valid</ts:error>
          <ts:error>foo</ts:error>
          <ts:error>not facet-valid</ts:error>
          <ts:error>[YES]</ts:error>        </ts:validationErrors>
      </ts:parserTestCase>

      <ts:parserTestCase name="expectsNoValidationErrorGetsNoValidationError" root="r" model="schema1" roundTrip="onePass">
        <ts:infoset>
          <ts:dfdlInfoset>
            <ex:r>YES</ex:r>
          </ts:dfdlInfoset>
        </ts:infoset>
        <ts:document>YES</ts:document>
      </ts:parserTestCase>

    </ts:testSuite>

  lazy val testSuite2 =
    <ts:testSuite xmlns:dfdl={XMLUtils.DFDL_NAMESPACE} xmlns:xs={
      XMLUtils.XSD_NAMESPACE
    } xmlns:ex={XMLUtils.EXAMPLE_NAMESPACE} xmlns:ts={
      XMLUtils.TDML_NAMESPACE
    } suiteName="theSuiteName"
                  defaultValidation="on">
      <ts:defineSchema name="schema1">
        <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
        <dfdl:format ref="ex:GeneralFormat"/>
        <xs:element name="r" dfdl:lengthKind="delimited">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="YES"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:element>
      </ts:defineSchema>

      <ts:parserTestCase name="expectsNoValidationErrorGetsValidationError2"
                         root="r" model="schema1" roundTrip="onePass"
                         ignoreUnexpectedValidationErrors="true">
        <ts:infoset>
          <ts:dfdlInfoset>
            <ex:r>foo</ex:r>
          </ts:dfdlInfoset>
        </ts:infoset>
        <ts:document>foo</ts:document>
      </ts:parserTestCase>

      <ts:parserTestCase name="expectsNoValidationErrorGetsValidationError3"
                         root="r" model="schema1" roundTrip="onePass"
                         ignoreUnexpectedValidationErrors="false">
        <ts:infoset>
          <ts:dfdlInfoset>
            <ex:r>foo</ex:r>
          </ts:dfdlInfoset>
        </ts:infoset>
        <ts:document>foo</ts:document>
      </ts:parserTestCase>
    </ts:testSuite>

  lazy val testSuite3 =
    <ts:testSuite xmlns:dfdl={XMLUtils.DFDL_NAMESPACE} xmlns:xs={
      XMLUtils.XSD_NAMESPACE
    } xmlns:ex={XMLUtils.EXAMPLE_NAMESPACE} xmlns:ts={
      XMLUtils.TDML_NAMESPACE
    } suiteName="theSuiteName"
                  defaultValidation="on"
                  defaultIgnoreUnexpectedValidationErrors="false">
      <ts:defineSchema name="schema1">
        <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
        <dfdl:format ref="ex:GeneralFormat"/>
        <xs:element name="r" dfdl:lengthKind="delimited">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="YES"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:element>
      </ts:defineSchema>

      <ts:parserTestCase name="enforceNoValidationErrorsUnsupportedVersion" root="r" model="schema1" roundTrip="onePass">
        <ts:infoset>
          <ts:dfdlInfoset>
            <ex:r>YES</ex:r>
          </ts:dfdlInfoset>
        </ts:infoset>
        <ts:document>YES</ts:document>
        <ts:validationErrors/>
      </ts:parserTestCase>


    </ts:testSuite>

  @Test def test_expectsNoValidationErrorGetsValidationError(): Unit = {
    val runner = new Runner(testSuite1)
    val e = intercept[TDMLException] {
      runner.runOneTest("expectsNoValidationErrorGetsValidationError")
    }
    runner.reset
    val msg = e.getMessage()
    assertTrue(
      msg.contains(
        "ignoreUnexpectedValidationErrors = false and test does not expect ValidationError diagnostics"
      )
    )
  }
  @Test def test_expectsNoValidationErrorGetsValidationError2(): Unit = {
    val runner = new Runner(testSuite2)
    runner.runOneTest("expectsNoValidationErrorGetsValidationError2")
  }

  @Test def test_expectsNoValidationErrorGetsValidationError3(): Unit = {
    val runner = new Runner(testSuite2)
    val e = intercept[TDMLException] {
      runner.runOneTest("expectsNoValidationErrorGetsValidationError3")
    }
    runner.reset
    val msg = e.getMessage()
    assertTrue(
      msg.contains(
        "ignoreUnexpectedValidationErrors = false and test does not expect ValidationError diagnostics"
      )
    )
  }

  @Test def test_expectsValidationErrorGetsNoValidationError(): Unit = {
    val runner = new Runner(testSuite1)
    val e = intercept[TDMLException] {
      runner.runOneTest("expectsValidationErrorGetsNoValidationError")
    }
    runner.reset
    val msg = e.getMessage()
    assertTrue(msg.contains("Diagnostic message(s) were expected but not found"))
  }

  @Test def test_expectsValidationErrorGetsValidationError(): Unit = {
    val runner = new Runner(testSuite1)
    runner.runOneTest("expectsValidationErrorGetsValidationError")
  }

  @Test def test_expectsNoValidationErrorGetsNoValidationError(): Unit = {
    val runner = new Runner(testSuite1)
    runner.runOneTest("expectsNoValidationErrorGetsNoValidationError")
  }

  @Test def test_enforceNoValidationErrorsUnsupportedVersion(): Unit = {
    val runner = new Runner(testSuite3)
    val e = intercept[TDMLException] {
      runner.runOneTest("enforceNoValidationErrorsUnsupportedVersion")
    }
    runner.reset
    val msg = e.getMessage()
    assertTrue(msg.contains("content of element 'ts:validationErrors' is not complete"))
    assertTrue(msg.contains("error"))
    assertTrue(msg.contains("is expected"))
  }
}
