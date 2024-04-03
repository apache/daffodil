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

import org.junit.AfterClass
import org.junit.Assert.assertTrue
import org.junit.Test

object TestTDMLRunnerMatchAttributes {
  val runner = Runner("/test/tdml/", "testTDMLErrorsWarningsMatchAttribute.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}

class TestTDMLRunnerMatchAttributes {
  import TestTDMLRunnerMatchAttributes._

  // DAFFODIL-1583
  @Test def test_getsWarningExpectsWarnings(): Unit = {
    runner.runOneTest("getsWarningExpectsWarnings")
  }
  @Test def test_getUnparserWarningWhenExpectingError(): Unit = {
    runner.runOneTest("getUnparserWarningWhenExpectingError")
  }
  @Test def test_expectsAnyValidationError() = {
    runner.runOneTest("expectsAnyValidationError")
  }

  @Test def test_noWarningExpectsNoWarnings() = {
    runner.runOneTest("noWarningExpectsNoWarnings")
  }

  @Test def test_getsWarningExpectsNoWarnings() = {
    val e = intercept[TDMLException] {
      runner.runOneTest("getsWarningExpectsNoWarnings")
    }
    val errMsg = e.getMessage()
    assertTrue(
      errMsg.contains(
        "ignoreUnexpectedDiags = false and test does not expect Warning diagnostics"
      )
    )
  }

  lazy val testSuite =
    <ts:testSuite xmlns:dfdl={XMLUtils.DFDL_NAMESPACE} xmlns:xs={
      XMLUtils.XSD_NAMESPACE
    } xmlns:ex={XMLUtils.EXAMPLE_NAMESPACE} xmlns:ts={
      XMLUtils.TDML_NAMESPACE
    } suiteName="theSuiteName"
      defaultIgnoreUnexpectedWarnings="false">
      <ts:defineSchema name="causesWarnings">
        <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
        <dfdl:format ref="ex:GeneralFormat"/>
        <xs:element name="r" type="xs:string" dfdl:lengthKind="delimited">
          <xs:annotation>
            <xs:appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/" />
          </xs:annotation>
        </xs:element>
      </ts:defineSchema>
      <ts:defineSchema name="noWarnings">
        <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
        <dfdl:format ref="ex:GeneralFormat"/>
        <xs:element name="r" type="xs:string" dfdl:lengthKind="delimited">
          <xs:annotation>
            <xs:appinfo source="http://www.ogf.org/dfdl/" />
          </xs:annotation>
        </xs:element>
      </ts:defineSchema>

      <ts:parserTestCase name="expectsNoWarningGetsWarnings" root="r" model="causesWarnings" roundTrip="onePass">
        <ts:infoset>
          <ts:dfdlInfoset>
            <ex:r>foo</ex:r>
          </ts:dfdlInfoset>
        </ts:infoset>
        <ts:document>foo</ts:document>
      </ts:parserTestCase>

      <ts:parserTestCase name="expectsWarningGetsNoWarnings" root="r" model="noWarnings" roundTrip="onePass">
        <ts:infoset>
          <ts:dfdlInfoset>
            <ex:r>foo</ex:r>
          </ts:dfdlInfoset>
        </ts:infoset>
        <ts:document>foo</ts:document>

        <ts:warnings>
          <ts:warning>Schema Definition Warning</ts:warning>
          <ts:warning>The xs:appinfo source attribute value 'http://www.ogf.org/dfdl/dfdl-1.0/' should be 'http://www.ogf.org/dfdl/'.</ts:warning>
        </ts:warnings>
      </ts:parserTestCase>

      <ts:parserTestCase name="expectsWarningGetsWarning" root="r" model="causesWarnings" roundTrip="onePass">
        <ts:infoset>
          <ts:dfdlInfoset>
            <ex:r>foo</ex:r>
          </ts:dfdlInfoset>
        </ts:infoset>
        <ts:document>foo</ts:document>

        <ts:warnings>
          <ts:warning>Schema Definition Warning</ts:warning>
          <ts:warning>The xs:appinfo source attribute value 'http://www.ogf.org/dfdl/dfdl-1.0/' should be 'http://www.ogf.org/dfdl/'.</ts:warning>
        </ts:warnings>
      </ts:parserTestCase>

      <ts:parserTestCase name="expectsNoWarningGetsNoWarnings" root="r" model="noWarnings" roundTrip="onePass">
        <ts:infoset>
          <ts:dfdlInfoset>
            <ex:r>foo</ex:r>
          </ts:dfdlInfoset>
        </ts:infoset>
        <ts:document>foo</ts:document>
      </ts:parserTestCase>
    </ts:testSuite>

  @Test def test_expectsNoWarningGetsWarnings(): Unit = {
    val runner = new Runner(testSuite)
    val e = intercept[TDMLException] {
      runner.runOneTest("expectsNoWarningGetsWarnings")
    }
    runner.reset
    val msg = e.getMessage()
    assertTrue(
      msg.contains("ignoreUnexpectedDiags = false and test does not expect Warning diagnostics")
    )
  }

  @Test def test_expectsWarningGetsNoWarnings(): Unit = {
    val runner = new Runner(testSuite)
    val e = intercept[TDMLException] {
      runner.runOneTest("expectsWarningGetsNoWarnings")
    }
    runner.reset
    val msg = e.getMessage()
    assertTrue(msg.contains("Diagnostic message(s) were expected but not found"))
  }
  @Test def test_expectsWarningGetsWarning(): Unit = {
    val runner = new Runner(testSuite)
    runner.runOneTest("expectsWarningGetsWarning")
  }
  @Test def test_expectsNoWarningGetsNoWarnings(): Unit = {
    val runner = new Runner(testSuite)
    runner.runOneTest("expectsNoWarningGetsNoWarnings")
  }
}
