/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.illinois.ncsa.daffodil.tdml

import org.junit.Test
import org.junit.AfterClass
import edu.illinois.ncsa.daffodil.Implicits._
import junit.framework.Assert.fail

object TestTDMLRunnerWarnings {
  val runner = Runner("/test/tdml/", "testWarnings.tdml")

  @AfterClass def shutDown {
    runner.reset
  }
}

class TestTDMLRunnerWarnings {
  import TestTDMLRunnerWarnings._

  // DAFFODIL-1583
  @Test def test_warningWhenExpectingSuccess() = { runner.runOneTest("warningWhenExpectingSuccess") }
  @Test def test_warningWhenExpectingError() = { runner.runOneTest("warningWhenExpectingError") }
  @Test def test_unparserWarningWhenExpectingSuccess() = { runner.runOneTest("unparserWarningWhenExpectingSuccess") }
  @Test def test_unparserWarningWhenExpectingError() = { runner.runOneTest("unparserWarningWhenExpectingError") }

  /*
   * These tests insure that the TDML runner is actually testing the warnings.
   * A TDML test could silently not even be checking the warnings and pass with false positive.
   * These tests cause the TDML runner test to fail to find warnings, and check
   * that we got the TDML runner to detect this.
   */

  @Test def testWarningsCheckedParserExpectingSuccess() = {
    val testSuite =
      <tdml:testSuite suiteName="tdml warnings" xmlns:tdml="http://www.ibm.com/xmlns/dfdl/testData" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:ex="http://example.com" xmlns:fn="http://www.w3.org/2005/xpath-functions">
        <tdml:defineSchema name="causesWarnings" elementFormDefault="unqualified">
          <dfdl:format ref="ex:daffodilTest1" lengthKind="explicit"/>
          <xs:element name="causesWarnings" dfdl:lengthKind="implicit">
            <xs:complexType>
              <xs:sequence>
                <xs:element name="A" type="xs:string" dfdl:length="1"/>
                <xs:element name="AmbigElt" type="xs:string" dfdl:length="1"/>
                <xs:element name="B" type="xs:string" dfdl:length="1"/>
                <xs:element name="AmbigElt" type="xs:string" dfdl:length="1" minOccurs="0"/>
                <xs:sequence>
                  <xs:annotation>
                    <xs:appinfo source="http://www.ogf.org/dfdl/">
                      <dfdl:discriminator test="{ ./AmbigElt eq '2' }"/>
                    </xs:appinfo>
                  </xs:annotation>
                </xs:sequence>
              </xs:sequence>
            </xs:complexType>
          </xs:element>
        </tdml:defineSchema>
        <tdml:parserTestCase name="warningWhenExpectingSuccess" root="causesWarnings" model="causesWarnings">
          <tdml:document><![CDATA[123]]></tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <ex:causesWarnings>
                <A>1</A>
                <AmbigElt>2</AmbigElt>
                <B>3</B>
              </ex:causesWarnings>
            </tdml:dfdlInfoset>
          </tdml:infoset>
          <tdml:warnings>
            <tdml:warning>This will not be found</tdml:warning>
          </tdml:warnings>
        </tdml:parserTestCase>
      </tdml:testSuite>

    lazy val ts = new DFDLTestSuite(testSuite)
    val e = intercept[Exception] {
      ts.runOneTest("warningWhenExpectingSuccess")
    }
    val msg = e.getMessage()
    System.err.println(msg)
    if (!msg.contains("This will not be found")) {
      println(msg)
      fail("TDML Warnings were not checked")
    }
  }

  @Test def testWarningsCheckedParserExpectingError() = {
    val testSuite =
      <tdml:testSuite suiteName="tdml warnings" xmlns:tdml="http://www.ibm.com/xmlns/dfdl/testData" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:ex="http://example.com" xmlns:fn="http://www.w3.org/2005/xpath-functions">
        <tdml:defineSchema name="causesWarnings" elementFormDefault="unqualified">
          <dfdl:format ref="ex:daffodilTest1" lengthKind="explicit"/>
          <xs:element name="causesWarnings" dfdl:lengthKind="implicit">
            <xs:complexType>
              <xs:sequence>
                <xs:element name="A" type="xs:string" dfdl:length="1"/>
                <xs:element name="AmbigElt" type="xs:string" dfdl:length="1"/>
                <xs:element name="B" type="xs:string" dfdl:length="1"/>
                <xs:element name="AmbigElt" type="xs:string" dfdl:length="1" minOccurs="0"/>
                <xs:sequence>
                  <xs:annotation>
                    <xs:appinfo source="http://www.ogf.org/dfdl/">
                      <dfdl:discriminator test="{ ./AmbigElt eq '2' }"/>
                    </xs:appinfo>
                  </xs:annotation>
                </xs:sequence>
              </xs:sequence>
            </xs:complexType>
          </xs:element>
        </tdml:defineSchema>
        <tdml:parserTestCase name="warningWhenExpectingError" root="causesWarnings" model="causesWarnings" description="TDML runner verifies warnings and errors.">
          <tdml:document><![CDATA[1234]]></tdml:document>
          <tdml:errors>
            <tdml:error>Schema Definition Error</tdml:error>
            <tdml:error>query-style</tdml:error>
            <tdml:error>AmbigElt</tdml:error>
          </tdml:errors>
          <tdml:warnings>
            <tdml:warning>This will not be found</tdml:warning>
          </tdml:warnings>
        </tdml:parserTestCase>
      </tdml:testSuite>

    lazy val ts = new DFDLTestSuite(testSuite)
    val e = intercept[Exception] {
      ts.runOneTest("warningWhenExpectingError")
    }
    val msg = e.getMessage()
    System.err.println(msg)
    if (!msg.contains("This will not be found")) {
      println(msg)
      fail("TDML Warnings were not checked")
    }
  }

  @Test def testWarningsCheckedUnparserExpectingSuccess() = {
    val testSuite =
      <tdml:testSuite suiteName="tdml warnings" xmlns:tdml="http://www.ibm.com/xmlns/dfdl/testData" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:ex="http://example.com" xmlns:fn="http://www.w3.org/2005/xpath-functions">
        <tdml:defineSchema name="causesWarnings" elementFormDefault="unqualified">
          <dfdl:format ref="ex:daffodilTest1" lengthKind="explicit"/>
          <xs:element name="errUnparsing" dfdl:lengthKind="implicit">
            <xs:complexType>
              <xs:sequence>
                <xs:element name="A" type="xs:string" dfdl:length="1"/>
                <xs:element name="AmbigElt" type="xs:string" dfdl:length="1"/>
                <xs:element name="B" type="xs:string" dfdl:length="1"/>
                <xs:element name="AmbigElt" type="xs:string" dfdl:length="1" minOccurs="0"/>
                <xs:element name="UnparseFails" type="xs:string" dfdl:length="1" dfdl:outputValueCalc="{ ../AmbigElt }"/>
              </xs:sequence>
            </xs:complexType>
          </xs:element>
        </tdml:defineSchema>
        <tdml:unparserTestCase name="unparserWarningWhenExpectingSuccess" root="errUnparsing" model="causesWarnings" roundTrip="false">
          <tdml:document><![CDATA[1232]]></tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <ex:errUnparsing>
                <A>1</A>
                <AmbigElt>2</AmbigElt>
                <B>3</B>
              </ex:errUnparsing>
            </tdml:dfdlInfoset>
          </tdml:infoset>
          <tdml:warnings>
            <tdml:warning>This will not be found</tdml:warning>
          </tdml:warnings>
        </tdml:unparserTestCase>
      </tdml:testSuite>

    lazy val ts = new DFDLTestSuite(testSuite)
    val e = intercept[Exception] {
      ts.runOneTest("unparserWarningWhenExpectingSuccess")
    }
    val msg = e.getMessage()
    System.err.println(msg)
    if (!msg.contains("This will not be found")) {
      println(msg)
      fail("TDML Warnings were not checked")
    }
  }

  @Test def testWarningsCheckedUnparserExpectingError() = {
    val testSuite =
      <tdml:testSuite suiteName="tdml warnings" xmlns:tdml="http://www.ibm.com/xmlns/dfdl/testData" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:ex="http://example.com" xmlns:fn="http://www.w3.org/2005/xpath-functions">
        <tdml:defineSchema name="causesWarnings" elementFormDefault="unqualified">
          <dfdl:format ref="ex:daffodilTest1" lengthKind="explicit"/>
          <xs:element name="errUnparsing" dfdl:lengthKind="implicit">
            <xs:complexType>
              <xs:sequence>
                <xs:element name="A" type="xs:string" dfdl:length="1"/>
                <xs:element name="AmbigElt" type="xs:string" dfdl:length="1"/>
                <xs:element name="B" type="xs:string" dfdl:length="1"/>
                <xs:element name="AmbigElt" type="xs:string" dfdl:length="1"/>
                <xs:element name="UnparseFails" type="xs:string" dfdl:length="1" dfdl:outputValueCalc="{ ../AmbigElt }"/>
              </xs:sequence>
            </xs:complexType>
          </xs:element>
        </tdml:defineSchema>
        <tdml:unparserTestCase name="unparserWarningWhenExpectingError" root="errUnparsing" model="causesWarnings" roundTrip="false">
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <ex:errUnparsing>
                <A>1</A>
                <AmbigElt>2</AmbigElt>
                <B>3</B>
                <AmbigElt>4</AmbigElt>
              </ex:errUnparsing>
            </tdml:dfdlInfoset>
          </tdml:infoset>
          <tdml:document><![CDATA[1234]]></tdml:document>
          <tdml:errors>
            <tdml:error>Schema Definition Error</tdml:error>
            <tdml:error>query-style</tdml:error>
            <tdml:error>AmbigElt</tdml:error>
          </tdml:errors>
          <tdml:warnings>
            <tdml:warning>This will not be found</tdml:warning>
          </tdml:warnings>
        </tdml:unparserTestCase>
      </tdml:testSuite>

    lazy val ts = new DFDLTestSuite(testSuite)
    val e = intercept[Exception] {
      ts.runOneTest("unparserWarningWhenExpectingError")
    }
    val msg = e.getMessage()
    System.err.println(msg)
    if (!msg.contains("This will not be found")) {
      println(msg)
      fail("TDML Warnings were not checked")
    }
  }
}
