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
import junit.framework.Assert._
import org.junit.Test
import org.apache.daffodil.Implicits._

class TestTDMLRoundTrips {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example
  val fn = XMLUtils.XPATH_FUNCTION_NAMESPACE

  @Test def testOnePassPass1() {

    val testSuite = <ts:testSuite xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:ex={ example } xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:defineSchema name="s">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="ex:GeneralFormat"/>
                        <xs:element name="r" dfdl:lengthKind="implicit">
                          <xs:complexType>
                            <xs:sequence dfdl:separator=",">
                              <xs:element name="foo" type="xs:string" dfdl:lengthKind="delimited"/>
                              <xs:element name="bar" type="xs:string" dfdl:lengthKind="delimited"/>
                            </xs:sequence>
                          </xs:complexType>
                        </xs:element>
                      </ts:defineSchema>
                      <ts:parserTestCase ID="test1" name="test1" root="r" model="s" roundTrip="onePass">
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <ex:r>
                              <foo>foo</foo>
                              <bar>bar</bar>
                            </ex:r>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                        <ts:document>foo,bar</ts:document>
                      </ts:parserTestCase>
                    </ts:testSuite>
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("test1")
  }

  /**
   * A test defined to show that we need two-pass, because the canonical separator
   * is a ";", but "," is also accepted and that's what is in the original data.
   *
   * The output from the one pass unparse will have the semicolon, and so will fail
   * to match the original input.
   */
  @Test def testOnePassFail1() {

    val testSuite = <ts:testSuite xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:ex={ example } xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:defineSchema name="s">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="ex:GeneralFormat"/>
                        <xs:element name="r" dfdl:lengthKind="implicit">
                          <xs:complexType>
                            <xs:sequence dfdl:separator="; ,">
                              <xs:element name="foo" type="xs:string" dfdl:lengthKind="delimited"/>
                              <xs:element name="bar" type="xs:string" dfdl:lengthKind="delimited"/>
                            </xs:sequence>
                          </xs:complexType>
                        </xs:element>
                      </ts:defineSchema>
                      <ts:parserTestCase ID="test1" name="test1" root="r" model="s" roundTrip="onePass">
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <ex:r>
                              <foo>foo</foo>
                              <bar>bar</bar>
                            </ex:r>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                        <ts:document>foo,bar</ts:document>
                      </ts:parserTestCase>
                    </ts:testSuite>
    lazy val ts = new DFDLTestSuite(testSuite)
    val e = intercept[TDMLException] {
      ts.runOneTest("test1")
    }
    val m = e.getMessage()
    assertTrue(m.toLowerCase.contains("unparsed data differs"))
  }

  /**
   * A test defined to show that we need two-passes, so that we re-parse
   * the canonical data from the unparse and then get a matching infoset.
   */
  @Test def testTwoPass1() {

    val testSuite = <ts:testSuite xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:ex={ example } xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:defineSchema name="s">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="ex:GeneralFormat"/>
                        <xs:element name="r" dfdl:lengthKind="implicit">
                          <xs:complexType>
                            <xs:sequence dfdl:separator="; ,">
                              <xs:element name="foo" type="xs:string" dfdl:lengthKind="delimited"/>
                              <xs:element name="bar" type="xs:string" dfdl:lengthKind="delimited"/>
                            </xs:sequence>
                          </xs:complexType>
                        </xs:element>
                      </ts:defineSchema>
                      <ts:parserTestCase ID="test1" name="test1" root="r" model="s" roundTrip="twoPass">
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <ex:r>
                              <foo>foo</foo>
                              <bar>bar</bar>
                            </ex:r>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                        <ts:document>foo,bar</ts:document>
                      </ts:parserTestCase>
                    </ts:testSuite>
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("test1")
  }

  /**
   * A test defined to show that when we say we need two passes, if the
   * unparse data in fact matches the original (i.e., we didn't really need two passes)
   * that is detected and reported.
   *
   * This means test authors have to know if the test is parse-only, one pass parse/unparse round trip,
   * or two-pass, and label the test accordingly.
   */
  @Test def testTwoPassNotNeeded1() {

    val testSuite = <ts:testSuite xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:ex={ example } xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:defineSchema name="s">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="ex:GeneralFormat"/>
                        <xs:element name="r" dfdl:lengthKind="implicit">
                          <xs:complexType>
                            <xs:sequence dfdl:separator=",">
                              <xs:element name="foo" type="xs:string" dfdl:lengthKind="delimited"/>
                              <xs:element name="bar" type="xs:string" dfdl:lengthKind="delimited"/>
                            </xs:sequence>
                          </xs:complexType>
                        </xs:element>
                      </ts:defineSchema>
                      <ts:parserTestCase ID="test1" name="test1" root="r" model="s" roundTrip="twoPass">
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <ex:r>
                              <foo>foo</foo>
                              <bar>bar</bar>
                            </ex:r>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                        <ts:document>foo,bar</ts:document>
                      </ts:parserTestCase>
                    </ts:testSuite>
    lazy val ts = new DFDLTestSuite(testSuite)
    val e = intercept[TDMLException] {
      ts.runOneTest("test1")
    }
    val m = e.getMessage()
    assertTrue(m.toLowerCase.contains("should this really be a two-pass test"))
  }

  /**
   * A test defined to show that we need three-passes, so that we re-parse
   * the canonical data from the unparse and then still do not get a matching infoset,
   * but unparsing that infoset finally does give us matching unparsed data.
   */
  /*
  @Test def testThreePass1() {
    val testSuite = <ts:testSuite xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:xsi={ xsi } xmlns:fn={ fn } xmlns:ex={ example } xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:defineSchema name="s" elementFormDefault="unqualified">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="ex:GeneralFormat"/>
                        <xs:element name="r" dfdl:lengthKind="implicit">
                          <xs:complexType>
                            <xs:sequence dfdl:separator=",">
                              <xs:element name="foo" nillable="true" type="xs:string" dfdl:lengthKind="delimited" dfdl:useNilForDefault="yes" dfdl:nilKind="literalValue" dfdl:nilValue="%ES; nil" dfdl:outputValueCalc="{ ../bar }"/>
                              <xs:element name="bar" type="xs:string" dfdl:inputValueCalc="{ if (fn:nilled(../foo)) then '' else 'foo' }"/>
                            </xs:sequence>
                          </xs:complexType>
                        </xs:element>
                      </ts:defineSchema>
                      <ts:parserTestCase ID="test1" name="test1" root="r" model="s" roundTrip="threePass">
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <ex:r>
                              <foo>foo</foo>
                              <bar/>
                            </ex:r>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                        <ts:document></ts:document>
                      </ts:parserTestCase>
                    </ts:testSuite>
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("test1")
  }
  */
}
