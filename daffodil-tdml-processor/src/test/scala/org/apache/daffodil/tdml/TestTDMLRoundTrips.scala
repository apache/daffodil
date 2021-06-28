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
import org.junit.Assert._
import org.junit.Test
import org.apache.daffodil.Implicits._

class TestTDMLRoundTrips {

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val daf = XMLUtils.EXT_NS_APACHE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example
  val fn = XMLUtils.XPATH_FUNCTION_NAMESPACE

  @Test def testOnePassPass1(): Unit = {

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
    val runner = new Runner(testSuite)
    runner.runOneTest("test1")
    runner.reset
  }

  /**
   * A test defined to show that we need two-pass, because the canonical separator
   * is a ";", but "," is also accepted and that's what is in the original data.
   *
   * The output from the one pass unparse will have the semicolon, and so will fail
   * to match the original input.
   */
  @Test def testOnePassFail1(): Unit = {

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
    val runner = new Runner(testSuite)
    val e = intercept[TDMLException] {
      runner.runOneTest("test1")
    }
    runner.reset
    val m = e.getMessage()
    assertTrue(m.toLowerCase.contains("unparsed data differs"))
  }

  def needsTwoPassesOnlyTDML(passesEnum: String) =
    <ts:testSuite xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:ex={ example } xmlns:ts={ tdml } suiteName="theSuiteName">
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
      <ts:parserTestCase ID="test1" name="test1" root="r" model="s" roundTrip={ passesEnum }>
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

  /**
   * A test defined to show that we need two-passes, so that we re-parse
   * the canonical data from the unparse and then get a matching infoset.
   */
  @Test def testTwoPass1(): Unit = {

    val testSuite = needsTwoPassesOnlyTDML("twoPass")
    val runner = new Runner(testSuite)
    runner.runOneTest("test1")
    runner.reset
  }
  /**
   * Tests windows style (CRLF) line endings for two pass
   */
  @Test def testTwoPass2(): Unit = {

    val testSuite =
      <ts:testSuite xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:ex={ example } xmlns:ts={ tdml } suiteName="theSuiteName">
        <ts:defineSchema name="s">
          <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="ex:GeneralFormat"/>
          <xs:element name="all">
            <xs:complexType>
              <xs:sequence dfdl:separator="//%NL;">
                <xs:element name="r" dfdl:lengthKind="implicit" dfdl:occursCountKind="parsed" maxOccurs="unbounded">
                  <xs:complexType>
                    <xs:sequence dfdl:separator="; ,">
                      <xs:element name="foo" type="xs:string" dfdl:lengthKind="delimited"/>
                      <xs:element name="bar" type="xs:string" dfdl:lengthKind="delimited"/>
                    </xs:sequence>
                  </xs:complexType>
                </xs:element>
              </xs:sequence>
            </xs:complexType>
          </xs:element>
        </ts:defineSchema>
        <ts:parserTestCase ID="test1" name="test1" root="all" model="s" roundTrip="twoPass">
          <ts:document>
            <ts:documentPart type="text" replaceDFDLEntities="true"><![CDATA[Text,One%CR;%LF;New Line//%CR;%LF;Text following,Two%CR;%LF;New Line//%CR;%LF;]]></ts:documentPart>
          </ts:document>
          <ts:infoset>
            <ts:dfdlInfoset>
              <ex:all>
                <ex:r>
                  <foo>Text</foo>
                  <bar><![CDATA[One
New Line]]></bar>
                </ex:r>
                <ex:r>
                  <foo>Text following</foo>
                  <bar><![CDATA[Two
New Line]]></bar>
                </ex:r>
              </ex:all>
            </ts:dfdlInfoset>
          </ts:infoset>
        </ts:parserTestCase>
      </ts:testSuite>
    val runner = new Runner(testSuite)
    runner.runOneTest("test1")
    runner.reset
  }

  /**
   * Tests mac style (CR) line endings for two pass
   */
  @Test def testTwoPass3(): Unit = {

    val testSuite = <ts:testSuite xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:ex={ example } xmlns:ts={ tdml } suiteName="theSuiteName">
                      <ts:defineSchema name="s">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="ex:GeneralFormat"/>
                        <xs:element name="all">
                          <xs:complexType>
                            <xs:sequence dfdl:separator="//%NL;">
                              <xs:element name="r" dfdl:occursCountKind="parsed" minOccurs="0" maxOccurs="unbounded">
                                <xs:complexType>
                                  <xs:sequence dfdl:separator="; ,">
                                    <xs:element name="foo" type="xs:string" dfdl:lengthKind="delimited"/>
                                    <xs:element name="bar" type="xs:string" dfdl:lengthKind="delimited"/>
                                  </xs:sequence>
                                </xs:complexType>
                              </xs:element>
                            </xs:sequence>
                          </xs:complexType>
                        </xs:element>
                      </ts:defineSchema>
                      <ts:parserTestCase ID="test1" name="test1" root="all" model="s" roundTrip="twoPass">
                        <ts:document>
                          <ts:documentPart type="text" replaceDFDLEntities="true"><![CDATA[Text,One%CR;New Line//%CR;%LF;Text following,Two%CR;New Line//%CR;%LF;]]></ts:documentPart>
                        </ts:document>
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <ex:all>
                              <ex:r>
                                <foo>Text</foo>
                                <bar><![CDATA[One
New Line]]></bar>
                              </ex:r>
                              <ex:r>
                                <foo>Text following</foo>
                                <bar><![CDATA[Two
New Line]]></bar>
                              </ex:r>
                            </ex:all>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val runner = new Runner(testSuite)
    runner.runOneTest("test1")
    runner.reset
  }

  def nPassNotNeededTDML(passesEnum: String) =
    <ts:testSuite xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:ex={ example } xmlns:ts={ tdml } suiteName="theSuiteName">
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
      <ts:parserTestCase ID="test1" name="test1" root="r" model="s" roundTrip={ passesEnum }>
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
  /**
   * A test defined to show that when we say we need two passes, if the
   * unparse data in fact matches the original (i.e., we didn't really need two passes)
   * that is detected and reported.
   *
   * This means test authors have to know if the test is parse-only, one pass parse/unparse round trip,
   * or two-pass, and label the test accordingly.
   */
  @Test def testTwoPassNotNeeded1(): Unit = {
    val testSuite = nPassNotNeededTDML("twoPass")
    val runner = new Runner(testSuite)
    val e = intercept[TDMLException] {
      runner.runOneTest("test1")
    }
    runner.reset
    val m = e.getMessage()
    assertTrue(m.toLowerCase.contains("should this really be a twopass test"))
  }

  /**
   * A test defined to show that we need three-passes, so that we re-parse
   * the canonical data from the unparse and then still do not get a matching infoset,
   * but unparsing that infoset finally does give us matching unparsed data.
   */
  @Test def testThreePass1(): Unit = {
    val testSuite = <ts:testSuite xmlns:dfdl={ dfdl } xmlns:xs={ xsd } xmlns:xsi={ xsi } xmlns:fn={ fn } xmlns:ex={ example } xmlns:ts={ tdml } xmlns:daf={ daf } suiteName="theSuiteName">
                      <ts:defineSchema name="s" elementFormDefault="unqualified">
                        <xs:include schemaLocation="org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="ex:GeneralFormat" lengthKind="delimited" separatorSuppressionPolicy="anyEmpty"/>
                        <xs:element name="r" dfdl:lengthKind="implicit">
                          <xs:complexType>
                            <xs:choice>
                              <xs:sequence dfdl:separator=",," dfdl:separatorPosition="postfix">
                                <xs:element name="bar" type="xs:string"/>
                              </xs:sequence>
                              <xs:sequence dfdl:separator="," dfdl:separatorPosition="postfix">
                                <xs:element name="foo" type="xs:string"/>
                                <xs:element name="bar" type="xs:string" nillable="true" dfdl:nilKind="literalValue" dfdl:nilValue="%ES; nil"/>
                              </xs:sequence>
                            </xs:choice>
                          </xs:complexType>
                        </xs:element>
                      </ts:defineSchema>
                      <ts:parserTestCase name="test1" root="r" model="s" roundTrip="threePass">
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <!--
                              In a three pass test the infoset given should be the
                              final steady-state infoset.
                              -->
                            <ex:r>
                              <bar>foo</bar>
                            </ex:r>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                        <ts:document>foo,nil,</ts:document>
                      </ts:parserTestCase>
                    </ts:testSuite>
    val runner = new Runner(testSuite)
    runner.runOneTest("test1")
    runner.reset
  }

  /**
   * A test defined to show that when we say we need three passes, if the
   * unparse data in fact matches the original (i.e., we didn't
   * even really need two passes, that it is detected and reported.
   */
  @Test def testThreePassNotNeeded1(): Unit = {
    val testSuite = nPassNotNeededTDML("threePass")
    val runner = new Runner(testSuite)
    val e = intercept[TDMLException] {
      runner.runOneTest("test1")
    }
    runner.reset
    val m = e.getMessage()
    assertTrue(m.toLowerCase.contains("should this really be a threepass test"))
  }

  /**
   * A test defined to show that when we say we need three passes, if the
   * reparsed unparse data in fact matches the original infoset (i.e., we didn't
   * even really need three passes, two would have been enough),
   * that it is detected and reported.
   */
  // Note: three pass tests no longer actually check that the first parse infoset
  // matches. There is a class of such tests where even though the unparsed data requires
  // three pass testing, the infoset is right from the very first parse.
  //
  // Forcing the original infoset to NOT match just makes those tests harder to write.
  // FYI: One such test is in PCAP.
  //
  //      ts.runOneTest("test1")
  //
  //    val testSuite = needsTwoPassesOnlyTDML("threePass")
  //    val runner = new Runner(testSuite)
  //    val e = intercept[TDMLException] {
  //      runner.runOneTest("test1")
  //    }
  //    runner.reset
  //    val m = e.getMessage()
  //    assertTrue(m.toLowerCase.contains("should this really be a threepass test"))
  //  }

}
