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

package org.apache.daffodil.core.dsom

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.util._
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Test

class TestDsomCompilerUnparse1 {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testUnparse1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:e1 xmlns:ex={example}><s1>1</s1><s2>2</s2></ex:e1>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "12", areTracing)
  }

  /**
   * Test emphasis on delimiter unparsers
   */
  @Test def testUnparse2(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes" outputNewLine="%CR;%LF;"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:initiator="[" dfdl:separator="," dfdl:terminator="]">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:e1 xmlns:ex={example}><s1>1</s1><s2>2</s2></ex:e1>
    TestUtils.testUnparsing(sch, infoset, "[1,2]")
  }

  /**
   * Test emphasis on StringDelimitedUnparser
   */
  @Test def testUnparse3(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes" outputNewLine="%CR;%LF;"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:initiator="[" dfdl:separator="," dfdl:terminator="]">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:e1 xmlns:ex={example}><s1>1</s1><s2>2</s2></ex:e1>
    TestUtils.testUnparsing(sch, infoset, "[1,2]")
  }

  /**
   * Test emphasis on StringDelimitedUnparser w/ padding
   */
  @Test def testUnparse4(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes" outputNewLine="%CR;%LF;" truncateSpecifiedLengthString="no"/>,
      <xs:element name="e1" dfdl:lengthKind="implicit" dfdl:initiator="!#" dfdl:terminator="#!">
        <xs:complexType>
          <xs:sequence dfdl:separator=",">
            <xs:element name="s1" type="st" dfdl:lengthKind="implicit" dfdl:textStringPadCharacter="_" dfdl:textPadKind="padChar" dfdl:textStringJustification="left"/>
            <xs:element name="s2" type="st" dfdl:lengthKind="implicit" dfdl:textStringPadCharacter="_" dfdl:textPadKind="padChar" dfdl:textStringJustification="right"/>
            <xs:element name="s3" type="st" dfdl:lengthKind="implicit" dfdl:textStringPadCharacter="_" dfdl:textPadKind="padChar" dfdl:textStringJustification="center"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:simpleType name="st">
        <xs:restriction base="xs:string">
          <xs:maxLength value="3"/>
          <xs:minLength value="3"/>
        </xs:restriction>
      </xs:simpleType>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:e1 xmlns:ex={example}><s1>1</s1><s2>2</s2><s3>3</s3></ex:e1>
    TestUtils.testUnparsing(sch, infoset, "!#1__,__2,_3_#!")
  }

  /**
   * Test emphasis on StringDelimitedUnparser w/ escape scheme
   */
  @Test def testUnparse5(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes" outputNewLine="%CR;%LF;"/>
      <dfdl:defineEscapeScheme name="pound">
        <dfdl:escapeScheme escapeCharacter='#' escapeKind="escapeCharacter" escapeEscapeCharacter="" extraEscapedCharacters="" generateEscapeBlock="whenNeeded"/>
      </dfdl:defineEscapeScheme>
      <dfdl:defineEscapeScheme name='cStyleComment'>
        <dfdl:escapeScheme escapeBlockStart='/*' escapeBlockEnd='*/' escapeKind="escapeBlock" escapeEscapeCharacter="#" generateEscapeBlock="whenNeeded" extraEscapedCharacters=""/>
      </dfdl:defineEscapeScheme>,
      <xs:element name="e1" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator=",">
            <xs:element name="s1" type="xs:string" dfdl:lengthKind="delimited" dfdl:escapeSchemeRef="pound"/>
            <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited" dfdl:escapeSchemeRef="cStyleComment"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:e1 xmlns:ex={example}><s1>one, two</s1><s2>, three and four*/</s2></ex:e1>
    val areTracing = false
    TestUtils.testUnparsing(sch, infoset, "one#, two,/*, three and four#*/*/", areTracing)
  }

}
