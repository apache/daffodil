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

package org.apache.daffodil.core.runtime1

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Test

class TestUnparseHidden {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  val schema1 =
    <xs:element name="r" dfdl:lengthKind="implicit">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="a" type="xs:string" dfdl:length="3"/>
          <xs:choice>
            <xs:sequence>
              <xs:sequence dfdl:hiddenGroupRef="ex:presentBit"/>
              <xs:element name="b" type="xs:string" dfdl:length="3"/>
            </xs:sequence>
            <xs:sequence dfdl:hiddenGroupRef="ex:absentBit"/>
          </xs:choice>
          <xs:element name="c" type="xs:string" dfdl:length="3"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
    <xs:group name="presentBit">
      <xs:sequence>
        <xs:element name="flagBit" type="xs:int" dfdl:length="1" dfdl:outputValueCalc="{ 1 }"/>
      </xs:sequence>
    </xs:group>
    <xs:group name="absentBit">
      <xs:sequence>
        <xs:element name="flagBit" type="xs:int" dfdl:length="1" dfdl:outputValueCalc="{ 0 }"/>
      </xs:sequence>
    </xs:group>

  @Test def testUnparseHiddenGroupsPresenceFlags1(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind="explicit"/>,
      schema1,
      elementFormDefault = "unqualified"
    )

    val infoset =
      <ex:r xmlns:ex={example}>
        <a>abc</a>
        <b>def</b>
        <c>ghi</c>
      </ex:r>
    val data = "abc1defghi"
    TestUtils.testUnparsing(testSchema, infoset, data)
  }

  @Test def testUnparseHiddenGroupsPresenceFlags2(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind="explicit"/>,
      schema1,
      elementFormDefault = "unqualified"
    )

    val infoset =
      <ex:r xmlns:ex={example}>
        <a>abc</a>
        <c>ghi</c>
      </ex:r>
    val data = "abc0ghi"
    TestUtils.testUnparsing(testSchema, infoset, data)
  }

  val schema2 =
    <xs:element name="r" dfdl:lengthKind="implicit">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="a" type="xs:string" dfdl:length="3"/>
          <xs:choice>
            <xs:sequence>
              <xs:sequence dfdl:hiddenGroupRef="ex:presentBit"/>
              <xs:element name="b" type="xs:string" dfdl:length="3"/>
            </xs:sequence>
            <xs:sequence dfdl:hiddenGroupRef="ex:absentBit"/>
          </xs:choice>
          <xs:choice>
            <xs:sequence>
              <xs:sequence dfdl:hiddenGroupRef="ex:presentBit"/>
              <xs:element name="c" type="xs:string" dfdl:length="3"/>
            </xs:sequence>
            <xs:sequence dfdl:hiddenGroupRef="ex:absentBit"/>
          </xs:choice>
          <xs:element name="d" type="xs:string" dfdl:length="3"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
    <xs:group name="presentBit">
      <xs:sequence>
        <xs:element name="flagBit" type="xs:int" dfdl:length="1" dfdl:outputValueCalc="{ 1 }"/>
      </xs:sequence>
    </xs:group>
    <xs:group name="absentBit">
      <xs:sequence>
        <xs:element name="flagBit" type="xs:int" dfdl:length="1" dfdl:outputValueCalc="{ 0 }"/>
      </xs:sequence>
    </xs:group>

  @Test def testUnparseHiddenGroupsPresenceFlags3(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind="explicit"/>,
      schema2,
      elementFormDefault = "unqualified"
    )

    val infoset =
      <ex:r xmlns:ex={example}>
        <a>abc</a>
        <b>def</b>
        <c>ghi</c>
        <d>jkl</d>
      </ex:r>
    val data = "abc1def1ghijkl"
    TestUtils.testUnparsing(testSchema, infoset, data)
  }

  @Test def testUnparseHiddenGroupsPresenceFlags4(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind="explicit"/>,
      schema2,
      elementFormDefault = "unqualified"
    )

    val infoset =
      <ex:r xmlns:ex={example}>
        <a>abc</a>
        <d>jkl</d>
      </ex:r>
    val data = "abc00jkl"
    TestUtils.testUnparsing(testSchema, infoset, data)
  }

  val schema3 =
    <xs:element name="r" dfdl:lengthKind="implicit">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="a" type="xs:string" dfdl:length="3"/>
          <xs:choice>
            <xs:sequence dfdl:hiddenGroupRef="ex:absentBit"/>
            <xs:sequence>
              <xs:sequence dfdl:hiddenGroupRef="ex:presentBit"/>
              <xs:element name="b" type="xs:string" dfdl:length="1" dfdl:outputValueCalc="{ dfdl:valueLength(../d, 'bytes') }"/>
            </xs:sequence>
          </xs:choice>
          <xs:group ref="ex:arr"/>
          <xs:element name="d" type="xs:string" dfdl:length="3"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
    <xs:group name="arr">
      <xs:sequence>
        <xs:element name="arr" type="xs:string" minOccurs="0" dfdl:length="1" dfdl:occursCountKind="expression" dfdl:occursCount="{ dfdl:valueLength(../a, 'bytes') - 2 }"/>
      </xs:sequence>
    </xs:group>
    <xs:group name="presentBit">
      <xs:sequence>
        <xs:element name="flagBit" type="xs:int" dfdl:length="1" dfdl:outputValueCalc="{ 1 }"/>
      </xs:sequence>
    </xs:group>
    <xs:group name="absentBit">
      <xs:sequence>
        <xs:element name="flagBit" type="xs:int" dfdl:length="1" dfdl:outputValueCalc="{ 0 }"/>
      </xs:sequence>
    </xs:group>

  /**
   * In this test note that the schema above has a choice where both branches can have no representation in the
   * Infoset for unparsing. The infoset may or may not contain an element 'b' because that has dfdl:outputValueCalc
   * so that element, if present, will be used to select the choice branch, but if absent, the other branch will be
   * used.
   */
  @Test def testUnparseHiddenGroupsPresenceFlags5a(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind="explicit"/>,
      schema3,
      elementFormDefault = "unqualified"
    )

    val infoset =
      <ex:r xmlns:ex={example}>
        <a>abc</a>
        <arr>X</arr>
        <d>jkl</d>
      </ex:r>
    val data = "abc0Xjkl" // the '0' is the absent flag indicating there is no 'b' element.
    TestUtils.testUnparsing(testSchema, infoset, data)
  }

  /**
   * In this test the 'b' element which has dfdl:outputValueCalc is found in the infoset, and so guides the
   * selection of the choice branch. However its value is ignored and recomputed.
   */
  @Test def testUnparseHiddenGroupsPresenceFlags5b(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind="explicit"/>,
      schema3,
      elementFormDefault = "unqualified"
    )

    val infoset =
      <ex:r xmlns:ex={example}>
        <a>abc</a>
        <b>42</b> <!-- value is ignored and recomputed as 3 -->
        <arr>X</arr>
        <d>jkl</d>
      </ex:r>
    val data =
      "abc13Xjkl" // 1 is the present flag indicating 'b' is present. 3 is the value of b as computed.
    TestUtils.testUnparsing(testSchema, infoset, data)
  }

  val schemaX =
    <xs:element name="x" dfdl:lengthKind="implicit">
      <xs:complexType>
        <xs:sequence>
          <xs:choice>
            <xs:sequence>
              <xs:sequence dfdl:hiddenGroupRef="tns:present"/>
              <xs:element maxOccurs="2" name="b" type="xs:int" dfdl:length="1"/>
            </xs:sequence>
            <xs:sequence dfdl:hiddenGroupRef="tns:absent"/>
          </xs:choice>
          <xs:element name="y" minOccurs="0" type="xs:int" dfdl:length="1"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
    <xs:group name="present">
      <xs:sequence>
        <xs:element name="present" type="xs:int" dfdl:length="1" dfdl:outputValueCalc="{ 1 }">
          <xs:annotation>
            <xs:appinfo source="http://www.ogf.org/dfdl/">
              <dfdl:discriminator test="{ . eq 1 }"/>
            </xs:appinfo>
          </xs:annotation>
        </xs:element>
      </xs:sequence>
    </xs:group>
    <xs:group name="absent">
      <xs:sequence>
        <xs:element name="absent" type="xs:int" dfdl:length="1" dfdl:outputValueCalc="{ 0 }"/>
      </xs:sequence>
    </xs:group>

  @Test def testUnparseHiddenGroupsPresenceFlags6(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind="explicit"/>,
      schemaX,
      elementFormDefault = "unqualified"
    )

    val infoset =
      <ex:x xmlns:ex={example}>
        <b>3</b>
        <y>1</y>
      </ex:x>
    val data = "131"
    TestUtils.testUnparsing(testSchema, infoset, data)
  }
}
