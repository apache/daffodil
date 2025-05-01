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

import org.apache.daffodil.core.compiler._
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.dsom._

import org.junit.Assert._
import org.junit.Test

class TestIsScannable {

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testIsScannableAllText1(): Unit = {
    val sc = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="utf-8"/>,
      <xs:element name="list">
        <xs:complexType>
          <xs:sequence dfdl:separator="">
            <xs:sequence dfdl:hiddenGroupRef="ex:g"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="w" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
      <xs:group name="g">
        <xs:choice>
          <xs:element ref="ex:w"/>
          <xs:element name="x" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:choice>
      </xs:group>
    )

    val sset = Compiler().compileNode(sc).sset

    val Seq(schema) = sset.schemas
    val schemaDoc = schema.schemaDocuments.head
    val list = schemaDoc.globalElementDecls.head.asRoot
    assertTrue(list.isScannable)
    val Seq(child) = list.termChildren
    assertTrue(child.isScannable)
    assertEquals(NamedEncoding("UTF-8"), child.summaryEncoding)
  }

  @Test def testIsScannableHasBinary1(): Unit = {
    val sc = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="list">
        <xs:complexType>
          <xs:sequence dfdl:separator="">
            <xs:sequence dfdl:hiddenGroupRef="ex:g"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="w" dfdl:representation="binary" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
      <xs:group name="g">
        <xs:choice>
          <xs:element ref="ex:w"/>
          <xs:element name="x" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:choice>
      </xs:group>
    )

    val sset = Compiler().compileNode(sc).sset

    val Seq(schema) = sset.schemas
    val schemaDoc = schema.schemaDocuments.head
    val list = schemaDoc.globalElementDecls.head.asRoot
    assertFalse(list.isScannable)
    val Seq(child) = list.termChildren
    assertFalse(child.isScannable)
    val Seq(s1) = child.termChildren
    assertFalse(s1.isScannable)
    val Seq(e1, e2) = s1.termChildren
    assertEquals(Binary, e1.summaryEncoding)
    assertEquals(NamedEncoding("US-ASCII"), e2.summaryEncoding)
  }

  @Test def testIsScannableDifferentEncodings1(): Unit = {
    val sc = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="utf-8"/>,
      <xs:element name="list">
        <xs:complexType>
          <xs:sequence dfdl:separator="">
            <xs:sequence dfdl:hiddenGroupRef="ex:g"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="w" dfdl:encoding="ascii" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
      <xs:group name="g">
        <xs:choice>
          <xs:element ref="ex:w"/>
          <xs:element name="x" type="xs:int" dfdl:length="1" dfdl:lengthKind="explicit"/>
        </xs:choice>
      </xs:group>
    )

    val sset = Compiler().compileNode(sc).sset

    val Seq(schema) = sset.schemas
    val schemaDoc = schema.schemaDocuments.head
    val list = schemaDoc.globalElementDecls.head.asRoot
    assertFalse(list.isScannable)
    val Seq(child) = list.termChildren
    assertFalse(child.isScannable)
    val Seq(s1) = child.termChildren
    assertFalse(s1.isScannable)
    val Seq(e1, e2) = s1.termChildren
    assertEquals(NamedEncoding("US-ASCII"), e1.summaryEncoding)
    assertEquals(NamedEncoding("UTF-8"), e2.summaryEncoding)
    assertTrue(e1.isScannable)
    assertTrue(e2.isScannable)
  }

}
