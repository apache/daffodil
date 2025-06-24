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
import org.apache.daffodil.runtime1.dpath.NodeInfo._
import org.apache.daffodil.runtime1.dsom._
import org.apache.daffodil.runtime1.infoset.DIDocument
import org.apache.daffodil.runtime1.infoset.DISimple
import org.apache.daffodil.runtime1.processors.parsers.PState

import org.junit.Assert._
import org.junit.Test

class TestSimpleTypeUnions {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  val testSchema1 = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="GeneralFormat"/>,
    <xs:simpleType name="int1Type">
      <xs:restriction base="xs:int">
        <xs:minInclusive value="1"/>
        <xs:maxInclusive value="1"/>
      </xs:restriction>
    </xs:simpleType>
    <xs:simpleType name="int2Type">
      <xs:restriction base="xs:int">
        <xs:minInclusive value="2"/>
        <xs:maxInclusive value="2"/>
      </xs:restriction>
    </xs:simpleType>
    <xs:simpleType name="oneOrTwo">
      <xs:union memberTypes="ex:int1Type ex:int2Type"/>
    </xs:simpleType>
    <xs:element name="e1" dfdl:lengthKind="delimited" type="ex:oneOrTwo">
      <xs:annotation>
        <xs:appinfo source="http://www.ogf.org/dfdl/">
          <!--
            this assert always passes, but uses e1 in an expression to prevent
            the InfosetWalker from freeing it, which allows the tests to
            inspect runtime internals
          -->
          <dfdl:assert test="{ fn:true() or /ex:e1 eq 0 }" />
        </xs:appinfo>
      </xs:annotation>
    </xs:element>
  )

  @Test def testUnion01(): Unit = {

    val sset = SchemaSet(testSchema1)
    val Seq(sch) = sset.schemas
    val sd = sch.schemaDocuments.head

    // Explore global element decl
    val Seq(e1) = sd.globalElementDecls
    val e1t = e1.simpleType
    val u = e1t.optUnion.get
    val e1tPrimType = u.primType
    assertEquals(PrimType.Int, e1tPrimType)
    assertTrue(e1t.optRestriction.isEmpty)
    val Seq(st1, st2) = u.unionMemberTypes
    st1.asInstanceOf[GlobalSimpleTypeDef].globalQName.toQNameString
    val st1n = st1.diagnosticDebugName
    assertEquals("int1Type", st1n)
    val st2n = st2.diagnosticDebugName
    assertEquals("int2Type", st2n)
    val st1rd = st1.simpleTypeRuntimeData
    val st2rd = st2.simpleTypeRuntimeData
    val st1mini = st1rd.minInclusive
    assertEquals(1, st1mini.get.intValue())
    assertTrue(st1rd.unionMemberTypes.isEmpty)
    assertTrue(st2rd.unionMemberTypes.isEmpty)
    val st2mini = st2rd.minInclusive
    assertEquals(2, st2mini.get.intValue())
    assertEquals("int1Type", st1rd.diagnosticDebugName)
    assertEquals("int2Type", st2rd.diagnosticDebugName)
  }

  @Test def testUnionFirstUnionMemberOk(): Unit = {
    val (result, actual) = TestUtils.testString(testSchema1, "1")
    val i = result.resultState
      .asInstanceOf[PState]
      .infoset
      .asInstanceOf[DIDocument]
      .child(0)
      .asInstanceOf[DISimple]
    val umstrd = i.unionMemberRuntimeData.get
    assertEquals("int1Type", umstrd.diagnosticDebugName)
    assertTrue(i.valid.get)
    val expected = <e1>1</e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testUnionSecondUnionMemberOk(): Unit = {
    val (result, actual) = TestUtils.testString(testSchema1, "2")
    val i = result.resultState
      .asInstanceOf[PState]
      .infoset
      .asInstanceOf[DIDocument]
      .child(0)
      .asInstanceOf[DISimple]
    val umstrd = i.unionMemberRuntimeData.get
    assertEquals("int2Type", umstrd.diagnosticDebugName)
    assertTrue(i.valid.get)
    val expected = <e1>2</e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testUnionNoUnionMemberOK(): Unit = {
    val (result, _) = TestUtils.testString(testSchema1, "3")
    val i = result.resultState
      .asInstanceOf[PState]
      .infoset
      .asInstanceOf[DIDocument]
      .child(0)
      .asInstanceOf[DISimple]
    val Some(dv: java.lang.Integer) = Some(i.dataValue.getInt)
    assertEquals(3, dv.intValue())
    assertTrue(i.unionMemberRuntimeData.isEmpty)
    assertFalse(i.valid.get)
    val ds = result.getDiagnostics
    assertTrue(ds.length == 1)
    val d = ds.head
    assertTrue(d.isInstanceOf[ValidationError])
    val msg = d.getMessage()
    def die() = { println(msg); fail() }
    if (!msg.contains("e1"))
      die()
    if (!msg.contains("union members"))
      die()
    if (!msg.contains("int1Type"))
      die()
    if (!msg.contains("int2Type"))
      die()
    if (!msg.contains("failed facet checks"))
      die()
  }

  val testSchema2 = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="GeneralFormat"/>,
    <xs:simpleType name="int19Type">
      <xs:restriction base="xs:int">
        <xs:minInclusive value="1"/>
        <xs:maxInclusive value="9"/>
      </xs:restriction>
    </xs:simpleType>
    <xs:simpleType name="int12Type">
      <xs:restriction base="ex:int19Type">
        <xs:minInclusive value="1"/>
        <xs:maxInclusive value="2"/>
      </xs:restriction>
    </xs:simpleType>
    <xs:simpleType name="int49Type">
      <xs:restriction base="ex:int19Type">
        <xs:minInclusive value="4"/>
        <xs:maxInclusive value="9"/>
      </xs:restriction>
    </xs:simpleType>
    <xs:simpleType name="int12or47Type">
      <xs:union memberTypes="ex:int12Type">
        <xs:simpleType>
          <xs:restriction base="ex:int49Type">
            <xs:maxInclusive value="7"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:union>
    </xs:simpleType>
    <xs:simpleType name="negIntType">
      <xs:restriction base="xs:int">
        <xs:maxInclusive value="-1"/>
      </xs:restriction>
    </xs:simpleType>
    <xs:simpleType name="eType">
      <xs:union>
        <xs:simpleType>
          <xs:restriction base="ex:int12or47Type">
          </xs:restriction>
        </xs:simpleType>
        <xs:simpleType>
          <xs:restriction base="ex:negIntType">
            <xs:minInclusive value="-8"/>
            <xs:maxInclusive value="-1"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:union>
    </xs:simpleType>
    <xs:element name="e1" dfdl:lengthKind="delimited" type="ex:eType">
      <xs:annotation>
        <xs:appinfo source="http://www.ogf.org/dfdl/">
          <!--
            this assert always passes, but uses e1 in an expression to prevent
            the InfosetWalker from freeing it, which allows the tests to
            inspect runtime internals
          -->
          <dfdl:assert test="{ fn:true() or /ex:e1 eq 0 }" />
        </xs:appinfo>
      </xs:annotation>
    </xs:element>
  )

  @Test def testUnionNot3(): Unit = {
    val (result, _) = TestUtils.testString(testSchema2, "3")
    val i = result.resultState
      .asInstanceOf[PState]
      .infoset
      .asInstanceOf[DIDocument]
      .child(0)
      .asInstanceOf[DISimple]
    val Some(dv: java.lang.Integer) = Some(i.dataValue.getInt)
    assertEquals(3, dv.intValue())
    assertTrue(i.unionMemberRuntimeData.isEmpty)
    assertFalse(i.valid.get)
    val ds = result.getDiagnostics
    assertTrue(ds.length == 1)
    val d = ds.head
    assertTrue(d.isInstanceOf[ValidationError])
    val msg = d.getMessage()
    def die() = { println(msg); fail() }
    if (!msg.contains("e1"))
      die()
    if (!msg.contains("union members"))
      die()
    if (!msg.contains("int12or47Type"))
      die()
    if (!msg.contains("negIntType"))
      die()
    if (!msg.contains("failed facet checks"))
      die()
  }

  @Test def testUnionNot3_01(): Unit = {
    val (result, actual) = TestUtils.testString(testSchema2, "1")
    val i = result.resultState
      .asInstanceOf[PState]
      .infoset
      .asInstanceOf[DIDocument]
      .child(0)
      .asInstanceOf[DISimple]
    val umstrd = i.unionMemberRuntimeData.get
    assertEquals("int12Type", umstrd.diagnosticDebugName)
    assertTrue(i.valid.get)
    val expected = <e1>1</e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testUnionNot3_02(): Unit = {
    val (result, actual) = TestUtils.testString(testSchema2, "2")
    val i = result.resultState
      .asInstanceOf[PState]
      .infoset
      .asInstanceOf[DIDocument]
      .child(0)
      .asInstanceOf[DISimple]
    val umstrd = i.unionMemberRuntimeData.get
    assertEquals("int12Type", umstrd.diagnosticDebugName)
    assertTrue(i.valid.get)
    val expected = <e1>2</e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testUnionNot3_03(): Unit = {
    val (result, actual) = TestUtils.testString(testSchema2, "-1")
    val i = result.resultState
      .asInstanceOf[PState]
      .infoset
      .asInstanceOf[DIDocument]
      .child(0)
      .asInstanceOf[DISimple]
    val umstrd = i.unionMemberRuntimeData.get
    assertEquals(
      "ex:negIntType",
      umstrd.diagnosticDebugName
    ) // anonymous simple type gets this name from base.
    assertTrue(i.valid.get)
    val expected = <e1>-1</e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  val testSchema3 = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="GeneralFormat"/>,
    <xs:simpleType name="fooXbar">
      <xs:restriction base="xs:string">
        <xs:pattern value="foo\w*bar"/>
      </xs:restriction>
    </xs:simpleType>
    <xs:simpleType name="foo1or2bar">
      <xs:restriction base="ex:fooXbar">
        <xs:enumeration value="foo1bar"/>
        <xs:enumeration value="foo2bar"/>
      </xs:restriction>
    </xs:simpleType>
    <xs:simpleType name="foo3or4bar">
      <xs:restriction base="ex:fooXbar">
        <xs:pattern value="foo[34]bar"/>
      </xs:restriction>
    </xs:simpleType>
    <xs:simpleType name="foo1or2or3bar">
      <xs:union memberTypes="ex:foo1or2bar">
        <xs:simpleType>
          <xs:restriction base="ex:foo3or4bar">
            <xs:pattern value="foo[356]bar"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:union>
    </xs:simpleType>
    <xs:simpleType name="foo3bar">
      <xs:restriction base="ex:foo1or2or3bar">
        <xs:pattern value="foo[1234]bar"/>
      </xs:restriction>
    </xs:simpleType>
    <xs:element name="e1" dfdl:lengthKind="delimited" type="ex:foo3bar">
      <xs:annotation>
        <xs:appinfo source="http://www.ogf.org/dfdl/">
          <!--
            this assert always passes, but uses e1 in an expression to prevent
            the InfosetWalker from freeing it, which allows the tests to
            inspect runtime internals
          -->
          <dfdl:assert test="{ fn:true() or /ex:e1 eq '' }" />
        </xs:appinfo>
      </xs:annotation>
    </xs:element>
  )

  @Test def testRestrictionOnUnion_01(): Unit = {
    val (result, actual) = TestUtils.testString(testSchema3, "foo3bar")
    val i = result.resultState
      .asInstanceOf[PState]
      .infoset
      .asInstanceOf[DIDocument]
      .child(0)
      .asInstanceOf[DISimple]
    val umstrd = i.unionMemberRuntimeData.get
    assertEquals("ex:foo3or4bar", umstrd.diagnosticDebugName)
    assertTrue(i.valid.get)
    val expected = <e1>foo3bar</e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testRestrictionOnUnion_02(): Unit = {
    val (result, actual) = TestUtils.testString(testSchema3, "foo1bar")
    val i = result.resultState
      .asInstanceOf[PState]
      .infoset
      .asInstanceOf[DIDocument]
      .child(0)
      .asInstanceOf[DISimple]
    val umstrd = i.unionMemberRuntimeData.get
    assertEquals("foo1or2bar", umstrd.diagnosticDebugName)
    assertTrue(i.valid.get)
    val expected = <e1>foo1bar</e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testRestrictionOnUnion_03(): Unit = {
    val (result, actual) = TestUtils.testString(testSchema3, "foo2bar")
    val i = result.resultState
      .asInstanceOf[PState]
      .infoset
      .asInstanceOf[DIDocument]
      .child(0)
      .asInstanceOf[DISimple]
    val umstrd = i.unionMemberRuntimeData.get
    assertEquals("foo1or2bar", umstrd.diagnosticDebugName)
    assertTrue(i.valid.get)
    val expected = <e1>foo2bar</e1>
    XMLUtils.compareAndReport(expected, actual)
  }

  @Test def testRestrictionOnUnionFail_01(): Unit = {
    val (result, _) = TestUtils.testString(testSchema3, "foo4bar")
    val i = result.resultState
      .asInstanceOf[PState]
      .infoset
      .asInstanceOf[DIDocument]
      .child(0)
      .asInstanceOf[DISimple]
    val Some(dv: String) = Some(i.dataValue.getString)
    assertEquals("foo4bar", dv)
    assertTrue(i.unionMemberRuntimeData.isEmpty)
    assertFalse(i.valid.get)
    val ds = result.getDiagnostics
    assertTrue(ds.length == 1)
    val d = ds.head
    assertTrue(d.isInstanceOf[ValidationError])
    val msg = d.getMessage()
    def die() = { println(msg); fail() }
    if (!msg.contains("e1"))
      die()
    if (!msg.contains("union members"))
      die()
    if (!msg.contains("foo1or2bar"))
      die()
    if (!msg.contains("foo3or4bar"))
      die()
    if (!msg.contains("failed facet checks"))
      die()
  }

  /**
   * This test shows that the union isn't even attempted if the
   * restriction on the union fails.
   */
  @Test def testRestrictionOnUnionFail_02(): Unit = {
    val (result, _) = TestUtils.testString(testSchema3, "notfoo1bar")
    val i = result.resultState
      .asInstanceOf[PState]
      .infoset
      .asInstanceOf[DIDocument]
      .child(0)
      .asInstanceOf[DISimple]
    val Some(dv: String) = Some(i.dataValue.getString)
    assertEquals("notfoo1bar", dv)
    assertTrue(i.unionMemberRuntimeData.isEmpty)
    assertFalse(i.valid.get)
    val ds = result.getDiagnostics
    assertTrue(ds.length == 1)
    val d = ds.head
    assertTrue(d.isInstanceOf[ValidationError])
    val msg = d.getMessage()
    def die() = { println(msg); fail() }
    if (!msg.contains("e1"))
      die()
    if (!msg.contains("facet pattern"))
      die()
    if (!msg.contains("foo[1234]bar"))
      die()
    if (!msg.contains("failed facet checks"))
      die()
  }
}
