/* Copyright (c) 2017 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.xml.XMLUtils
import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo._
import org.junit.Test
import edu.illinois.ncsa.daffodil.infoset.DISimple
import edu.illinois.ncsa.daffodil.infoset.DIDocument
import edu.illinois.ncsa.daffodil.processors.parsers.PState

class TestSimpleTypeUnions {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  val testSchema1 = SchemaUtils.dfdlTestSchema(
    <dfdl:format ref="daffodilTest1"/>,

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
    <xs:element name="e1" dfdl:lengthKind="delimited" type="ex:oneOrTwo"/>)

  @Test def testUnion01 {

    val sset = new SchemaSet(testSchema1)
    val Seq(sch) = sset.schemas
    val Seq(sd, _) = sch.schemaDocuments

    // Explore global element decl
    val Seq(e1f) = sd.globalElementDecls
    val e1 = e1f.forRoot()
    val e1t = e1.simpleType
    val u = e1t.optUnion.get
    val e1tPrimType = u.primType
    assertEquals(PrimType.Int, e1tPrimType)
    assertTrue(e1t.optRestriction.isEmpty)
    val Seq(st1, st2) = u.unionMemberTypes
    st1.asInstanceOf[GlobalSimpleTypeDef].globalQName.toQNameString
    val st1n = st1.diagnosticDebugName
    assertEquals("ex:int1Type", st1n)
    val st2n = st2.diagnosticDebugName
    assertEquals("ex:int2Type", st2n)
    val st1rd = st1.simpleTypeRuntimeData
    val st2rd = st2.simpleTypeRuntimeData
    val st1mini = st1rd.minInclusive
    assertEquals(1, st1mini.get.intValue())
    assertTrue(st1rd.unionMemberTypes.isEmpty)
    assertTrue(st2rd.unionMemberTypes.isEmpty)
    val st2mini = st2rd.minInclusive
    assertEquals(2, st2mini.get.intValue())
    assertEquals("ex:int1Type", st1rd.diagnosticDebugName)
    assertEquals("ex:int2Type", st2rd.diagnosticDebugName)
  }

  @Test def testUnionFirstUnionMemberOk {
    val (result, actual) = TestUtils.testString(testSchema1, "1")
    val i = result.resultState.asInstanceOf[PState].infoset.asInstanceOf[DIDocument].root.asInstanceOf[DISimple]
    val umstrd = i.unionMemberRuntimeData.get
    assertEquals("ex:int1Type", umstrd.diagnosticDebugName)
    assertTrue(i.valid.get)
    val expected = <e1>1</e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testUnionSecondUnionMemberOk {
    val (result, actual) = TestUtils.testString(testSchema1, "2")
    val i = result.resultState.asInstanceOf[PState].infoset.asInstanceOf[DIDocument].root.asInstanceOf[DISimple]
    val umstrd = i.unionMemberRuntimeData.get
    assertEquals("ex:int2Type", umstrd.diagnosticDebugName)
    assertTrue(i.valid.get)
    val expected = <e1>2</e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testUnionNoUnionMemberOK {
    val (result, _) = TestUtils.testString(testSchema1, "3")
    val i = result.resultState.asInstanceOf[PState].infoset.asInstanceOf[DIDocument].root.asInstanceOf[DISimple]
    val Some(dv: java.lang.Integer) = Some(i.dataValue)
    assertEquals(3, dv.intValue())
    assertTrue(i.unionMemberRuntimeData.isEmpty)
    assertFalse(i.valid.get)
    val ds = result.getDiagnostics
    assertTrue(ds.length == 1)
    val d = ds.head
    assertTrue(d.isInstanceOf[ValidationError])
    val msg = d.getMessage()
    def die = { println(msg); fail() }
    if (!msg.contains("ex:e1"))
      die
    if (!msg.contains("union members"))
      die
    if (!msg.contains("ex:int1Type"))
      die
    if (!msg.contains("ex:int2Type"))
      die
    if (!msg.contains("failed facet checks"))
      die
  }

  val testSchema2 = SchemaUtils.dfdlTestSchema(
    <dfdl:format ref="daffodilTest1"/>,

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
    <xs:element name="e1" dfdl:lengthKind="delimited" type="ex:eType"/>)

  @Test def testUnionNot3 {
    val (result, _) = TestUtils.testString(testSchema2, "3")
    val i = result.resultState.asInstanceOf[PState].infoset.asInstanceOf[DIDocument].root.asInstanceOf[DISimple]
    val Some(dv: java.lang.Integer) = Some(i.dataValue)
    assertEquals(3, dv.intValue())
    assertTrue(i.unionMemberRuntimeData.isEmpty)
    assertFalse(i.valid.get)
    val ds = result.getDiagnostics
    assertTrue(ds.length == 1)
    val d = ds.head
    assertTrue(d.isInstanceOf[ValidationError])
    val msg = d.getMessage()
    def die = { println(msg); fail() }
    if (!msg.contains("ex:e1"))
      die
    if (!msg.contains("union members"))
      die
    if (!msg.contains("ex:int12or47Type"))
      die
    if (!msg.contains("ex:negIntType"))
      die
    if (!msg.contains("failed facet checks"))
      die
  }

  @Test def testUnionNot3_01 {
    val (result, actual) = TestUtils.testString(testSchema2, "1")
    val i = result.resultState.asInstanceOf[PState].infoset.asInstanceOf[DIDocument].root.asInstanceOf[DISimple]
    val umstrd = i.unionMemberRuntimeData.get
    assertEquals("ex:int12Type", umstrd.diagnosticDebugName)
    assertTrue(i.valid.get)
    val expected = <e1>1</e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testUnionNot3_02 {
    val (result, actual) = TestUtils.testString(testSchema2, "2")
    val i = result.resultState.asInstanceOf[PState].infoset.asInstanceOf[DIDocument].root.asInstanceOf[DISimple]
    val umstrd = i.unionMemberRuntimeData.get
    assertEquals("ex:int12Type", umstrd.diagnosticDebugName)
    assertTrue(i.valid.get)
    val expected = <e1>2</e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testUnionNot3_03 {
    val (result, actual) = TestUtils.testString(testSchema2, "-1")
    val i = result.resultState.asInstanceOf[PState].infoset.asInstanceOf[DIDocument].root.asInstanceOf[DISimple]
    val umstrd = i.unionMemberRuntimeData.get
    assertEquals("ex:negIntType", umstrd.diagnosticDebugName) // anonymous simple type gets this name from base.
    assertTrue(i.valid.get)
    val expected = <e1>-1</e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  val testSchema3 = SchemaUtils.dfdlTestSchema(
    <dfdl:format ref="daffodilTest1"/>,

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
    <xs:element name="e1" dfdl:lengthKind="delimited" type="ex:foo3bar"/>)

  @Test def testRestrictionOnUnion_01 {
    val (result, actual) = TestUtils.testString(testSchema3, "foo3bar")
    val i = result.resultState.asInstanceOf[PState].infoset.asInstanceOf[DIDocument].root.asInstanceOf[DISimple]
    val umstrd = i.unionMemberRuntimeData.get
    assertEquals("ex:foo3or4bar", umstrd.diagnosticDebugName)
    assertTrue(i.valid.get)
    val expected = <e1>foo3bar</e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testRestrictionOnUnion_02 {
    val (result, actual) = TestUtils.testString(testSchema3, "foo1bar")
    val i = result.resultState.asInstanceOf[PState].infoset.asInstanceOf[DIDocument].root.asInstanceOf[DISimple]
    val umstrd = i.unionMemberRuntimeData.get
    assertEquals("ex:foo1or2bar", umstrd.diagnosticDebugName)
    assertTrue(i.valid.get)
    val expected = <e1>foo1bar</e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testRestrictionOnUnion_03 {
    val (result, actual) = TestUtils.testString(testSchema3, "foo2bar")
    val i = result.resultState.asInstanceOf[PState].infoset.asInstanceOf[DIDocument].root.asInstanceOf[DISimple]
    val umstrd = i.unionMemberRuntimeData.get
    assertEquals("ex:foo1or2bar", umstrd.diagnosticDebugName)
    assertTrue(i.valid.get)
    val expected = <e1>foo2bar</e1>
    TestUtils.assertEqualsXMLElements(expected, actual)
  }

  @Test def testRestrictionOnUnionFail_01 {
    val (result, _) = TestUtils.testString(testSchema3, "foo4bar")
    val i = result.resultState.asInstanceOf[PState].infoset.asInstanceOf[DIDocument].root.asInstanceOf[DISimple]
    val Some(dv: String) = Some(i.dataValue)
    assertEquals("foo4bar", dv)
    assertTrue(i.unionMemberRuntimeData.isEmpty)
    assertFalse(i.valid.get)
    val ds = result.getDiagnostics
    assertTrue(ds.length == 1)
    val d = ds.head
    assertTrue(d.isInstanceOf[ValidationError])
    val msg = d.getMessage()
    def die = { println(msg); fail() }
    if (!msg.contains("ex:e1"))
      die
    if (!msg.contains("union members"))
      die
    if (!msg.contains("ex:foo1or2bar"))
      die
    if (!msg.contains("ex:foo3or4bar"))
      die
    if (!msg.contains("failed facet checks"))
      die
  }

  /**
   * This test shows that the union isn't even attempted if the
   * restriction on the union fails.
   */
  @Test def testRestrictionOnUnionFail_02 {
    val (result, _) = TestUtils.testString(testSchema3, "notfoo1bar")
    val i = result.resultState.asInstanceOf[PState].infoset.asInstanceOf[DIDocument].root.asInstanceOf[DISimple]
    val Some(dv: String) = Some(i.dataValue)
    assertEquals("notfoo1bar", dv)
    assertTrue(i.unionMemberRuntimeData.isEmpty)
    assertFalse(i.valid.get)
    val ds = result.getDiagnostics
    assertTrue(ds.length == 1)
    val d = ds.head
    assertTrue(d.isInstanceOf[ValidationError])
    val msg = d.getMessage()
    def die = { println(msg); fail() }
    if (!msg.contains("ex:e1"))
      die
    if (!msg.contains("facet pattern"))
      die
    if (!msg.contains("foo[1234]bar"))
      die
    if (!msg.contains("failed facet checks"))
      die
  }
}
