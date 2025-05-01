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

package org.apache.daffodil.core.infoset

import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.equality._
import org.apache.daffodil.lib.util.IteratorFromCursor
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml._
import org.apache.daffodil.runtime1.infoset._
import org.apache.daffodil.runtime1.processors.ChoiceRuntimeData
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.ErrorERD
import org.apache.daffodil.runtime1.processors.SequenceRuntimeData

import org.junit.Assert._
import org.junit.Test

/**
 * All these tests were written for iterator style.
 * Now that we're doing Cursor style, need an adapter. otherwise we have to edit them all.
 */
case class Adapter(isrc: InfosetInputter)
  extends IteratorFromCursor[InfosetAccessor, InfosetAccessor](
    isrc,
    (ie: InfosetAccessor) => ie
  )

class TestInfosetInputterFromReader {

  def infosetInputter(testSchema: scala.xml.Node, infosetXML: scala.xml.Node) = {
    val compiler = Compiler()
    val pf = compiler.compileNode(testSchema)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val u = pf.onPath("/").asInstanceOf[DataProcessor]
    if (u.isError) {
      val msgs = u.getDiagnostics.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val rootERD = u.ssrd.elementRuntimeData
    val inputter = new InfosetInputter(new ScalaXMLInfosetInputter(infosetXML))
    inputter.initialize(rootERD, u.tunables)
    val is = Adapter(inputter)
    (is, rootERD, inputter, u.tunables)
  }

  @Test def testUnparseFixedLengthString1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
    )
    val infosetXML = <foo xmlns={XMLUtils.EXAMPLE_NAMESPACE}>Hello</foo>
    TestUtils.testUnparsing(sch, infosetXML, "Hello")
  }

  @Test def testInfosetInputter1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
    )
    val infosetXML = <foo xmlns={XMLUtils.EXAMPLE_NAMESPACE}>Hello</foo>
    val (ii, _, _, _) = infosetInputter(sch, infosetXML)
    val is = ii.to(LazyList).toList
    val List(Start(s: DISimple), End(e: DISimple)) = is
    assertTrue(s eq e) // exact same object
    assertTrue(s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(s.dataValueAsString =:= "Hello")
  }

  @Test def testInfosetInputterNil1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element nillable="true" dfdl:nilValue="nil" dfdl:nilKind="literalValue" name="foo" dfdl:lengthKind="explicit" dfdl:length="3" type="xs:string"/>
    )
    val infosetXML = <foo xsi:nil="true" xmlns={XMLUtils.EXAMPLE_NAMESPACE} xmlns:xsi={
      XMLUtils.XSI_NAMESPACE
    }/>
    val (ii, _, _, _) = infosetInputter(sch, infosetXML)
    val is = ii.to(LazyList).toList
    val List(Start(s: DISimple), End(e: DISimple)) = is
    assertTrue(s eq e) // exact same object
    assertTrue(s.isNilled)
  }

  @Test def testInfosetComplex1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val infosetXML = <bar xmlns={XMLUtils.EXAMPLE_NAMESPACE}><foo>Hello</foo></bar>
    val (is, rootERD, inp, tunable) = infosetInputter(sch, infosetXML)
    val Seq(fooERD) = rootERD.childERDs
    val Start(bar_s: DIComplex) = is.next()

    inp.pushTRD(fooERD)
    val Start(foo_s: DISimple) = is.next()
    bar_s.addChild(foo_s, tunable)
    val End(foo_e: DISimple) = is.next()
    val poppedERD = inp.popTRD()
    assertEquals(fooERD, poppedERD)
    val End(bar_e: DIComplex) = is.next()
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_s eq foo_e)
    assertTrue(foo_s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(foo_s.dataValueAsString =:= "Hello")
  }

  @Test def testInfosetComplex2(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
            <xs:element name="baz" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val infosetXML = <bar xmlns={
      XMLUtils.EXAMPLE_NAMESPACE
    }><foo>Hello</foo><baz>World</baz></bar>
    val (is, rootERD, inp, tunable) = infosetInputter(sch, infosetXML)
    val Start(bar_s: DIComplex) = is.next()
    val Seq(fooERD, bazERD) = rootERD.childERDs
    inp.pushTRD(fooERD)
    val Start(foo_s: DISimple) = is.next()
    val End(foo_e: DISimple) = is.next()
    assertEquals(fooERD, inp.popTRD())
    inp.pushTRD(bazERD)
    val Start(baz_s: DISimple) = is.next()
    val End(baz_e: DISimple) = is.next(); assertNotNull(baz_e)
    assertEquals(bazERD, inp.popTRD())
    val End(bar_e: DIComplex) = is.next()
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_s eq foo_e)
    assertTrue(foo_s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(foo_s.dataValueAsString =:= "Hello")
    assertTrue(baz_s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(baz_s.dataValueAsString =:= "World")
  }

  @Test def testInfosetComplex3(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="quux">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="bar1" dfdl:lengthKind="implicit">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="foo1" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
                  <xs:element name="baz1" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="bar2" dfdl:lengthKind="implicit">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="foo2" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
                  <xs:element name="baz2" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val infosetXML = <quux xmlns={XMLUtils.EXAMPLE_NAMESPACE}>
                       <bar1><foo1>Hello</foo1><baz1>World</baz1></bar1>
                       <bar2><foo2>Hello</foo2><baz2>World</baz2></bar2>
                     </quux>
    val (is, rootERD, inp, tunable) = infosetInputter(sch, infosetXML)
    //
    // Get all the ERDs and Sequence TRDs
    //
    val Some(seq1TRD: SequenceRuntimeData) = rootERD.optComplexTypeModelGroupRuntimeData
    val Seq(bar1ERD: ElementRuntimeData, bar2ERD: ElementRuntimeData) = seq1TRD.groupMembers
    val Some(bar1SeqTRD: SequenceRuntimeData) = bar1ERD.optComplexTypeModelGroupRuntimeData
    val Seq(foo1ERD: ElementRuntimeData, baz1ERD: ElementRuntimeData) = bar1SeqTRD.groupMembers
    val Some(bar2SeqTRD: SequenceRuntimeData) = bar2ERD.optComplexTypeModelGroupRuntimeData
    val Seq(foo2ERD: ElementRuntimeData, baz2ERD: ElementRuntimeData) = bar2SeqTRD.groupMembers

    inp.pushTRD(rootERD)
    val Start(quux_s: DIComplex) = is.next()
    inp.pushTRD(seq1TRD)
    inp.pushTRD(bar1ERD)
    val Start(bar1_s: DIComplex) = is.next()
    //
    // When the unparser for the bar1 element does eventually run, it will push the bar1ERD
    // and when it runs the model group unparser it will push that sequence's TRD.
    inp.pushTRD(bar1SeqTRD)
    inp.pushTRD(foo1ERD)
    val Start(foo1_s: DISimple) = is.next()
    val End(foo1_e: DISimple) = is.next()
    assertEquals(foo1ERD, inp.popTRD())
    inp.pushTRD(baz1ERD)
    val Start(baz1_s: DISimple) = is.next()
    val End(baz1_e: DISimple) = is.next();
    assertNotNull(baz1_e)
    assertEquals(baz1ERD, inp.popTRD())
    //
    // At the end of a complex element, it should not be expecting
    // any other element start events
    //
    val badERD = inp.nextElement("notFound", XMLUtils.EXAMPLE_NAMESPACE, true)
    assertTrue(badERD.isInstanceOf[ErrorERD])

    val End(bar1_e: DIComplex) = is.next()
    assertEquals(bar1SeqTRD, inp.popTRD())
    assertEquals(bar1ERD, inp.popTRD())
    inp.pushTRD(bar2ERD)
    val Start(bar2_s: DIComplex) = is.next()
    inp.pushTRD(bar2SeqTRD)
    inp.pushTRD(foo2ERD)
    val Start(foo2_s: DISimple) = is.next()
    val End(foo2_e: DISimple) = is.next()
    assertEquals(foo2ERD, inp.popTRD())
    inp.pushTRD(baz2ERD)
    val Start(baz2_s: DISimple) = is.next()
    val End(baz2_e: DISimple) = is.next();
    assertNotNull(baz2_e)
    assertEquals(baz2ERD, inp.popTRD())
    assertEquals(bar2SeqTRD, inp.popTRD())
    assertEquals(bar2ERD, inp.popTRD())
    val End(bar2_e: DIComplex) = is.next()
    val End(quux_e: DIComplex) = is.next()
    assertFalse(is.hasNext)
    assertTrue(bar1_s eq bar1_e) // exact same object
    assertTrue(foo1_s eq foo1_e)
    assertTrue(foo1_s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(foo1_s.dataValueAsString =:= "Hello")
    assertTrue(baz1_s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(baz1_s.dataValueAsString =:= "World")
    assertTrue(bar2_s eq bar2_e) // exact same object
    assertTrue(foo2_s eq foo2_e)
    assertTrue(foo2_s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(foo2_s.dataValueAsString =:= "Hello")
    assertTrue(baz2_s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(baz2_s.dataValueAsString =:= "World")
    assertTrue(quux_s eq quux_e)
  }

  @Test def testInfosetArray1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val infosetXML = <bar xmlns={
      XMLUtils.EXAMPLE_NAMESPACE
    }><foo>Hello</foo><foo>World</foo></bar>
    val (is, rootERD, inp, tunable) = infosetInputter(sch, infosetXML)
    val Some(barSeqTRD: SequenceRuntimeData) = rootERD.optComplexTypeModelGroupRuntimeData
    val Seq(fooERD: ElementRuntimeData) = barSeqTRD.groupMembers
    val doc = inp.documentElement
    val Start(bar_s: DIComplex) = is.next()
    doc.addChild(bar_s, tunable)
    inp.pushTRD(barSeqTRD)
    inp.pushTRD(fooERD)
    val StartArray(foo_arr_s) = is.next()
    val Start(foo_1_s: DISimple) = is.next()
    bar_s.addChild(foo_1_s, tunable)
    val End(foo_1_e: DISimple) = is.next()
    val Start(foo_2_s: DISimple) = is.next()
    bar_s.addChild(foo_2_s, tunable)
    val End(foo_2_e: DISimple) = is.next()
    val EndArray(foo_arr_e) = is.next()
    assertEquals(fooERD, inp.popTRD())
    assertEquals(barSeqTRD, inp.popTRD())
    val End(bar_e: DIComplex) = is.next()
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_arr_s eq foo_arr_e)
    assertTrue(foo_1_s eq foo_1_e)
    assertTrue(foo_1_s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(foo_1_s.dataValueAsString =:= "Hello")
    assertTrue(foo_2_s eq foo_2_e)
    assertTrue(foo_2_s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(foo_2_s.dataValueAsString =:= "World")
  }

  @Test def testInfosetArray2(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
            <xs:element name="baz" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val infosetXML = <bar xmlns={
      XMLUtils.EXAMPLE_NAMESPACE
    }><foo>Hello</foo><foo>World</foo><baz>Yadda</baz></bar>
    val (is, rootERD, inp, tunable) = infosetInputter(sch, infosetXML)
    val Some(barSeqTRD: SequenceRuntimeData) = rootERD.optComplexTypeModelGroupRuntimeData
    val Seq(fooERD: ElementRuntimeData, bazERD: ElementRuntimeData) = barSeqTRD.groupMembers
    val Start(bar_s: DIComplex) = is.next()
    inp.pushTRD(barSeqTRD)
    inp.pushTRD(fooERD)
    val StartArray(foo_arr_s) = is.next()
    val Start(foo_1_s: DISimple) = is.next()
    val End(foo_1_e: DISimple) = is.next()
    val Start(foo_2_s: DISimple) = is.next()
    val End(foo_2_e: DISimple) = is.next()
    val EndArray(foo_arr_e) = is.next()
    //
    // While fooERD is still on the stack, we should be able to resolve baz element since
    // foo is optional
    //
    val Start(baz_s: DISimple) = is.next()
    assertEquals(fooERD, inp.popTRD())
    inp.pushTRD(bazERD)
    val End(baz_e: DISimple) = is.next()
    assertNotNull(baz_e)
    assertEquals(bazERD, inp.popTRD())
    assertEquals(barSeqTRD, inp.popTRD())
    val End(bar_e: DIComplex) = is.next()
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_arr_s eq foo_arr_e)
    assertTrue(foo_1_s eq foo_1_e)
    assertTrue(foo_1_s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(foo_1_s.dataValueAsString =:= "Hello")
    assertTrue(foo_2_s eq foo_2_e)
    assertTrue(foo_2_s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(foo_2_s.dataValueAsString =:= "World")
    assertTrue(baz_s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(baz_s.dataValueAsString =:= "Yadda")
  }

  @Test def testInfosetArray3(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="baz" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
            <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val infosetXML = <bar xmlns={
      XMLUtils.EXAMPLE_NAMESPACE
    }><baz>Yadda</baz><foo>Hello</foo><foo>World</foo></bar>
    val (is, rootERD, inp, tunable) = infosetInputter(sch, infosetXML)
    val Some(barSeqTRD: SequenceRuntimeData) = rootERD.optComplexTypeModelGroupRuntimeData
    val Seq(bazERD: ElementRuntimeData, fooERD: ElementRuntimeData) = barSeqTRD.groupMembers

    val Start(bar_s: DIComplex) = is.next()

    inp.pushTRD(barSeqTRD)
    inp.pushTRD(bazERD)

    val Start(baz_s: DISimple) = is.next()
    val End(baz_e: DISimple) = is.next()
    assertNotNull(baz_e)
    inp.popTRD()
    inp.pushTRD(fooERD)
    val StartArray(foo_arr_s) = is.next()
    val Start(foo_1_s: DISimple) = is.next()
    val End(foo_1_e: DISimple) = is.next()
    val Start(foo_2_s: DISimple) = is.next()
    val End(foo_2_e: DISimple) = is.next()
    inp.popTRD()
    inp.popTRD()
    val EndArray(foo_arr_e) = is.next()

    val End(bar_e: DIComplex) = is.next()
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_arr_s eq foo_arr_e)
    assertTrue(foo_1_s eq foo_1_e)
    assertTrue(foo_1_s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(foo_1_s.dataValueAsString =:= "Hello")
    assertTrue(foo_2_s eq foo_2_e)
    assertTrue(foo_2_s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(foo_2_s.dataValueAsString =:= "World")
    assertTrue(baz_s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(baz_s.dataValueAsString =:= "Yadda")
  }

  @Test def testInfosetArray4(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="baz" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
            <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val infosetXML = <bar xmlns={
      XMLUtils.EXAMPLE_NAMESPACE
    }><baz>Yadda</baz><foo>Hello</foo><foo>World</foo></bar>
    val (is, rootERD, inp, tunable) = infosetInputter(sch, infosetXML)
    val Some(barSeqTRD: SequenceRuntimeData) = rootERD.optComplexTypeModelGroupRuntimeData
    val Seq(bazERD: ElementRuntimeData, fooERD: ElementRuntimeData) = barSeqTRD.groupMembers

    val Start(bar_s: DIComplex) = is.next()
    inp.pushTRD(barSeqTRD)
    inp.pushTRD(bazERD)
    val StartArray(baz_arr_s) = is.next()
    val Start(baz_s: DISimple) = is.next()
    val End(baz_e: DISimple) = is.next()
    assertNotNull(baz_e)
    val EndArray(baz_arr_e) = is.next()
    val StartArray(foo_arr_s) = is.next()
    val Start(foo_1_s: DISimple) = is.next()
    val End(foo_1_e: DISimple) = is.next()
    val Start(foo_2_s: DISimple) = is.next()
    val End(foo_2_e: DISimple) = is.next()
    val EndArray(foo_arr_e) = is.next()
    inp.popTRD()
    val End(bar_e: DIComplex) = is.next()
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_arr_s eq foo_arr_e)
    assertTrue(baz_arr_s eq baz_arr_e)
    assertTrue(foo_1_s eq foo_1_e)
    assertTrue(foo_1_s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(foo_1_s.dataValueAsString =:= "Hello")
    assertTrue(foo_2_s eq foo_2_e)
    assertTrue(foo_2_s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(foo_2_s.dataValueAsString =:= "World")
    assertTrue(baz_s.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(baz_s.dataValueAsString =:= "Yadda")
  }

  @Test def testInfosetComplexPeek1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val infosetXML = <bar xmlns={XMLUtils.EXAMPLE_NAMESPACE}><foo>Hello</foo></bar>
    val (is, rootERD, inp, tunable) = infosetInputter(sch, infosetXML)
    val Some(barSeqTRD: SequenceRuntimeData) = rootERD.optComplexTypeModelGroupRuntimeData
    val Seq(fooERD: ElementRuntimeData) = barSeqTRD.groupMembers

    val Start(bar_s1: DIComplex) = is.peek
    val Start(bar_s2: DIComplex) = is.peek
    val Start(bar_s: DIComplex) = is.next()
    inp.pushTRD(barSeqTRD)
    inp.pushTRD(fooERD)
    val Start(foo_s1: DISimple) = is.peek
    val Start(foo_s2: DISimple) = is.next()
    val End(foo_e: DISimple) = is.next()
    inp.popTRD()
    inp.popTRD()
    val End(bar_e1: DIComplex) = is.peek
    assertTrue(is.hasNext)
    val End(bar_e2: DIComplex) = is.next()
    assertFalse(is.hasNext)
    assertTrue(bar_s1 eq bar_s2)
    assertTrue(bar_e1 eq bar_e2)
    assertTrue(bar_s1 eq bar_e1) // exact same object
    assertTrue(foo_s1 eq foo_s2)
    assertTrue(foo_s1 eq foo_e)
    assertTrue(foo_s1.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(foo_s1.dataValueAsString =:= "Hello")
  }

  @Test def testInfosetArrayComplex1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="e" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="s" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:choice>
                  <xs:element name="c1" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
                  <xs:element name="c2" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
                </xs:choice>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    )
    val infosetXML = <e xmlns={
      XMLUtils.EXAMPLE_NAMESPACE
    }><s><c1>Hello</c1></s><s><c2>World</c2></s></e>
    val (is, eERD, inp, tunable) = infosetInputter(sch, infosetXML)
    val Some(eSeqTRD: SequenceRuntimeData) = eERD.optComplexTypeModelGroupRuntimeData
    val Seq(sERD: ElementRuntimeData) = eSeqTRD.groupMembers
    val Some(sChoTRD: ChoiceRuntimeData) = sERD.optComplexTypeModelGroupRuntimeData
    val Seq(c1ERD: ElementRuntimeData, c2ERD: ElementRuntimeData) = sChoTRD.groupMembers
    val Start(e: DIComplex) = is.next()
    inp.pushTRD(eSeqTRD)
    inp.pushTRD(sERD)
    val StartArray(as) = is.next()
    val Start(s1: DIComplex) = is.next(); assertNotNull(s1)
    inp.pushTRD(sChoTRD)
    val Start(c1: DISimple) = is.next()
    val End(c1e: DISimple) = is.next(); assertNotNull(c1e)
    inp.popTRD()
    inp.popTRD()
    val End(s1e: DIComplex) = is.next(); assertNotNull(s1e)
    inp.pushTRD(sERD)
    val Start(s2: DIComplex) = is.next(); assertNotNull(s2)
    inp.pushTRD(sChoTRD)
    val Start(c2: DISimple) = is.next()
    val End(c2e: DISimple) = is.next();; assertNotNull(c2e)
    inp.popTRD()
    val End(s2e: DIComplex) = is.next(); assertNotNull(s2e)
    val EndArray(ase) = is.next()
    inp.popTRD()
    val End(ee: DIComplex) = is.next()
    assertFalse(is.hasNext)
    assertTrue(as eq ase) // exact same object
    assertTrue(e eq ee)
    assertTrue(c1.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(c1.dataValueAsString =:= "Hello")
    assertTrue(c2.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(c2.dataValueAsString =:= "World")
  }
}
