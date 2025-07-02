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

import scala.jdk.CollectionConverters._

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
      val msgs = pf.getDiagnostics.asScala.map(_.getMessage()).mkString("\n")
      throw new Exception(msgs)
    }
    val u = pf.onPath("/").asInstanceOf[DataProcessor]
    if (u.isError) {
      val msgs = u.getDiagnostics.asScala.map(_.getMessage()).mkString("\n")
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
    val (s, e) = is match {
      case List(Start(s: DISimple), End(e: DISimple)) => (s, e)
      case _ => fail(); null
    }
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
    val (s, e) = is match {
      case List(Start(s: DISimple), End(e: DISimple)) => (s, e)
      case _ => fail(); null
    }
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
    val bar_s = is.next() match {
      case Start(bar_s: DIComplex) => bar_s
      case _ => fail(); null
    }

    inp.pushTRD(fooERD)
    val foo_s = is.next() match {
      case Start(foo_s: DISimple) => foo_s
      case _ => fail(); null
    }
    bar_s.addChild(foo_s, tunable)
    val foo_e = is.next() match {
      case End(foo_e: DISimple) => foo_e
      case _ => fail(); null
    }
    val poppedERD = inp.popTRD()
    assertEquals(fooERD, poppedERD)
    val bar_e = is.next() match {
      case End(bar_e: DIComplex) => bar_e
      case _ => fail(); null
    }
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
    val bar_s = is.next() match {
      case Start(bar_s: DIComplex) => bar_s
      case _ => fail(); null
    }
    val Seq(fooERD, bazERD) = rootERD.childERDs
    inp.pushTRD(fooERD)
    val foo_s = is.next() match {
      case Start(foo_s: DISimple) => foo_s
      case _ => fail(); null
    }
    val foo_e = is.next() match {
      case End(foo_e: DISimple) => foo_e
      case _ => fail(); null
    }
    assertEquals(fooERD, inp.popTRD())
    inp.pushTRD(bazERD)
    val baz_s = is.next() match {
      case Start(baz_s: DISimple) => baz_s
      case _ => fail(); null
    }
    val baz_e = is.next() match {
      case End(baz_e: DISimple) => baz_e
      case _ => fail(); null
    }; assertNotNull(baz_e)
    assertEquals(bazERD, inp.popTRD())
    val bar_e = is.next() match {
      case End(bar_e: DIComplex) => bar_e
      case _ => fail(); null
    }
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
    val seq1TRD = rootERD.optComplexTypeModelGroupRuntimeData match {
      case Some(seq1TRD: SequenceRuntimeData) => seq1TRD
      case _ => fail(); null
    }
    val (bar1ERD, bar2ERD) = seq1TRD.groupMembers match {
      case Seq(bar1ERD: ElementRuntimeData, bar2ERD: ElementRuntimeData) => (bar1ERD, bar2ERD)
      case _ => fail(); null
    }
    val bar1SeqTRD = bar1ERD.optComplexTypeModelGroupRuntimeData match {
      case Some(bar1SeqTRD: SequenceRuntimeData) => bar1SeqTRD
      case _ => fail(); null
    }
    val (foo1ERD, baz1ERD) = bar1SeqTRD.groupMembers match {
      case Seq(foo1ERD: ElementRuntimeData, baz1ERD: ElementRuntimeData) => (foo1ERD, baz1ERD)
      case _ => fail(); null
    }
    val bar2SeqTRD = bar2ERD.optComplexTypeModelGroupRuntimeData match {
      case Some(bar2SeqTRD: SequenceRuntimeData) => bar2SeqTRD
      case _ => fail(); null
    }
    val (foo2ERD, baz2ERD) = bar2SeqTRD.groupMembers match {
      case Seq(foo2ERD: ElementRuntimeData, baz2ERD: ElementRuntimeData) => (foo2ERD, baz2ERD)
      case _ => fail(); null
    }

    inp.pushTRD(rootERD)
    val quux_s = is.next() match {
      case Start(quux_s: DIComplex) => quux_s
      case _ => fail(); null
    }
    inp.pushTRD(seq1TRD)
    inp.pushTRD(bar1ERD)
    val bar1_s = is.next() match {
      case Start(bar1_s: DIComplex) => bar1_s
      case _ => fail(); null
    }
    //
    // When the unparser for the bar1 element does eventually run, it will push the bar1ERD
    // and when it runs the model group unparser it will push that sequence's TRD.
    inp.pushTRD(bar1SeqTRD)
    inp.pushTRD(foo1ERD)
    val foo1_s = is.next() match {
      case Start(foo1_s: DISimple) => foo1_s
      case _ => fail(); null
    }
    val foo1_e = is.next() match {
      case End(foo1_e: DISimple) => foo1_e
      case _ => fail(); null
    }
    assertEquals(foo1ERD, inp.popTRD())
    inp.pushTRD(baz1ERD)
    val baz1_s = is.next() match {
      case Start(baz1_s: DISimple) => baz1_s
      case _ => fail(); null
    }
    val baz1_e = is.next() match {
      case End(baz1_e: DISimple) => baz1_e
      case _ => fail(); null
    }; assertNotNull(baz1_e)
    assertEquals(baz1ERD, inp.popTRD())
    //
    // At the end of a complex element, it should not be expecting
    // any other element start events
    //
    val badERD = inp.nextElement("notFound", XMLUtils.EXAMPLE_NAMESPACE, true)
    assertTrue(badERD.isInstanceOf[ErrorERD])

    val bar1_e = is.next() match {
      case End(bar1_e: DIComplex) => bar1_e
      case _ => fail(); null
    }
    assertEquals(bar1SeqTRD, inp.popTRD())
    assertEquals(bar1ERD, inp.popTRD())
    inp.pushTRD(bar2ERD)
    val bar2_s = is.next() match {
      case Start(bar2_s: DIComplex) => bar2_s
      case _ => fail(); null
    }
    inp.pushTRD(bar2SeqTRD)
    inp.pushTRD(foo2ERD)
    val foo2_s = is.next() match {
      case Start(foo2_s: DISimple) => foo2_s
      case _ => fail(); null
    }
    val foo2_e = is.next() match {
      case End(foo2_e: DISimple) => foo2_e
      case _ => fail(); null
    }
    assertEquals(foo2ERD, inp.popTRD())
    inp.pushTRD(baz2ERD)
    val baz2_s = is.next() match {
      case Start(baz2_s: DISimple) => baz2_s
      case _ => fail(); null
    }
    val baz2_e = is.next() match {
      case End(baz2_e: DISimple) => baz2_e
      case _ => fail(); null
    }
    assertNotNull(baz2_e)
    assertEquals(baz2ERD, inp.popTRD())
    assertEquals(bar2SeqTRD, inp.popTRD())
    assertEquals(bar2ERD, inp.popTRD())
    val bar2_e = is.next() match {
      case End(bar2_e: DIComplex) => bar2_e
      case _ => fail(); null
    }
    val quux_e = is.next() match {
      case End(quux_e: DIComplex) => quux_e
      case _ => fail(); null
    }
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
    val barSeqTRD = rootERD.optComplexTypeModelGroupRuntimeData match {
      case Some(barSeqTRD: SequenceRuntimeData) => barSeqTRD
      case _ => fail(); null
    }
    val fooERD = barSeqTRD.groupMembers match {
      case Seq(fooERD: ElementRuntimeData) => fooERD
      case _ => fail(); null
    }
    val doc = inp.documentElement
    val bar_s = is.next() match {
      case Start(bar_s: DIComplex) => bar_s
      case _ => fail(); null
    }
    doc.addChild(bar_s, tunable)
    inp.pushTRD(barSeqTRD)
    inp.pushTRD(fooERD)
    val foo_arr_s = is.next() match {
      case StartArray(foo_arr_s) => foo_arr_s
      case _ => fail(); null
    }
    val foo_1_s = is.next() match {
      case Start(foo_1_s: DISimple) => foo_1_s
      case _ => fail(); null
    }
    bar_s.addChild(foo_1_s, tunable)
    val foo_1_e = is.next() match {
      case End(foo_1_e: DISimple) => foo_1_e
      case _ => fail(); null
    }
    val foo_2_s = is.next() match {
      case Start(foo_2_s: DISimple) => foo_2_s
      case _ => fail(); null
    }
    bar_s.addChild(foo_2_s, tunable)
    val foo_2_e = is.next() match {
      case End(foo_2_e: DISimple) => foo_2_e
      case _ => fail(); null
    }
    val foo_arr_e = is.next() match {
      case EndArray(foo_arr_e) => foo_arr_e
      case _ => fail(); null
    }
    assertEquals(fooERD, inp.popTRD())
    assertEquals(barSeqTRD, inp.popTRD())
    val bar_e = is.next() match {
      case End(bar_e: DIComplex) => bar_e
      case _ => fail(); null
    }
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
    val barSeqTRD = rootERD.optComplexTypeModelGroupRuntimeData match {
      case Some(barSeqTRD: SequenceRuntimeData) => barSeqTRD
      case _ => fail(); null
    }
    val (fooERD, bazERD) = barSeqTRD.groupMembers match {
      case Seq(fooERD: ElementRuntimeData, bazERD: ElementRuntimeData) => (fooERD, bazERD)
      case _ => fail(); null
    }
    val bar_s = is.next() match {
      case Start(bar_s: DIComplex) => bar_s
      case _ => fail(); null
    }
    inp.pushTRD(barSeqTRD)
    inp.pushTRD(fooERD)
    val foo_arr_s = is.next() match {
      case StartArray(foo_arr_s) => foo_arr_s
      case _ => fail(); null
    }
    val foo_1_s = is.next() match {
      case Start(foo_1_s: DISimple) => foo_1_s
      case _ => fail(); null
    }
    val foo_1_e = is.next() match {
      case End(foo_1_e: DISimple) => foo_1_e
      case _ => fail(); null
    }
    val foo_2_s = is.next() match {
      case Start(foo_2_s: DISimple) => foo_2_s
      case _ => fail(); null
    }
    val foo_2_e = is.next() match {
      case End(foo_2_e: DISimple) => foo_2_e
      case _ => fail(); null
    }
    val foo_arr_e = is.next() match {
      case EndArray(foo_arr_e) => foo_arr_e
      case _ => fail(); null
    }
    //
    // While fooERD is still on the stack, we should be able to resolve baz element since
    // foo is optional
    //
    val baz_s = is.next() match {
      case Start(baz_s: DISimple) => baz_s
      case _ => fail(); null
    }
    assertEquals(fooERD, inp.popTRD())
    inp.pushTRD(bazERD)
    val baz_e = is.next() match {
      case End(baz_e: DISimple) => baz_e
      case _ => fail(); null
    }
    assertNotNull(baz_e)
    assertEquals(bazERD, inp.popTRD())
    assertEquals(barSeqTRD, inp.popTRD())
    val bar_e = is.next() match {
      case End(bar_e: DIComplex) => bar_e
      case _ => fail(); null
    }
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
    val barSeqTRD = rootERD.optComplexTypeModelGroupRuntimeData match {
      case Some(barSeqTRD: SequenceRuntimeData) => barSeqTRD
      case _ => fail(); null
    }
    val (bazERD, fooERD) = barSeqTRD.groupMembers match {
      case Seq(bazERD: ElementRuntimeData, fooERD: ElementRuntimeData) => (bazERD, fooERD)
      case _ => fail(); null
    }

    val bar_s = is.next() match {
      case Start(bar_s: DIComplex) => bar_s
      case _ => fail(); null
    }

    inp.pushTRD(barSeqTRD)
    inp.pushTRD(bazERD)

    val baz_s = is.next() match {
      case Start(baz_s: DISimple) => baz_s
      case _ => fail(); null
    }
    val baz_e = is.next() match {
      case End(baz_e: DISimple) => baz_e
      case _ => fail(); null
    }
    assertNotNull(baz_e)
    inp.popTRD()
    inp.pushTRD(fooERD)
    val foo_arr_s = is.next() match {
      case StartArray(foo_arr_s) => foo_arr_s
      case _ => fail(); null
    }
    val foo_1_s = is.next() match {
      case Start(foo_1_s: DISimple) => foo_1_s
      case _ => fail(); null
    }
    val foo_1_e = is.next() match {
      case End(foo_1_e: DISimple) => foo_1_e
      case _ => fail(); null
    }
    val foo_2_s = is.next() match {
      case Start(foo_2_s: DISimple) => foo_2_s
      case _ => fail(); null
    }
    val foo_2_e = is.next() match {
      case End(foo_2_e: DISimple) => foo_2_e
      case _ => fail(); null
    }
    inp.popTRD()
    inp.popTRD()
    val foo_arr_e = is.next() match {
      case EndArray(foo_arr_e) => foo_arr_e
      case _ => fail(); null
    }

    val bar_e = is.next() match {
      case End(bar_e: DIComplex) => bar_e
      case _ => fail(); null
    }
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
    val barSeqTRD = rootERD.optComplexTypeModelGroupRuntimeData match {
      case Some(barSeqTRD: SequenceRuntimeData) => barSeqTRD
      case _ => fail(); null
    }
    val (bazERD, fooERD) = barSeqTRD.groupMembers match {
      case Seq(bazERD: ElementRuntimeData, fooERD: ElementRuntimeData) => (bazERD, fooERD)
      case _ => fail(); null
    }

    val bar_s = is.next() match {
      case Start(bar_s: DIComplex) => bar_s
      case _ => fail(); null
    }
    inp.pushTRD(barSeqTRD)
    inp.pushTRD(bazERD)
    val baz_arr_s = is.next() match {
      case StartArray(baz_arr_s) => baz_arr_s
      case _ => fail(); null
    }
    val baz_s = is.next() match {
      case Start(baz_s: DISimple) => baz_s
      case _ => fail(); null
    }
    val baz_e = is.next() match {
      case End(baz_e: DISimple) => baz_e
      case _ => fail(); null
    }
    assertNotNull(baz_e)
    val baz_arr_e = is.next() match {
      case EndArray(baz_arr_e) => baz_arr_e
      case _ => fail(); null
    }
    val foo_arr_s = is.next() match {
      case StartArray(foo_arr_s) => foo_arr_s
      case _ => fail(); null
    }
    val foo_1_s = is.next() match {
      case Start(foo_1_s: DISimple) => foo_1_s
      case _ => fail(); null
    }
    val foo_1_e = is.next() match {
      case End(foo_1_e: DISimple) => foo_1_e
      case _ => fail(); null
    }
    val foo_2_s = is.next() match {
      case Start(foo_2_s: DISimple) => foo_2_s
      case _ => fail(); null
    }
    val foo_2_e = is.next() match {
      case End(foo_2_e: DISimple) => foo_2_e
      case _ => fail(); null
    }
    val foo_arr_e = is.next() match {
      case EndArray(foo_arr_e) => foo_arr_e
      case _ => fail(); null
    }
    inp.popTRD()
    val bar_e = is.next() match {
      case End(bar_e: DIComplex) => bar_e
      case _ => fail(); null
    }
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
    val barSeqTRD = rootERD.optComplexTypeModelGroupRuntimeData match {
      case Some(barSeqTRD: SequenceRuntimeData) => barSeqTRD
      case _ => fail(); null
    }
    val fooERD = barSeqTRD.groupMembers match {
      case Seq(fooERD: ElementRuntimeData) => fooERD
      case _ => fail(); null
    }

    val bar_s1 = is.peek match {
      case Start(bar_s1: DIComplex) => bar_s1
      case _ => fail(); null
    }
    val bar_s2 = is.peek match {
      case Start(bar_s2: DIComplex) => bar_s2
      case _ => fail(); null
    }
    val bar_s = is.next() match {
      case Start(bar_s: DIComplex) => bar_s
      case _ => fail(); null
    }
    inp.pushTRD(barSeqTRD)
    inp.pushTRD(fooERD)
    val foo_s1 = is.peek match {
      case Start(foo_s1: DISimple) => foo_s1
      case _ => fail(); null
    }
    val foo_s2 = is.next() match {
      case Start(foo_s2: DISimple) => foo_s2
      case _ => fail(); null
    }
    val foo_e = is.next() match {
      case End(foo_e: DISimple) => foo_e
      case _ => fail(); null
    }
    inp.popTRD()
    inp.popTRD()
    val bar_e1 = is.peek match {
      case End(bar_e1: DIComplex) => bar_e1
      case _ => fail(); null
    }
    assertTrue(is.hasNext)
    val bar_e2 = is.next() match {
      case End(bar_e2: DIComplex) => bar_e2
      case _ => fail(); null
    }
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
    val eSeqTRD = eERD.optComplexTypeModelGroupRuntimeData match {
      case Some(eSeqTRD: SequenceRuntimeData) => eSeqTRD
      case _ => fail(); null
    }
    val sERD = eSeqTRD.groupMembers match {
      case Seq(sERD: ElementRuntimeData) => sERD
      case _ => fail(); null
    }
    val sChoTRD = sERD.optComplexTypeModelGroupRuntimeData match {
      case Some(sChoTRD: ChoiceRuntimeData) => sChoTRD
      case _ => fail(); null
    }
    val _ = sChoTRD.groupMembers match {
      case Seq(c1ERD: ElementRuntimeData, c2ERD: ElementRuntimeData) => c1ERD
      case _ => fail(); null
    }
    val e = is.next() match {
      case Start(e: DIComplex) => e
      case _ => fail(); null
    }
    inp.pushTRD(eSeqTRD)
    inp.pushTRD(sERD)
    val as = is.next() match {
      case StartArray(as) => as
      case _ => fail(); null
    }
    val s1 = is.next() match {
      case Start(s1: DIComplex) => s1
      case _ => fail(); null
    }; assertNotNull(s1)
    inp.pushTRD(sChoTRD)
    val c1 = is.next() match {
      case Start(c1: DISimple) => c1
      case _ => fail(); null
    }
    val c1e = is.next() match {
      case End(c1e: DISimple) => c1e
      case _ => fail(); null
    }; assertNotNull(c1e)
    inp.popTRD()
    inp.popTRD()
    val s1e = is.next() match {
      case End(s1e: DIComplex) => s1e
      case _ => fail(); null
    }; assertNotNull(s1e)
    inp.pushTRD(sERD)
    val s2 = is.next() match {
      case Start(s2: DIComplex) => s2
      case _ => fail(); null
    }; assertNotNull(s2)
    inp.pushTRD(sChoTRD)
    val c2 = is.next() match {
      case Start(c2: DISimple) => c2
      case _ => fail(); null
    }
    val c2e = is.next() match {
      case End(c2e: DISimple) => c2e
      case _ => fail(); null
    };; assertNotNull(c2e)
    inp.popTRD()
    val s2e = is.next() match {
      case End(s2e: DIComplex) => s2e
      case _ => fail(); null
    }; assertNotNull(s2e)
    val ase = is.next() match {
      case EndArray(ase) => ase
      case _ => fail(); null
    }
    inp.popTRD()
    val ee = is.next() match {
      case End(ee: DIComplex) => ee
      case _ => fail(); null
    }
    assertFalse(is.hasNext)
    assertTrue(as eq ase) // exact same object
    assertTrue(e eq ee)
    assertTrue(c1.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(c1.dataValueAsString =:= "Hello")
    assertTrue(c2.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(c2.dataValueAsString =:= "World")
  }
}
