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
import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.equality._
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml._
import org.apache.daffodil.runtime1.infoset._
import org.apache.daffodil.runtime1.processors.ChoiceRuntimeData
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.SequenceRuntimeData

import InfosetEventKind._
import org.junit.Assert._
import org.junit.Test

/**
 * Lets us pattern deconstruct infoset events, which is nice for unit testing
 */
object Start {
  def unapply(ev: InfosetAccessor) =
    if (ev.kind.isInstanceOf[StartKind]) Some(ev.info.element) else None
}

object End {
  def unapply(ev: InfosetAccessor) =
    if (ev.kind.isInstanceOf[EndKind]) Some(ev.info.element) else None
}

object StartElement {
  def apply(node: DIElement) = InfosetAccessor(InfosetEventKind.StartElement, node)
  def unapply(ev: InfosetAccessor) =
    if (ev.kind eq InfosetEventKind.StartElement) Some(ev.info.element) else None
}

object EndElement {
  def apply(node: DIElement) = InfosetAccessor(InfosetEventKind.EndElement, node)
  def unapply(ev: InfosetAccessor) =
    if (ev.kind eq InfosetEventKind.EndElement) Some(ev.info.element) else None
}

object StartArray {
  def apply(arrayERD: ElementRuntimeData) =
    InfosetAccessor(InfosetEventKind.StartArray, arrayERD)
  def unapply(ev: InfosetAccessor) =
    if (ev.kind eq InfosetEventKind.StartArray) {
      assertTrue(ev.erd.isArray)
      Some(ev.info.arrayERD)
    } else None
}

object EndArray {
  def apply(arrayERD: ElementRuntimeData) = InfosetAccessor(InfosetEventKind.EndArray, arrayERD)
  def unapply(ev: InfosetAccessor) =
    if (ev.kind eq InfosetEventKind.EndArray) {
      assertTrue(ev.erd.isArray)
      Some(ev.info.arrayERD)
    } else None
}

class TestInfosetInputter {

  /**
   * Compiles the schema to runtime data, then converts the infosetXML (our XML representation
   * of a Daffodil infoset) into an actual InfosetDocument object (Daffodil's native data structure),
   * and then creates an InfosetInputter which can be used to read out those nodes in-order.
   */
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
    val infosetInputter = new InfosetInputter(new ScalaXMLInfosetInputter(infosetXML))
    infosetInputter.initialize(rootERD, u.tunables)
    (infosetInputter, rootERD)
  }

  @Test def testInfosetInputterFromSimpleValue1(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
    )
    val infosetXML = <foo xmlns={XMLUtils.EXAMPLE_NAMESPACE}>Hello</foo>
    val (ic, rootERD) = infosetInputter(sch, infosetXML)

    val aacc: InfosetAccessor = ic.advanceAccessor
    val iicc: InfosetAccessor = ic.inspectAccessor

    assertTrue(ic.advance)
    val Start(foo: DISimple) = aacc
    assertEquals("Hello", foo.dataValueAsString)

    assertTrue(ic.inspect)
    val End(foo_i: DISimple) = iicc
    assertTrue(foo_i eq foo)

    assertTrue(ic.advance)
    val End(foo_e: DISimple) = aacc
    assertTrue(foo_e eq foo)

    assertFalse(ic.inspect)
    assertFalse(ic.advance)
  }

  @Test def testInfosetInputterFromTreeNil(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"/>,
      <xs:element nillable="true" dfdl:nilValue="nil" dfdl:nilKind="literalValue" name="foo" dfdl:lengthKind="explicit" dfdl:length="3" type="xs:string"/>
    )
    val infosetXML = <foo xsi:nil="true" xmlns={XMLUtils.EXAMPLE_NAMESPACE} xmlns:xsi={
      XMLUtils.XSI_NAMESPACE
    }/>
    val (ic, rootERD) = infosetInputter(sch, infosetXML)

    val aacc = ic.advanceAccessor
    val iicc: InfosetAccessor = ic.inspectAccessor

    assertTrue(ic.advance)
    val Start(foo: DISimple) = aacc
    assertTrue(foo.isNilled)

    assertTrue(ic.inspect)
    val End(foo_i: DISimple) = iicc
    assertTrue(foo_i eq foo)

    assertTrue(ic.advance)
    val End(foo_e: DISimple) = aacc
    assertTrue(foo_e eq foo)

    assertFalse(ic.inspect)
    assertFalse(ic.advance)
  }

  @Test def testInfosetInputterFromTreeComplex1(): Unit = {
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

    val (ic, rootERD) = infosetInputter(sch, infosetXML)

    val aacc = ic.advanceAccessor
    val iacc = ic.inspectAccessor

    val barERD = rootERD
    val Some(barSeqTRD) = barERD.optComplexTypeModelGroupRuntimeData
    val Seq(fooERD) = barERD.childERDs
    ic.pushTRD(barERD)
    assertTrue(ic.advance)
    val Start(bar: DIComplex) = aacc
    ic.pushTRD(barSeqTRD)
    ic.pushTRD(fooERD)
    assertTrue(ic.advance)
    val Start(foo: DISimple) = aacc
    assertEquals("Hello", foo.dataValue.getAnyRef)

    assertTrue(ic.inspect)
    val End(ifoo: DISimple) = iacc
    assertTrue(foo eq ifoo)

    assertTrue(ic.advance)
    val End(eFoo: DISimple) = aacc
    assertTrue(foo eq eFoo)

    ic.popTRD()
    assertTrue(ic.inspect)
    val End(ibar: DIComplex) = iacc
    assertTrue(bar eq ibar)

    assertTrue(ic.advance)
    val End(bar_e: DIComplex) = aacc
    assertTrue(bar eq bar_e)
    ic.popTRD()
    ic.popTRD()
    assertFalse(ic.inspect)
    assertFalse(ic.advance)
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
    val (ic, rootERD) = infosetInputter(sch, infosetXML)
    val aacc = ic.advanceAccessor
    val Start(bar_s: DIComplex) = { assertTrue(ic.advance); aacc }
    val barERD = bar_s.erd
    val Some(barSeqTRD) = barERD.optComplexTypeModelGroupRuntimeData
    val Seq(fooERD, bazERD) = barERD.childERDs

    ic.pushTRD(barSeqTRD)
    ic.pushTRD(fooERD)
    val Start(foo_s: DISimple) = { assertTrue(ic.advance); aacc }
    val End(foo_e: DISimple) = { assertTrue(ic.advance); aacc }
    ic.popTRD()
    ic.pushTRD(bazERD)
    val Start(baz_s: DISimple) = { assertTrue(ic.advance); aacc }
    val End(baz_e: DISimple) = { assertTrue(ic.advance); aacc };
    assertNotNull(baz_e)
    ic.popTRD()
    val End(bar_e: DIComplex) = { assertTrue(ic.advance); aacc }
    ic.popTRD()
    ic.popTRD()
    assertFalse(ic.inspect)
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
    val (ic, rootERD) = infosetInputter(sch, infosetXML)
    val aacc = ic.advanceAccessor
    val Start(quux_s: DIComplex) = { assertTrue(ic.advance); aacc }
    val quuxERD = quux_s.erd
    val Some(quuxSeqTRD: SequenceRuntimeData) = quuxERD.optComplexTypeModelGroupRuntimeData
    val Seq(bar1ERD, bar2ERD) = quuxERD.childERDs
    val Some(bar1SeqTRD: SequenceRuntimeData) = bar1ERD.optComplexTypeModelGroupRuntimeData
    val Some(bar2SeqTRD: SequenceRuntimeData) = bar2ERD.optComplexTypeModelGroupRuntimeData
    val Seq(foo1ERD, baz1ERD) = bar1ERD.childERDs
    val Seq(foo2ERD, baz2ERD) = bar2ERD.childERDs
    ic.pushTRD(quuxSeqTRD)
    ic.pushTRD(bar1ERD)
    val Start(bar1_s: DIComplex) = { assertTrue(ic.advance); aacc }
    ic.pushTRD(bar1SeqTRD)
    ic.pushTRD(foo1ERD)
    val Start(foo1_s: DISimple) = { assertTrue(ic.advance); aacc }
    val End(foo1_e: DISimple) = { assertTrue(ic.advance); aacc }
    ic.popTRD()
    ic.pushTRD(baz1ERD)
    val Start(baz1_s: DISimple) = { assertTrue(ic.advance); aacc }
    val End(baz1_e: DISimple) = { assertTrue(ic.advance); aacc }; assertNotNull(baz1_e)
    ic.popTRD()
    ic.popTRD()
    val End(bar1_e: DIComplex) = { assertTrue(ic.advance); aacc }
    ic.pushTRD(bar2ERD)
    val Start(bar2_s: DIComplex) = { assertTrue(ic.advance); aacc }
    ic.pushTRD(bar2SeqTRD)
    ic.pushTRD(foo2ERD)
    val Start(foo2_s: DISimple) = { assertTrue(ic.advance); aacc }
    val End(foo2_e: DISimple) = { assertTrue(ic.advance); aacc }
    ic.popTRD()
    ic.pushTRD(baz2ERD)
    val Start(baz2_s: DISimple) = { assertTrue(ic.advance); aacc }
    val End(baz2_e: DISimple) = { assertTrue(ic.advance); aacc }; assertNotNull(baz2_e)
    ic.popTRD()
    ic.popTRD()
    val End(bar2_e: DIComplex) = { assertTrue(ic.advance); aacc }
    ic.popTRD()
    val End(quux_e: DIComplex) = { assertTrue(ic.advance); aacc }
    assertFalse(ic.inspect)
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
    val (ic, rootERD) = infosetInputter(sch, infosetXML)
    val aacc = ic.advanceAccessor
    val barERD = rootERD
    val Some(barSeqTRD: SequenceRuntimeData) = barERD.optComplexTypeModelGroupRuntimeData
    val Seq(fooERD) = barERD.childERDs
    ic.pushTRD(barERD)
    val Start(bar_s: DIComplex) = { assertTrue(ic.advance); aacc }
    ic.pushTRD(barSeqTRD)
    ic.pushTRD(fooERD)
    val StartArray(foo_arr_s) = { assertTrue(ic.advance); aacc }
    val Start(foo_1_s: DISimple) = { assertTrue(ic.advance); aacc }
    val End(foo_1_e: DISimple) = { assertTrue(ic.advance); aacc }
    val Start(foo_2_s: DISimple) = { assertTrue(ic.advance); aacc }
    val End(foo_2_e: DISimple) = { assertTrue(ic.advance); aacc }
    val EndArray(foo_arr_e) = { assertTrue(ic.advance); aacc }
    ic.popTRD()
    val End(bar_e: DIComplex) = { assertTrue(ic.advance); aacc }
    ic.popTRD()
    ic.popTRD()
    assertFalse(ic.inspect)
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
    val (ic, rootERD) = infosetInputter(sch, infosetXML)
    val aacc = ic.advanceAccessor
    val Start(bar_s: DIComplex) = { assertTrue(ic.advance); aacc }
    val barERD = bar_s.erd
    val Some(barSeqTRD: SequenceRuntimeData) = barERD.optComplexTypeModelGroupRuntimeData
    val Seq(fooERD, bazERD) = barERD.childERDs
    ic.pushTRD(barSeqTRD)
    ic.pushTRD(fooERD)
    val StartArray(foo_arr_s) = { assertTrue(ic.advance); aacc }
    val Start(foo_1_s: DISimple) = { assertTrue(ic.advance); aacc }
    val End(foo_1_e: DISimple) = { assertTrue(ic.advance); aacc }
    val Start(foo_2_s: DISimple) = { assertTrue(ic.advance); aacc }
    val End(foo_2_e: DISimple) = { assertTrue(ic.advance); aacc }
    val EndArray(foo_arr_e) = { assertTrue(ic.advance); aacc }
    ic.popTRD()
    ic.pushTRD(bazERD)
    val Start(baz_s: DISimple) = { assertTrue(ic.advance); aacc }
    val End(baz_e: DISimple) = { assertTrue(ic.advance); aacc }
    assertNotNull(baz_e)
    val End(bar_e: DIComplex) = { assertTrue(ic.advance); aacc }
    ic.popTRD()
    ic.popTRD()
    assertFalse(ic.inspect)
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
    val (ic, rootERD) = infosetInputter(sch, infosetXML)
    val aacc = ic.advanceAccessor
    val barERD = rootERD
    val Some(barSeqTRD: SequenceRuntimeData) = barERD.optComplexTypeModelGroupRuntimeData
    ic.pushTRD(barERD)
    val Start(bar_s: DIComplex) = { assertTrue(ic.advance); aacc }
    val Seq(bazERD, fooERD) = barERD.childERDs
    ic.pushTRD(barSeqTRD)
    ic.pushTRD(bazERD)
    val Start(baz_s: DISimple) = { assertTrue(ic.advance); aacc }
    val End(baz_e: DISimple) = { assertTrue(ic.advance); aacc }
    assertNotNull(baz_e)
    ic.popTRD()
    ic.pushTRD(fooERD)
    val StartArray(foo_arr_s) = { assertTrue(ic.advance); aacc }
    val Start(foo_1_s: DISimple) = { assertTrue(ic.advance); aacc }
    val End(foo_1_e: DISimple) = { assertTrue(ic.advance); aacc }
    val Start(foo_2_s: DISimple) = { assertTrue(ic.advance); aacc }
    val End(foo_2_e: DISimple) = { assertTrue(ic.advance); aacc }
    val EndArray(foo_arr_e) = { assertTrue(ic.advance); aacc }
    ic.popTRD()
    val End(bar_e: DIComplex) = { assertTrue(ic.advance); aacc }
    ic.popTRD()
    ic.popTRD()
    assertFalse(ic.inspect)
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
    val (ic, rootERD) = infosetInputter(sch, infosetXML)
    val aacc = ic.advanceAccessor
    val barERD = rootERD
    val Some(barSeqTRD: SequenceRuntimeData) = barERD.optComplexTypeModelGroupRuntimeData
    val Seq(bazERD, fooERD) = barERD.childERDs
    ic.pushTRD(barERD)
    val Start(bar_s: DIComplex) = { assertTrue(ic.advance); aacc }
    ic.pushTRD(barSeqTRD)
    ic.pushTRD(bazERD)
    val StartArray(baz_arr_s) = { assertTrue(ic.advance); aacc }
    val Start(baz_s: DISimple) = { assertTrue(ic.advance); aacc }
    val End(baz_e: DISimple) = { assertTrue(ic.advance); aacc }
    assertNotNull(baz_e)
    val EndArray(baz_arr_e) = { assertTrue(ic.advance); aacc }
    ic.popTRD()
    ic.pushTRD(fooERD)
    val StartArray(foo_arr_s) = { assertTrue(ic.advance); aacc }
    val Start(foo_1_s: DISimple) = { assertTrue(ic.advance); aacc }
    val End(foo_1_e: DISimple) = { assertTrue(ic.advance); aacc }
    val Start(foo_2_s: DISimple) = { assertTrue(ic.advance); aacc }
    val End(foo_2_e: DISimple) = { assertTrue(ic.advance); aacc }
    val EndArray(foo_arr_e) = { assertTrue(ic.advance); aacc }
    ic.popTRD()
    val End(bar_e: DIComplex) = { assertTrue(ic.advance); aacc }
    ic.popTRD()
    ic.popTRD()
    assertFalse(ic.inspect)
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
    val (ic, rootERD) = infosetInputter(sch, infosetXML)
    val aacc = ic.advanceAccessor
    val iacc = ic.inspectAccessor
    val barERD = rootERD
    val Some(barSeqTRD: SequenceRuntimeData) = barERD.optComplexTypeModelGroupRuntimeData
    val Seq(fooERD) = barERD.childERDs
    ic.pushTRD(barERD)
    val Start(bar_s1: DIComplex) = { assertTrue(ic.inspect); iacc }
    val Start(bar_s2: DIComplex) = { assertTrue(ic.inspect); iacc }
    val Start(bar_s3: DIComplex) = { assertTrue(ic.advance); aacc }
    ic.pushTRD(barSeqTRD)
    ic.pushTRD(fooERD)
    val Start(foo_s1: DISimple) = { assertTrue(ic.inspect); iacc }
    val Start(foo_s2: DISimple) = { assertTrue(ic.advance); aacc }
    val End(foo_e: DISimple) = { assertTrue(ic.advance); aacc }
    val End(bar_e1: DIComplex) = { assertTrue(ic.inspect); iacc }
    assertTrue(ic.inspect)
    ic.popTRD()
    val End(bar_e2: DIComplex) = { assertTrue(ic.advance); aacc }
    ic.popTRD()
    ic.popTRD()
    assertFalse(ic.inspect)
    assertTrue(bar_s1 eq bar_s2)
    assertTrue(bar_s2 eq bar_s3)
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
    val (ic, rootERD) = infosetInputter(sch, infosetXML)
    val aacc = ic.advanceAccessor
    val Start(e: DIComplex) = { assertTrue(ic.advance); aacc }
    val eERD = e.erd
    val Some(eSeqTRD: SequenceRuntimeData) = eERD.optComplexTypeModelGroupRuntimeData
    val Seq(sERD) = eERD.childERDs
    val Some(sChoTRD: ChoiceRuntimeData) = sERD.optComplexTypeModelGroupRuntimeData
    val Seq(c1ERD, c2ERD) = sERD.childERDs
    ic.pushTRD(eSeqTRD)
    ic.pushTRD(sERD)
    val StartArray(as) = { assertTrue(ic.advance); aacc }
    val Start(s1: DIComplex) = { assertTrue(ic.advance); aacc }; assertNotNull(s1)
    ic.pushTRD(sChoTRD)
    val Start(c1: DISimple) = { assertTrue(ic.advance); aacc }
    val End(c1e: DISimple) = { assertTrue(ic.advance); aacc }; assertNotNull(c1e)
    val End(s1e: DIComplex) = { assertTrue(ic.advance); aacc }; assertNotNull(s1e)
    ic.popTRD()
    val Start(s2: DIComplex) = { assertTrue(ic.advance); aacc }; assertNotNull(s2)
    ic.pushTRD(sChoTRD)
    val Start(c2: DISimple) = { assertTrue(ic.advance); aacc }
    val End(c2e: DISimple) = { assertTrue(ic.advance); aacc };; assertNotNull(c2e)
    val End(s2e: DIComplex) = { assertTrue(ic.advance); aacc }; assertNotNull(s2e)
    ic.popTRD()
    val EndArray(ase) = { assertTrue(ic.advance); aacc }
    val End(ee: DIComplex) = { assertTrue(ic.advance); aacc }
    ic.popTRD()
    ic.popTRD()
    assertFalse(ic.inspect)
    assertTrue(as eq ase) // exact same object
    assertTrue(e eq ee)
    assertTrue(c1.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(c1.dataValueAsString =:= "Hello")
    assertTrue(c2.dataValue.getAnyRef.isInstanceOf[String])
    assertTrue(c2.dataValueAsString =:= "World")
  }

}
