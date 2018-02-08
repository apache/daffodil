/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.infoset

import org.junit.Test
import org.junit.Assert._
import org.apache.daffodil.xml._
import org.apache.daffodil.Implicits._
import org.apache.daffodil.equality._
import org.apache.daffodil.util.TestUtils
import org.apache.daffodil.util.SchemaUtils
import org.apache.daffodil.processors.DataProcessor
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.util.IteratorFromCursor

/**
 * All these tests were written for iterator style.
 * Now that we're doing Cursor style, need an adapter. otherwise we have to edit them all.
 */
case class Adapter(isrc: InfosetInputter)
  extends IteratorFromCursor[InfosetAccessor, InfosetAccessor](isrc, (ie: InfosetAccessor) => ie)

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
    val inputter = new ScalaXMLInfosetInputter(infosetXML)
    inputter.initialize(rootERD, u.getTunables())
    val is = Adapter(inputter)
    is
  }

  @Test def testUnparseFixedLengthString1() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>)
    val infosetXML = <foo xmlns={ XMLUtils.EXAMPLE_NAMESPACE }>Hello</foo>
    TestUtils.testUnparsing(sch, infosetXML, "Hello")
  }

  @Test def testInfosetInputter1() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>)
    val infosetXML = <foo xmlns={ XMLUtils.EXAMPLE_NAMESPACE }>Hello</foo>
    val is = infosetInputter(sch, infosetXML).toStream.toList
    val List(Start(s: DISimple), End(e: DISimple)) = is
    assertTrue(s eq e) // exact same object
    assertTrue(s.dataValue.isInstanceOf[String])
    assertTrue(s.dataValueAsString =:= "Hello")
  }

  @Test def testInfosetInputterNil1() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element nillable="true" dfdl:nilValue="nil" dfdl:nilKind="literalValue" name="foo" dfdl:lengthKind="explicit" dfdl:length="3" type="xs:string"/>)
    val infosetXML = <foo xsi:nil="true" xmlns={ XMLUtils.EXAMPLE_NAMESPACE } xmlns:xsi={ XMLUtils.XSI_NAMESPACE }/>
    val is = infosetInputter(sch, infosetXML).toStream.toList
    val List(Start(s: DISimple), End(e: DISimple)) = is
    assertTrue(s eq e) // exact same object
    assertTrue(s.isNilled)
  }

  @Test def testInfosetComplex1() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val infosetXML = <bar xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><foo>Hello</foo></bar>
    val is = infosetInputter(sch, infosetXML)
    val Start(bar_s: DIComplex) = is.next
    val Start(foo_s: DISimple) = is.next
    val End(foo_e: DISimple) = is.next
    val End(bar_e: DIComplex) = is.next
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_s eq foo_e)
    assertTrue(foo_s.dataValue.isInstanceOf[String])
    assertTrue(foo_s.dataValueAsString =:= "Hello")
  }

  @Test def testInfosetComplex2() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
            <xs:element name="baz" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val infosetXML = <bar xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><foo>Hello</foo><baz>World</baz></bar>
    val is = infosetInputter(sch, infosetXML)
    val Start(bar_s: DIComplex) = is.next
    val Start(foo_s: DISimple) = is.next
    val End(foo_e: DISimple) = is.next
    val Start(baz_s: DISimple) = is.next
    val End(baz_e: DISimple) = is.next; assertNotNull(baz_e)
    val End(bar_e: DIComplex) = is.next
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_s eq foo_e)
    assertTrue(foo_s.dataValue.isInstanceOf[String])
    assertTrue(foo_s.dataValueAsString =:= "Hello")
    assertTrue(baz_s.dataValue.isInstanceOf[String])
    assertTrue(baz_s.dataValueAsString =:= "World")
  }

  @Test def testInfosetComplex3() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
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
      </xs:element>)
    val infosetXML = <quux xmlns={ XMLUtils.EXAMPLE_NAMESPACE }>
                       <bar1><foo1>Hello</foo1><baz1>World</baz1></bar1>
                       <bar2><foo2>Hello</foo2><baz2>World</baz2></bar2>
                     </quux>
    val is = infosetInputter(sch, infosetXML)
    val Start(quux_s: DIComplex) = is.next
    val Start(bar1_s: DIComplex) = is.next
    val Start(foo1_s: DISimple) = is.next
    val End(foo1_e: DISimple) = is.next
    val Start(baz1_s: DISimple) = is.next
    val End(baz1_e: DISimple) = is.next; assertNotNull(baz1_e)
    val End(bar1_e: DIComplex) = is.next
    val Start(bar2_s: DIComplex) = is.next
    val Start(foo2_s: DISimple) = is.next
    val End(foo2_e: DISimple) = is.next
    val Start(baz2_s: DISimple) = is.next
    val End(baz2_e: DISimple) = is.next; assertNotNull(baz2_e)
    val End(bar2_e: DIComplex) = is.next
    val End(quux_e: DIComplex) = is.next
    assertFalse(is.hasNext)
    assertTrue(bar1_s eq bar1_e) // exact same object
    assertTrue(foo1_s eq foo1_e)
    assertTrue(foo1_s.dataValue.isInstanceOf[String])
    assertTrue(foo1_s.dataValueAsString =:= "Hello")
    assertTrue(baz1_s.dataValue.isInstanceOf[String])
    assertTrue(baz1_s.dataValueAsString =:= "World")
    assertTrue(bar2_s eq bar2_e) // exact same object
    assertTrue(foo2_s eq foo2_e)
    assertTrue(foo2_s.dataValue.isInstanceOf[String])
    assertTrue(foo2_s.dataValueAsString =:= "Hello")
    assertTrue(baz2_s.dataValue.isInstanceOf[String])
    assertTrue(baz2_s.dataValueAsString =:= "World")
    assertTrue(quux_s eq quux_e)
  }

  @Test def testInfosetArray1() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val infosetXML = <bar xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><foo>Hello</foo><foo>World</foo></bar>
    val is = infosetInputter(sch, infosetXML)
    val Start(bar_s: DIComplex) = is.next
    val Start(foo_arr_s: DIArray) = is.next
    val Start(foo_1_s: DISimple) = is.next
    val End(foo_1_e: DISimple) = is.next
    val Start(foo_2_s: DISimple) = is.next
    val End(foo_2_e: DISimple) = is.next
    val End(foo_arr_e: DIArray) = is.next
    val End(bar_e: DIComplex) = is.next
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_arr_s eq foo_arr_e)
    assertTrue(foo_1_s eq foo_1_e)
    assertTrue(foo_1_s.dataValue.isInstanceOf[String])
    assertTrue(foo_1_s.dataValueAsString =:= "Hello")
    assertTrue(foo_2_s eq foo_2_e)
    assertTrue(foo_2_s.dataValue.isInstanceOf[String])
    assertTrue(foo_2_s.dataValueAsString =:= "World")
  }

  @Test def testInfosetArray2() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
            <xs:element name="baz" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val infosetXML = <bar xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><foo>Hello</foo><foo>World</foo><baz>Yadda</baz></bar>
    val is = infosetInputter(sch, infosetXML)
    val Start(bar_s: DIComplex) = is.next
    val Start(foo_arr_s: DIArray) = is.next
    val Start(foo_1_s: DISimple) = is.next
    val End(foo_1_e: DISimple) = is.next
    val Start(foo_2_s: DISimple) = is.next
    val End(foo_2_e: DISimple) = is.next
    val End(foo_arr_e: DIArray) = is.next
    val Start(baz_s: DISimple) = is.next
    val End(baz_e: DISimple) = is.next
    assertNotNull(baz_e)
    val End(bar_e: DIComplex) = is.next
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_arr_s eq foo_arr_e)
    assertTrue(foo_1_s eq foo_1_e)
    assertTrue(foo_1_s.dataValue.isInstanceOf[String])
    assertTrue(foo_1_s.dataValueAsString =:= "Hello")
    assertTrue(foo_2_s eq foo_2_e)
    assertTrue(foo_2_s.dataValue.isInstanceOf[String])
    assertTrue(foo_2_s.dataValueAsString =:= "World")
    assertTrue(baz_s.dataValue.isInstanceOf[String])
    assertTrue(baz_s.dataValueAsString =:= "Yadda")
  }

  @Test def testInfosetArray3() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="baz" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
            <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val infosetXML = <bar xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><baz>Yadda</baz><foo>Hello</foo><foo>World</foo></bar>
    val is = infosetInputter(sch, infosetXML)
    val Start(bar_s: DIComplex) = is.next
    val Start(baz_s: DISimple) = is.next
    val End(baz_e: DISimple) = is.next
    assertNotNull(baz_e)
    val Start(foo_arr_s: DIArray) = is.next
    val Start(foo_1_s: DISimple) = is.next
    val End(foo_1_e: DISimple) = is.next
    val Start(foo_2_s: DISimple) = is.next
    val End(foo_2_e: DISimple) = is.next
    val End(foo_arr_e: DIArray) = is.next

    val End(bar_e: DIComplex) = is.next
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_arr_s eq foo_arr_e)
    assertTrue(foo_1_s eq foo_1_e)
    assertTrue(foo_1_s.dataValue.isInstanceOf[String])
    assertTrue(foo_1_s.dataValueAsString =:= "Hello")
    assertTrue(foo_2_s eq foo_2_e)
    assertTrue(foo_2_s.dataValue.isInstanceOf[String])
    assertTrue(foo_2_s.dataValueAsString =:= "World")
    assertTrue(baz_s.dataValue.isInstanceOf[String])
    assertTrue(baz_s.dataValueAsString =:= "Yadda")
  }

  @Test def testInfosetArray4() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="baz" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
            <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val infosetXML = <bar xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><baz>Yadda</baz><foo>Hello</foo><foo>World</foo></bar>
    val is = infosetInputter(sch, infosetXML)
    val Start(bar_s: DIComplex) = is.next
    val Start(baz_arr_s: DIArray) = is.next
    val Start(baz_s: DISimple) = is.next
    val End(baz_e: DISimple) = is.next
    assertNotNull(baz_e)
    val End(baz_arr_e: DIArray) = is.next
    val Start(foo_arr_s: DIArray) = is.next
    val Start(foo_1_s: DISimple) = is.next
    val End(foo_1_e: DISimple) = is.next
    val Start(foo_2_s: DISimple) = is.next
    val End(foo_2_e: DISimple) = is.next
    val End(foo_arr_e: DIArray) = is.next
    val End(bar_e: DIComplex) = is.next
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_arr_s eq foo_arr_e)
    assertTrue(baz_arr_s eq baz_arr_e)
    assertTrue(foo_1_s eq foo_1_e)
    assertTrue(foo_1_s.dataValue.isInstanceOf[String])
    assertTrue(foo_1_s.dataValueAsString =:= "Hello")
    assertTrue(foo_2_s eq foo_2_e)
    assertTrue(foo_2_s.dataValue.isInstanceOf[String])
    assertTrue(foo_2_s.dataValueAsString =:= "World")
    assertTrue(baz_s.dataValue.isInstanceOf[String])
    assertTrue(baz_s.dataValueAsString =:= "Yadda")
  }

  @Test def testInfosetComplexPeek1() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val infosetXML = <bar xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><foo>Hello</foo></bar>
    val is = infosetInputter(sch, infosetXML)
    val Start(bar_s1: DIComplex) = is.peek
    val Start(bar_s2: DIComplex) = is.peek
    val Start(bar_s3: DIComplex) = is.next
    val Start(foo_s1: DISimple) = is.peek
    val Start(foo_s2: DISimple) = is.next
    val End(foo_e: DISimple) = is.next
    val End(bar_e1: DIComplex) = is.peek
    assertTrue(is.hasNext)
    val End(bar_e2: DIComplex) = is.next
    assertFalse(is.hasNext)
    assertTrue(bar_s1 eq bar_s2)
    assertTrue(bar_s2 eq bar_s3)
    assertTrue(bar_e1 eq bar_e2)
    assertTrue(bar_s1 eq bar_e1) // exact same object
    assertTrue(foo_s1 eq foo_s2)
    assertTrue(foo_s1 eq foo_e)
    assertTrue(foo_s1.dataValue.isInstanceOf[String])
    assertTrue(foo_s1.dataValueAsString =:= "Hello")
  }

  @Test def testInfosetArrayComplex1() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
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
      </xs:element>)
    val infosetXML = <e xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><s><c1>Hello</c1></s><s><c2>World</c2></s></e>
    val is = infosetInputter(sch, infosetXML)
    val Start(e: DIComplex) = is.next
    val Start(as: DIArray) = is.next
    val Start(s1: DIComplex) = is.next; assertNotNull(s1)
    val Start(c1: DISimple) = is.next
    val End(c1e: DISimple) = is.next; assertNotNull(c1e)
    val End(s1e: DIComplex) = is.next; assertNotNull(s1e)
    val Start(s2: DIComplex) = is.next; assertNotNull(s2)
    val Start(c2: DISimple) = is.next
    val End(c2e: DISimple) = is.next; ; assertNotNull(c2e)
    val End(s2e: DIComplex) = is.next; assertNotNull(s2e)
    val End(ase: DIArray) = is.next
    val End(ee: DIComplex) = is.next
    assertFalse(is.hasNext)
    assertTrue(as eq ase) // exact same object
    assertTrue(e eq ee)
    assertTrue(c1.dataValue.isInstanceOf[String])
    assertTrue(c1.dataValueAsString =:= "Hello")
    assertTrue(c2.dataValue.isInstanceOf[String])
    assertTrue(c2.dataValueAsString =:= "World")
  }
}
