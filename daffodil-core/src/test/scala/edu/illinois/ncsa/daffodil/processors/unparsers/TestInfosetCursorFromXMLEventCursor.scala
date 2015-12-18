package edu.illinois.ncsa.daffodil.processors.unparsers

import org.junit.Test
import org.junit.Assert._
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.equality._
import scala.io.Source
import edu.illinois.ncsa.daffodil.util.TestUtils
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import edu.illinois.ncsa.daffodil.processors.DataProcessor
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.processors.DIComplex
import edu.illinois.ncsa.daffodil.processors.DIArray
import edu.illinois.ncsa.daffodil.util.IteratorFromCursor
/**
 * All these tests were written for iterator style.
 * Now that we're doing Cursor style, need an adapter. otherwise we have to edit them all.
 */
case class Adapter(isrc: InfosetSource)
  extends IteratorFromCursor[InfosetEvent, InfosetEvent](isrc, (ie: InfosetEvent) => ie)

class TestInfosetCursorFromXMLEventCursor1 {

  def infosetSource(testSchema: scala.xml.Node, infosetXML: scala.xml.Node) = {
    val compiler = Compiler()
    val pf = compiler.compileNode(testSchema)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val u = pf.onPath("/").asInstanceOf[DataProcessor]
    if (u.isError) {
      val msgs = u.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val xmlEventCursor = XMLUtils.nodeToXMLEventCursor(infosetXML)
    val rootERD = u.ssrd.elementRuntimeData
    val is = Adapter(InfosetSource.fromXMLSource(xmlEventCursor, rootERD))
    is
  }

  @Test def testUnparseFixedLengthString1() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>)
    val infosetXML = <foo xmlns={ XMLUtils.EXAMPLE_NAMESPACE }>Hello</foo>
    TestUtils.testUnparsing(sch, infosetXML, "Hello")
  }

  @Test def testInfosetSource1() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="foo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>)
    val infosetXML = <foo xmlns={ XMLUtils.EXAMPLE_NAMESPACE }>Hello</foo>
    val is = infosetSource(sch, infosetXML).toStream.toList
    val List(Start(s: DISimple), End(e: DISimple)) = is
    assertTrue(s eq e) // exact same object
    assertTrue(s.dataValue.isInstanceOf[String])
    assertTrue(s.dataValueAsString =:= "Hello")
    assertEquals(s.toXML(), infosetXML)
  }

  @Test def testInfosetSourceNil1() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element nillable="true" dfdl:nilValue="nil" dfdl:nilKind="literalValue" name="foo" dfdl:lengthKind="explicit" dfdl:length="3" type="xs:string"/>)
    val infosetXML = <foo xsi:nil="true" xmlns={ XMLUtils.EXAMPLE_NAMESPACE } xmlns:xsi={ XMLUtils.XSI_NAMESPACE }/>
    val is = infosetSource(sch, infosetXML).toStream.toList
    val List(Start(s: DISimple), End(e: DISimple)) = is
    assertTrue(s eq e) // exact same object
    assertTrue(s.isNilled)
    assertEquals(s.toXML(), infosetXML)
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
    val is = infosetSource(sch, infosetXML)
    val Start(bar_s: DIComplex) = is.next
    val Start(foo_s: DISimple) = is.next
    val End(foo_e: DISimple) = is.next
    val End(bar_e: DIComplex) = is.next
    assertFalse(is.hasNext)
    assertTrue(bar_s eq bar_e) // exact same object
    assertTrue(foo_s eq foo_e)
    assertTrue(foo_s.dataValue.isInstanceOf[String])
    assertTrue(foo_s.dataValueAsString =:= "Hello")
    assertEquals(bar_s.toXML(), infosetXML)
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
    val is = infosetSource(sch, infosetXML)
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
    assertEquals(bar_s.toXML(), infosetXML)
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
    val is = infosetSource(sch, infosetXML)
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
    TestUtils.assertEqualsXMLElements(quux_s.toXML()(0), infosetXML)
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
    val is = infosetSource(sch, infosetXML)
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
    assertEquals(bar_s.toXML(), infosetXML)
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
    val is = infosetSource(sch, infosetXML)
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
    assertEquals(bar_s.toXML(), infosetXML)
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
    val is = infosetSource(sch, infosetXML)
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
    assertEquals(bar_s.toXML(), infosetXML)
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
    val is = infosetSource(sch, infosetXML)
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
    assertEquals(bar_s.toXML(), infosetXML)
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
    val is = infosetSource(sch, infosetXML)
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
    assertEquals(bar_s1.toXML(), infosetXML)
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
    val is = infosetSource(sch, infosetXML)
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
