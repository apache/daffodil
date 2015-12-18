package edu.illinois.ncsa.daffodil.processors.unparsers

import org.junit.Test
import org.junit.Assert._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.equality._; object ENoWarnU1 { EqualitySuppressUnusedImportWarning() }
import scala.io.Source
import edu.illinois.ncsa.daffodil.util.SchemaUtils
import edu.illinois.ncsa.daffodil.processors.DataProcessor
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.processors.DIComplex
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.util.IteratorFromCursor

/*
 * These are all tests of default-value insertion.
 * But I determined that the approach to defaulting,
 * which was a schema-aware separate pass, can't really work
 * and isn't worth a whole pass on its own.
 *
 * So these tests will have to be revisited someday when we
 * have defaulting working.
 *
 * The way these tests work is broken. They are assuming that
 * defaulting is being inserted by the XMLEventCursor. It will
 * not be there. Defaulting will be done as part of Unparsing.
 */
class TestInfosetDefaultingInUnparser {

  @Test def testDefaultable = {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator=",">
            <xs:element name="foo" dfdl:lengthKind="delimited" type="xs:string" default="abcde"/>
            <xs:element name="afterFoo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val compiler = Compiler()
    val pf = compiler.compileNode(sch)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val u = pf.onPath("/").asInstanceOf[DataProcessor]
    if (u.isError) {
      val msgs = u.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }

    val source = Source.fromString(<bar xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><afterFoo>Hello</afterFoo></bar>.toString)

    val xmlEventCursor = new XMLEventCursorFromInput(source)
    val rootERD = u.ssrd.elementRuntimeData

    val is = Adapter(InfosetCursor.fromXMLEventCursor(xmlEventCursor, rootERD))

    val Start(bar_s: DIComplex) = is.next
    assertNotNull(bar_s)
    val Start(foo_s: DISimple) = is.next
    val End(foo_e: DISimple) = is.next
    assertNotNull(foo_e)
    val Start(afterFoo_s: DISimple) = is.next
    val End(afterFoo_e: DISimple) = is.next
    assertNotNull(afterFoo_e)
    val End(bar_e: DIComplex) = is.next
    assertNotNull(bar_e)

    assertEquals("abcde", foo_s.dataValue)
    assertEquals("Hello", afterFoo_s.dataValue)
  }

  @Test def testOutputValueCalc = {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="foo" type="xs:string" dfdl:outputValueCalc='{ "abcde" }' dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="afterFoo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val compiler = Compiler()
    val pf = compiler.compileNode(sch)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val u = pf.onPath("/").asInstanceOf[DataProcessor]
    if (u.isError) {
      val msgs = u.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }

    val source = Source.fromString(<bar xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><afterFoo>Hello</afterFoo></bar>.toString)

    val xmlEventCursor = new XMLEventCursorFromInput(source)
    val rootERD = u.ssrd.elementRuntimeData

    val is = Adapter(InfosetCursor.fromXMLEventCursor(xmlEventCursor, rootERD))

    val Start(bar_s: DIComplex) = is.next; assertNotNull(bar_s)
    val Start(foo_s: DISimple) = is.next
    val End(foo_e: DISimple) = is.next; assertNotNull(foo_e)
    val Start(afterFoo_s: DISimple) = is.next
    val End(afterFoo_e: DISimple) = is.next; assertNotNull(afterFoo_e)
    val End(bar_e: DIComplex) = is.next; assertNotNull(bar_e)

    assertTrue(foo_s.asInstanceOf[DISimple].hasValue) // has a value because expression is a constant
    assertTrue(foo_s.runtimeData.outputValueCalcExpr.isDefined)
    assertEquals("Hello", afterFoo_s.dataValue)
  }

  @Test def testOutputValueCalcAfterOptional = {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="beforeFoo" type="xs:string" dfdl:initiator="beforeFoo" dfdl:lengthKind="explicit" dfdl:length="5" minOccurs="0"/>
            <xs:element name="foo" type="xs:string" dfdl:outputValueCalc='{ "abcde" }' dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="afterFoo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val compiler = Compiler()
    val pf = compiler.compileNode(sch)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val u = pf.onPath("/").asInstanceOf[DataProcessor]
    if (u.isError) {
      val msgs = u.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }

    val source = Source.fromString(<bar xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><afterFoo>Hello</afterFoo></bar>.toString)

    val xmlEventCursor = new XMLEventCursorFromInput(source)
    val rootERD = u.ssrd.elementRuntimeData

    val is = Adapter(InfosetCursor.fromXMLEventCursor(xmlEventCursor, rootERD))

    val Start(bar_s: DIComplex) = is.next; assertNotNull(bar_s)
    val Start(foo_s: DISimple) = is.next
    val End(foo_e: DISimple) = is.next; assertNotNull(foo_e)
    val Start(afterFoo_s: DISimple) = is.next
    val End(afterFoo_e: DISimple) = is.next; assertNotNull(afterFoo_e)
    val End(bar_e: DIComplex) = is.next; assertNotNull(bar_e)

    assertTrue(foo_s.asInstanceOf[DISimple].hasValue) // has a value because expression is a constant
    assertEquals("abcde", foo_s.dataValue)
    assertTrue(foo_s.runtimeData.outputValueCalcExpr.isDefined)
    assertEquals("Hello", afterFoo_s.dataValue)
  }

  @Test def testMultipleOutputValueCalcAfterOptional = {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="bar" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="beforeFoo" type="xs:string" dfdl:initiator="beforeFoo" dfdl:lengthKind="explicit" dfdl:length="5" minOccurs="0"/>
            <xs:element name="foo" type="xs:string" dfdl:outputValueCalc='{ "abcde" }' dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="foo2" type="xs:string" default="fghij" dfdl:lengthKind="delimited" dfdl:terminator="!"/>
            <xs:element name="afterFoo" dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val compiler = Compiler()
    val pf = compiler.compileNode(sch)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val u = pf.onPath("/").asInstanceOf[DataProcessor]
    if (u.isError) {
      val msgs = u.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }

    val source = Source.fromString(<bar xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><afterFoo>Hello</afterFoo></bar>.toString)

    val xmlEventCursor = new XMLEventCursorFromInput(source)
    val rootERD = u.ssrd.elementRuntimeData

    val is = Adapter(InfosetCursor.fromXMLEventCursor(xmlEventCursor, rootERD))

    val Start(bar_s: DIComplex) = is.next; assertNotNull(bar_s)
    val Start(foo_s: DISimple) = is.next
    val End(foo_e: DISimple) = is.next; assertNotNull(foo_e)
    val Start(foo2_s: DISimple) = is.next
    val End(foo2_e: DISimple) = is.next; assertNotNull(foo2_e)
    val Start(afterFoo_s: DISimple) = is.next
    val End(afterFoo_e: DISimple) = is.next; assertNotNull(afterFoo_e)
    val End(bar_e: DIComplex) = is.next; assertNotNull(bar_e)

    assertEquals("foo2", foo2_s.erd.namedQName.local)
    assertEquals("fghij", foo2_s.dataValueAsString)
    assertTrue(foo_s.asInstanceOf[DISimple].hasValue) // constant expression, so has value.
    assertTrue(foo_s.runtimeData.outputValueCalcExpr.isDefined)
    assertEquals("Hello", afterFoo_s.dataValue)
  }

}
