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
import edu.illinois.ncsa.daffodil.util.TestUtils

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

  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.dfdlAppinfoSource // XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

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

}
