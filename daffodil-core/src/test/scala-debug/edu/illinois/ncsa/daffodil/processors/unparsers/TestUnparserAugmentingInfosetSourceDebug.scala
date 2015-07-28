package edu.illinois.ncsa.daffodil.processors.unparsers

import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.util._
import scala.xml._
import edu.illinois.ncsa.daffodil.compiler._
import junit.framework.Assert.assertEquals
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom.GlobalElementDecl
import edu.illinois.ncsa.daffodil.processors.DataProcessor
import edu.illinois.ncsa.daffodil.processors.DIComplex
import edu.illinois.ncsa.daffodil.processors.DISimple
import edu.illinois.ncsa.daffodil.processors.OutputValueCalcEvaluationException
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils

class TestUnparserAugmentingInfosetSourceDebug {

  def infosetRootERDAndSource(testSchema: scala.xml.Node, infosetXML: scala.xml.Node) = {
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
    val xmlEventReader = XMLUtils.nodeToXMLEventReader(infosetXML)
    val rootERD = u.ssrd.elementRuntimeData
    val infosetSource = InfosetSource.fromXMLSource(xmlEventReader, rootERD)
    val augmentingSource = new UnparserAugmentingInfosetSource(rootERD, infosetSource)
    (rootERD, augmentingSource)
  }

  @Test def testUnparserAugment1() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="ovc1" dfdl:outputValueCalc='{ "Hello" }' dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val infosetXML = <root xmlns={ XMLUtils.EXAMPLE_NAMESPACE }></root>
    val (rootERD, is) = infosetRootERDAndSource(sch, infosetXML)
    val Seq(ovc1ERD) = rootERD.childERDs
    val Start(root_s: DIComplex) = is.next
    val Start(ovc1_s: DISimple) = is.next
    val End(ovc1_e: DISimple) = is.next
    val End(root_e: DIComplex) = is.next
    assertFalse(is.hasNext)
    assertTrue(root_s eq root_e) // exact same object
    assertTrue(root_s.runtimeData =:= rootERD)
    assertTrue(ovc1_s eq ovc1_e)
    assertTrue(ovc1_s.runtimeData =:= ovc1ERD)
    assertTrue(ovc1_s.dataValue.isInstanceOf[String])
    assertTrue(ovc1_s.dataValueAsString =:= "Hello")
  }

  @Test def testUnparserAugment2() {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root" xmlns:ex={ XMLUtils.EXAMPLE_NAMESPACE }>
        <xs:complexType>
          <xs:sequence>
            <xs:element name="before1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="ovc1" dfdl:outputValueCalc='{ ../ex:before1 }' dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val infosetXML = <root xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><before1>Hello</before1></root>
    val (rootERD, is) = infosetRootERDAndSource(sch, infosetXML)
    val Seq(_, ovc1ERD) = rootERD.childERDs
    val Start(root_s: DIComplex) = is.next
    val Start(before1_s: DISimple) = is.next
    val End(before1_e: DISimple) = is.next
    val Start(ovc1_s: DISimple) = is.next
    val End(ovc1_e: DISimple) = is.next
    val End(root_e: DIComplex) = is.next
    assertFalse(is.hasNext)
    assertTrue(root_s eq root_e) // exact same object
    assertTrue(root_s.runtimeData =:= rootERD)
    assertTrue(ovc1_s eq ovc1_e)
    assertTrue(ovc1_s.runtimeData =:= ovc1ERD)
    val OutputValueCalcEvaluationException(`ovc1ERD`) =
      intercept[OutputValueCalcEvaluationException] {
        assertTrue(ovc1_s.dataValue.isInstanceOf[String])
      }
    val Some(ovcExpr) = ovc1ERD.outputValueCalcExpr
    assertTrue(!ovcExpr.isConstant)
    assertEquals("{ ../ex:before1 }", ovcExpr.prettyExpr)
  }

  @Test def testUnparsingOVC1 {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="ovc1" dfdl:outputValueCalc='{ "Hello" }' dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val infosetXML = <root xmlns={ XMLUtils.EXAMPLE_NAMESPACE }></root>
    TestUtils.testUnparsing(sch, infosetXML, "Hello")
  }

  @Test def testUnparsingOVCWithLookback2 {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root" xmlns:ex={ XMLUtils.EXAMPLE_NAMESPACE }>
        <xs:complexType>
          <xs:sequence>
            <xs:element name="before1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="ovc1" dfdl:outputValueCalc='{ ../ex:before1 }' dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val infosetXML = <root xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><before1>Hello</before1></root>
    TestUtils.testUnparsing(sch, infosetXML, "HelloHello")
  }

  @Test def testUnparsingOVCWithLookAhead3 {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root" xmlns:ex={ XMLUtils.EXAMPLE_NAMESPACE }>
        <xs:complexType>
          <xs:sequence>
            <xs:element name="ovc1" dfdl:outputValueCalc='{ ../ex:after1 }' dfdl:lengthKind="explicit" dfdl:length="5" type="xs:string"/>
            <xs:element name="after1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="5"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val infosetXML = <root xmlns={ XMLUtils.EXAMPLE_NAMESPACE }><after1>Hello</after1></root>
    TestUtils.testUnparsing(sch, infosetXML, "HelloHello")
  }

}