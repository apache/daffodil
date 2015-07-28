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

class TestUnparserAugmentingInfosetSource {

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

  @Test def testUnparsingDefaultSDEFixedLength1 {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <!-- below is an error because explicit length 5 cannot have a default value because it can never be empty -->
            <xs:element name="defaultable" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="5" default="hello"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val infosetXML = <root xmlns={ XMLUtils.EXAMPLE_NAMESPACE }></root>
    val x = intercept[Exception] {
      TestUtils.testUnparsing(sch, infosetXML, "hello")
    }
    val expectedText = "cannot have XSD default='hello'"
    val Some(msgs) = DiagnosticUtils.getSomeMessage(x)
    assertTrue(msgs.toLowerCase.contains(expectedText.toLowerCase))
  }

  @Test def testUnparsingDefaultSDEImplicitStringLength1 {
    val sch = SchemaUtils.dfdlTestSchema(
      <dfdl:format ref="tns:daffodilTest1"/>,
      <xs:element name="root" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <!-- below is an error because implicit length string maxLength 5 can never be empty -->
            <xs:element name="defaultable" dfdl:lengthKind="implicit" default="hello">
              <xs:simpleType>
                <xs:restriction base="xs:string">
                  <xs:maxLength value="5"/>
                  <xs:minLength value="5"/>
                </xs:restriction>
              </xs:simpleType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>)
    val infosetXML = <root xmlns={ XMLUtils.EXAMPLE_NAMESPACE }></root>
    val x = intercept[Exception] {
      TestUtils.testUnparsing(sch, infosetXML, "hello")
    }
    val expectedText = "cannot have XSD default='hello'"
    val Some(msgs) = DiagnosticUtils.getSomeMessage(x)
    assertTrue(msgs.toLowerCase.contains(expectedText.toLowerCase))
  }

}