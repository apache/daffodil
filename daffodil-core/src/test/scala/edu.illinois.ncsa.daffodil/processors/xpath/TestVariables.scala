package edu.illinois.ncsa.daffodil.processors.xpath

import scala.math.Pi
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.xml._
import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import javax.xml.xpath.XPathConstants
import edu.illinois.ncsa.daffodil.debugger.Debugger
import org.junit.Test

class TestVariables extends JUnitSuite with WithParseErrorThrowing {
  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example

  val context: SchemaComponent = Fakes.fakeSD

  //  @Test def testVariables1() {
  //
  //    val text = new org.jdom.Text("19")
  //    val root = new org.jdom.Element("root")
  //    root addContent (text)
  //    val document = new org.jdom.Document(root)
  //    val decl = new DFDLDefineVariable(<dfdl:defineVariable name="pi" type="xs:double"/>, context.schemaDocument)
  //    val expName = XMLUtils.expandedQName(XMLUtils.EXAMPLE_NAMESPACE, "pi")
  //    val variable = // new VariableMap() defineVariable("pi", XMLUtils.XSD_DOUBLE, new Namespaces())
  //      Variable(decl, expName, "xs:string", Some(Pi.toString), false, context.schemaDocument)
  //
  //    val varMap = new VariableMap(List((expName, variable)).toMap)
  //    varMap.setVariable(null, expName, Pi.toString)
  //
  //    withParseErrorThrowing(null) {
  //    val ns = List(org.jdom.Namespace.getNamespace("ex", XMLUtils.EXAMPLE_NAMESPACE))
  //    val result = XPathUtil evalExpressionFromString ("$ex:pi", varMap, root, ns, XPathConstants.NUMBER)
  //
  //    result match {
  //      case NumberResult(x) => assertEquals(Pi, x)
  //      case _ => fail
  //    }
  //    null // wants us to return a PState. Don't want to bother creating one.
  //    }
  //  }

  val variables2 =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema">
        <dfdl:format ref="tns:daffodilTest1"/>
        <dfdl:defineVariable name="pi" type="xs:double" defaultValue={ Pi.toString }/>
        <xs:element name="data" type="xs:double" dfdl:inputValueCalc="{ $tns:pi }"/>
      </tdml:defineSchema>
      <tdml:parserTestCase name="testVariables2" root="data" model="mySchema">
        <tdml:document/>
        <tdml:infoset>
          <tdml:dfdlInfoset>
            <tns:data>3.141592653589793</tns:data>
          </tdml:dfdlInfoset>
        </tdml:infoset>
      </tdml:parserTestCase>
    </tdml:testSuite>

  @Test def testVariables2() {
    val testSuite = variables2
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testVariables2")
  }

  val variables3 =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={ tns } xmlns:tdml={ tdml } xmlns:dfdl={ dfdl } xmlns:xsd={ xsd } xmlns:xs={ xsd } xmlns:xsi={ xsi }>
      <tdml:defineSchema name="mySchema">
        <dfdl:format ref="tns:daffodilTest1"/>
        <dfdl:defineVariable name="x" type="xs:double"/>
        <xs:element name="data">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="e1" type="xs:double" dfdl:lengthKind="delimited">
                <xs:annotation>
                  <xs:appinfo source="http://www.ogf.org/dfdl/dfdl-1.0/">
                    <dfdl:setVariable ref="tns:x" value="{ . }"/>
                  </xs:appinfo>
                </xs:annotation>
              </xs:element>
              <xs:element name="e2" type="xs:double" dfdl:inputValueCalc="{ $tns:x - 0.141592653589793 }"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      </tdml:defineSchema>
      <tdml:parserTestCase name="testVariables3" root="data" model="mySchema">
        <tdml:document>3.141592653589793</tdml:document>
        <tdml:infoset>
          <tdml:dfdlInfoset>
            <tns:data><tns:e1>3.141592653589793</tns:e1><tns:e2>3.0</tns:e2></tns:data>
          </tdml:dfdlInfoset>
        </tdml:infoset>
      </tdml:parserTestCase>
    </tdml:testSuite>

  @Test def testVariables3() {
    // Debugger.setDebugging(true)
    val testSuite = variables3
    lazy val ts = new DFDLTestSuite(testSuite)
    ts.runOneTest("testVariables3")
  }

}
