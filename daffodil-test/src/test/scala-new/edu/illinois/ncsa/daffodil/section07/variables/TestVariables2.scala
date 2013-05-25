package edu.illinois.ncsa.daffodil.section07.variables
import edu.illinois.ncsa.daffodil.xml._
import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import org.junit.Test
import scala.math.Pi

class TestVariables2 extends WithParseErrorThrowing {
  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example

  val context: SchemaComponent = Fakes.fakeSD

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
