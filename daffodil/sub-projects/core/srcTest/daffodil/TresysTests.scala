package daffodil

import java.io.File

import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert._

import daffodil.xml.XMLUtil
import daffodil.xml.XMLUtil._
import scala.xml._
import daffodil.dsom.Compiler

import tdml.DFDLTestSuite

class TresysTests extends JUnit3Suite {
  val testDir = "./test-suite/tresys-contributed/"
  val tdml1 = testDir + "AA.tdml"
  val runner1 = new DFDLTestSuite(new File(tdml1))

  def test_AA000() { runner1.runOneTest("AA000") }

  // Test related to Jira task DFDL-76
  def testSchemaReferentialIntegrityChecking() {
    // Schema below should error out, because name 'bar' isn't a valid internal reference to the type. It should
    // be caught as in the xsd namespace, which won't allow it to match the targetNS.
    val realSchema = <schema xmlns={ xsdURI } xmlns:dfdl={ dfdlURI } xmlns:xsi={ xsiURI } xmlns:tns={ targetNS } targetNamespace={ targetNS }>
                       <annotation>
                         <appinfo source={ dfdlURI }>
                           <dfdl:format representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" terminator="" separator="" ignoreCase="no" textNumberRep="standard"/>
                         </appinfo>
                       </annotation>
                       <element name="foo" type="bar"/>
                       <complexType name="bar">
                         <sequence/>
                       </complexType>
                     </schema>
    val realSchemaText = realSchema.toString()
    val sch = XML.loadString(realSchemaText)
    val exc = intercept[Exception] {
       val actual = Compiler.testString(sch, "")
    }
    val m = exc.getMessage()
    assertTrue(m.contains("bar"))
  }
}