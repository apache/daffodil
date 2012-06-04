package daffodil

import java.io.File

import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert._

import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import scala.xml._
import daffodil.dsom.Compiler

import tdml.DFDLTestSuite

class TresysTests extends JUnit3Suite {
  val testDir = "test-suite/tresys-contributed/"
  val aa = testDir + "AA.tdml"
  val runnerAA = new DFDLTestSuite(new File(aa))

  def test_AA000() { runnerAA.runOneTest("AA000") }
  
  val ab = testDir + "AB.tdml"
  val runnerAB = new DFDLTestSuite(new File(ab))
    
  def test_AB000() { runnerAB.runOneTest("AB000") }
  def test_AB001() { runnerAB.runOneTest("AB001") }
  def test_AB002() { runnerAB.runOneTest("AB002") }
  def test_AB003() { runnerAB.runOneTest("AB003") }
  def test_AB004() { runnerAB.runOneTest("AB004") }
  def test_AB005() { runnerAB.runOneTest("AB005") }
  
  val an = testDir + "AN.tdml"
  val runnerAN = new DFDLTestSuite(new File(an))
  
  def test_AN000() { runnerAN.runOneTest("AN000") }
  def test_AN001() { runnerAN.runOneTest("AN001") }
  
  /* Very big test data files, so each is in its own TDML file */
//  val ab6 = testDir + "AB006.tdml"
//  val runnerAB6 = new DFDLTestSuite(new File(ab6))
//  def test_AB006() { runnerAB6.runOneTest("AB006") }
//  val ab7 = testDir + "AB007.tdml"
//  val runnerAB7 = new DFDLTestSuite(new File(ab7))
//  def test_AB007() { runnerAB7.runOneTest("AB007") }
//  val ab8 = testDir + "AB008.tdml"
//  val runnerAB8 = new DFDLTestSuite(new File(ab8))
//  def test_AB008() { runnerAB8.runOneTest("AB008") }
//  val ab9 = testDir + "AB009.tdml"
//  val runnerAB9 = new DFDLTestSuite(new File(ab9))
//  def test_AB009() { runnerAB9.runOneTest("AB009") }
  
  val aj = testDir + "AJ.tdml"
  val runnerAJ = new DFDLTestSuite(new File(aj))
    
  def test_AJ000() { runnerAJ.runOneTest("AJ000") }
  def test_AJ001() { runnerAJ.runOneTest("AJ001") }
  
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