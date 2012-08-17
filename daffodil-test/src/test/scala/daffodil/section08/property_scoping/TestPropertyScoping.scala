package daffodil.section08.property_scoping

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestPropertyScoping extends JUnit3Suite {
  val testDir = "/daffodil/section08/property_scoping/"
  val aa = testDir + "PropertyScoping.tdml"
  val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
  
  def test_property_scoping_01() { runner.runOneTest("property_scoping_01") }
  
  val tdml = testDir + "PropertyScoping_01.tdml"
  val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml))
  
  def test_property_scoping_02() { runner_01.runOneTest("property_scoping_02") }
  //def test_property_scoping_03() { runner_01.runOneTest("property_scoping_03") }
  def test_property_scoping_04() { runner_01.runOneTest("property_scoping_04") }
  def test_property_scoping_05() { runner_01.runOneTest("property_scoping_05") }
  def testNearestEnclosingSequenceElementRef() { runner_01.runOneTest("NearestEnclosingSequenceElementRef") }
}
