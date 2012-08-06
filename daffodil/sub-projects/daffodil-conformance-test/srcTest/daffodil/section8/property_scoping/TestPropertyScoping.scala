package daffodil.section8.property_scoping

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
  val testDir = "srcTest/daffodil/section8/property_scoping/"
  val aa = testDir + "PropertyScoping.tdml"
  val runner = new DFDLTestSuite(new File(aa))
  
  def test_property_scoping_01() { runner.runOneTest("property_scoping_01") }
}
