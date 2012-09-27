package daffodil.section16.array_optional_elem

import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestArrayOptionalElem extends JUnitSuite {
  val testDir = "/daffodil/section16/array_optional_elem/"
  val aa = testDir + "ArrayOptionalElem.tdml"
  
  lazy val runner= new DFDLTestSuite(Misc.getRequiredResource(aa))
  @Test def test_optionalElem() { runner.runOneTest("optionalElem")}
  
  val testDir01 = "/daffodil/section05/facets/"
  val ab = testDir01 + "Facets.tdml"
  lazy val runner01 = new DFDLTestSuite(Misc.getRequiredResource(ab))
  @Test def test_leftOverData_Neg() { runner01.runOneTest("leftOverData_Neg") }
}