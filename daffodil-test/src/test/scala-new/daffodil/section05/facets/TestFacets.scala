package daffodil.section05.facets

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
import daffodil.debugger.Debugger

class TestFacetsNew extends JUnitSuite {
  val testDir = "/daffodil/section05/facets/"
  val aa = testDir + "Facets.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_arraysMinOccursZero() { runner.runOneTest("arraysMinOccursZero") }
  @Test def test_arraysOccurInRange_01() { runner.runOneTest("arraysOccursInRange_01") }
  @Test def test_arraysOccurInRange_02() { runner.runOneTest("arraysOccursInRange_02") }
  @Test def test_arraysOccurInRange_03() { runner.runOneTest("arraysOccursInRange_03") }
  @Test def test_arraysOccurInRange_04() { runner.runOneTest("arraysOccursInRange_04") }
  @Test def test_arraysOccurInRange_05() { runner.runOneTest("arraysOccursInRange_05") }
  @Test def test_arraysOccursOutOfRange_01() { runner.runOneTest("arraysOccursOutOfRange_01") }
  @Test def test_arraysOccursOutOfRange_02() { runner.runOneTest("arraysOccursOutOfRange_02") }

}
