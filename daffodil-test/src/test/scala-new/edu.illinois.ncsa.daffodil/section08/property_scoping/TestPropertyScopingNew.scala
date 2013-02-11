package edu.illinois.ncsa.daffodil.section08.property_scoping

import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File

class TestPropertyScopingNew extends JUnitSuite {
  val testDir = "/edu.illinois.ncsa.daffodil/section08/property_scoping/"
  val aa = testDir + "PropertyScoping.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  val tdml = testDir + "PropertyScoping_01.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  // New tests DFDL-326, don't know if a requirement exists for these already
  @Test def test_property_scoping_07() { runner_01.runOneTest("property_scoping_07") }
  @Test def test_property_scoping_08() { runner_01.runOneTest("property_scoping_08") }
  @Test def test_property_scoping_09() { runner_01.runOneTest("property_scoping_09") }
  @Test def test_property_scoping_10() { runner_01.runOneTest("property_scoping_10") }
  @Test def test_property_scoping_11() { runner_01.runOneTest("property_scoping_11") }

  // Fixed DFDL-417
  @Test def test_property_scoping_03() = { runner_01.runOneTest("property_scoping_03") }
}