package daffodil.section23.dfdl_functions

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

class TestDFDLFunctionsNew extends JUnitSuite {
  val testDir = "/daffodil/section23/dfdl_functions/"
  val aa = testDir + "Functions.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_testBit_0() { runner.runOneTest("testBit_0") }
  @Test def test_setBits_0() { runner.runOneTest("setBits_0") }
  @Test def test_occursCount_0() { runner.runOneTest("occursCount_0") }
  @Test def test_stringLiteralFromString_0() { runner.runOneTest("stringLiteralFromString_0") }
  @Test def test_containsEntity_0() { runner.runOneTest("containsEntity_0") }



}