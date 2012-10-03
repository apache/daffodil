package daffodil.section07.variables

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

class TestVariablesDebug extends JUnitSuite {
  val testDir = "/daffodil/section07/variables/"
  val tdml = testDir + "variables.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))
  
  @Test def test_setVar1() { runner.runOneTest("setVar1") }
  @Test def test_doubleSetErr() { runner.runOneTest("doubleSetErr") }
  @Test def test_setAfterReadErr() { runner.runOneTest("setAfterReadErr") }
  
  val tdml_01 = testDir + "variables_01.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))
  
  @Test def test_setVar1_d() { runner_01.runOneTest("setVar1_d") }
  @Test def test_doubleSetErr_d() { runner_01.runOneTest("doubleSetErr_d") }
  @Test def test_setAfterReadErr_d() { runner_01.runOneTest("setAfterReadErr_d") }
  //@Test def test_var_01() { runner_01.runOneTest("var_01") }
  
  }