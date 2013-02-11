package edu.illinois.ncsa.daffodil.section07.variables

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

class TestVariablesDebug extends JUnitSuite {
  val testDir = "/edu.illinois.ncsa.daffodil/section07/variables/"
  val tdml = testDir + "variables.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_varInstance() { runner.runOneTest("varInstance") }
  @Test def test_varAsSeparator() { runner.runOneTest("varAsSeparator") }
  @Test def test_setVarTypeMismatch() { runner.runOneTest("setVarTypeMismatch") }

  val tdml_01 = testDir + "variables_01.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))

  @Test def test_setVar1_d() { runner_01.runOneTest("setVar1_d") }
  @Test def test_doubleSetErr_d() { runner_01.runOneTest("doubleSetErr_d") }
  @Test def test_setAfterReadErr_d() { runner_01.runOneTest("setAfterReadErr_d") }
  //@Test def test_var_01() { runner_01.runOneTest("var_01") }

}
