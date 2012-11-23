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

class TestVariables extends JUnitSuite {
  val testDir = "/daffodil/section07/variables/"
  val tdml = testDir + "variables.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_setVar1() { runner.runOneTest("setVar1") }
  @Test def test_doubleSetErr() { runner.runOneTest("doubleSetErr") }
  @Test def test_setAfterReadErr() { runner.runOneTest("setAfterReadErr") }
  @Test def test_setVarChoice() { runner.runOneTest("setVarChoice") }
  @Test def test_setVarOnSeqAndElemRef() { runner.runOneTest("setVarOnSeqAndElemRef") }
  @Test def test_setVarOnGroupRef() { runner.runOneTest("setVarOnGroupRef") }

}