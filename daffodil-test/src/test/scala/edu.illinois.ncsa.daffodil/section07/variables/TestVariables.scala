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

class TestVariables extends JUnitSuite {
  val testDir = "/edu.illinois.ncsa.daffodil/section07/variables/"
  val tdml = testDir + "variables.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_setVar1() { runner.runOneTest("setVar1") }
  @Test def test_doubleSetErr() { runner.runOneTest("doubleSetErr") }
  @Test def test_setAfterReadErr() { runner.runOneTest("setAfterReadErr") }
  @Test def test_setVarChoice() { runner.runOneTest("setVarChoice") }
  @Test def test_setVarOnSeqAndElemRef() { runner.runOneTest("setVarOnSeqAndElemRef") }
  @Test def test_setVarOnGroupRef() { runner.runOneTest("setVarOnGroupRef") }
  @Test def test_setVarSimpleType() { runner.runOneTest("setVarSimpleType") }
  
  @Test def test_setVarValAttribute() { runner.runOneTest("setVarValAttribute") }
  @Test def test_setVarValAttribute2() { runner.runOneTest("setVarValAttribute2") }
//  @Test def test_setVarTypeMismatch() { runner.runOneTest("setVarTypeMismatch") }

}
