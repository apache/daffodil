package edu.illinois.ncsa.daffodil.section05.dfdl_xsdl_subset

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

import edu.illinois.ncsa.daffodil.debugger.Debugger

class TestDFDLSubset extends JUnitSuite {
  val testDir = "/edu.illinois.ncsa.daffodil/section05/dfdl_xsdl_subset/"
  val tdml = testDir + "DFDLSubset.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_groupRefGroupRef() { { runner.runOneTest("groupRefGroupRef") } }
  @Test def test_refInitiator3() { { runner.runOneTest("refInitiator3") } }
  @Test def test_groupRef() { { runner.runOneTest("groupRef") } }
  @Test def test_groupRefChoice() { runner.runOneTest("groupRefChoice") }
  @Test def test_badGroupRef() { { runner.runOneTest("badGroupRef") } }
  
  @Test def test_groupRefDFDL() { runner.runOneTest("groupRefDFDL") }
}
