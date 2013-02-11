package edu.illinois.ncsa.daffodil.section13.nillable

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
import edu.illinois.ncsa.daffodil.debugger.Debugger.withDebugger
import edu.illinois.ncsa.daffodil.debugger.Debugger

class TestNillableNew extends JUnitSuite {

  val testDir = "/edu.illinois.ncsa.daffodil/section13/nillable/"
  val aa = testDir + "nillable.tdml"
  lazy val runnerAA = new DFDLTestSuite(Misc.getRequiredResource(aa))
  //@Test def test_nil8() { runnerAA.runOneTest("nil8") }
  val ln = testDir + "literal-value-nils.tdml"
  lazy val runnerLN = new DFDLTestSuite(Misc.getRequiredResource(ln))
  @Test def test_padded_nils() = { runnerLN.runOneTest("test_padded_nils") }

  val testDir_01 = "/edu.illinois.ncsa.daffodil/section06/entities/"
  val entity = testDir_01 + "entities_01.tdml"

}
