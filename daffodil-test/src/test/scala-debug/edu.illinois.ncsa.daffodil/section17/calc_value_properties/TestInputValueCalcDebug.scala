package edu.illinois.ncsa.daffodil.section17.calc_value_properties

import java.io.File
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import scala.xml._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.LoggingDefaults
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.debugger.Debugger

class TestInputValueCalcDebug extends JUnitSuite {
  val testDir = "/edu.illinois.ncsa.daffodil/section17/calc_value_properties/"
  val tdml = testDir + "inputValueCalc.tdml"

  lazy val runner = { new DFDLTestSuite(Misc.getRequiredResource(tdml)) }


  val aq = testDir + "AQ.tdml"
  lazy val runnerAQ = new DFDLTestSuite(Misc.getRequiredResource(aq))
  @Test def test_AQ001() { runnerAQ.runOneTest("AQ001") }

  // @Test def test_InputValueCalc_04() { runner.runOneTest("InputValueCalc_04")}
}
