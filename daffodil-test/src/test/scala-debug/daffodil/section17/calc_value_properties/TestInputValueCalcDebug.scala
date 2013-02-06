package daffodil.section17.calc_value_properties

import java.io.File
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import junit.framework.Assert._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import scala.xml._
import daffodil.compiler.Compiler
import daffodil.tdml.DFDLTestSuite
import daffodil.util.LogLevel
import daffodil.util.LoggingDefaults
import daffodil.util.Misc
import daffodil.debugger.Debugger

class TestInputValueCalcDebug extends JUnitSuite {
  val testDir = "/daffodil/section17/calc_value_properties/"
  val tdml = testDir + "inputValueCalc.tdml"

  lazy val runner = { new DFDLTestSuite(Misc.getRequiredResource(tdml)) }


  val aq = testDir + "AQ.tdml"
  lazy val runnerAQ = new DFDLTestSuite(Misc.getRequiredResource(aq))
  @Test def test_AQ001() { runnerAQ.runOneTest("AQ001") }

  // @Test def test_InputValueCalc_04() { runner.runOneTest("InputValueCalc_04")}
}
