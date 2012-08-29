package daffodil.section17.calc_value_properties

import java.io.File
import org.scalatest.junit.JUnit3Suite
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

class TestInputValueCalcDebug extends JUnit3Suite {
  val testDir = "/daffodil/section17/calc_value_properties/"
  val tdml = testDir + "inputValueCalc.tdml"

  lazy val runner = { new DFDLTestSuite(Misc.getRequiredResource(tdml)) }
  
  def test_InputValueCalc_04() { runner.runOneTest("InputValueCalc_04")}
}
