package daffodil

import java.io.File
import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import scala.xml._
import daffodil.compiler.Compiler
import tdml.DFDLTestSuite
import daffodil.util.LogLevel
import daffodil.util.LoggingDefaults
import daffodil.util.Misc
import org.junit.Test
import daffodil.debugger.Debugger

class TresysTests3 extends JUnitSuite {
  val testDir = "/test-suite/tresys-contributed/"

  lazy val runnerex = new DFDLTestSuite(Misc.getRequiredResource(testDir + "expressions.tdml"))
  @Test def test_nonFunctionIsDetected() = { runnerex.runOneTest("nonFunctionIsDetected") }
  @Test def test_dfdlPosition1() = { runnerex.runOneTest("dfdlPosition1") }
  @Test def test_dfdlPosition2() = { runnerex.runOneTest("dfdlPosition2") }
  @Test def test_dfdlPosition3() = { runnerex.runOneTest("dfdlPosition3") }
  @Test def test_dfdlPosition4() = { runnerex.runOneTest("dfdlPosition4") }

}