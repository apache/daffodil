package daffodil

import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import scala.xml._
import daffodil.compiler.Compiler
import tdml.DFDLTestSuite
import daffodil.util.LogLevel
import daffodil.util.LoggingDefaults
import daffodil.util.Logging
import daffodil.util.Misc
import org.junit.Test
import daffodil.debugger.Debugger

class TresysTests3 extends JUnitSuite {
  val testDir = "/test-suite/tresys-contributed/"

  lazy val runnerBF = new DFDLTestSuite(Misc.getRequiredResource(testDir + "bitFlagExpression.tdml"))

  @Test def test_testNone() = { runnerBF.runOneTest("testNone") }
  @Test def test_testOne() { runnerBF.runOneTest("testOne") }
  @Test def test_testMany() { runnerBF.runOneTest("testMany") }

  val sq = testDir + "sequence.tdml"
  lazy val runnerSQ = new DFDLTestSuite(Misc.getRequiredResource(sq))
  @Test def test_hiddenGroup1() = Debugger.withDebugger { runnerSQ.runOneTest("hiddenGroup1") }

}
