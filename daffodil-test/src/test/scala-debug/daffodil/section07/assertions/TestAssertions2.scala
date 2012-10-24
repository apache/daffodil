package daffodil.section07.assertions

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import junit.framework.Assert._
import daffodil.tdml.DFDLTestSuite
import daffodil.util.Misc
import daffodil.debugger.Debugger

class TestAssertions2 extends JUnitSuite {
  val testDir = "/daffodil/section07/assertions/"
  val tdml = testDir + "assert.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_assertions_assertPatternLiteralTextMatch() = Debugger.withDebugger {
    runner.runOneTest("assertPatternLiteralTextMatch")
  }
  @Test def test_assertPatternPass3() { runner.runOneTest("assertPatternPass3") }
  @Test def test_assertPatternPass4() { runner.runOneTest("assertPatternPass4") }

}