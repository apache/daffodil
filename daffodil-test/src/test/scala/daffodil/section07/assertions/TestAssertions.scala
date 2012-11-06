package daffodil.section07.assertions

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import junit.framework.Assert._
import daffodil.tdml.DFDLTestSuite
import daffodil.util.Misc
import daffodil.debugger.Debugger

class TestAssertions extends JUnitSuite {
  val testDir = "/daffodil/section07/assertions/"
  val tdml = testDir + "assert.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_assertions_assertPass() { runner.runOneTest("assertPass") }
  @Test def test_assertions_assertFail1() { runner.runOneTest("assertFail1") }
  @Test def test_assertions_assertFail2() { runner.runOneTest("assertFail2") }

  @Test def test_assertions_assertGuidesChoice() { runner.runOneTest("assertGuidesChoice") }
  
  @Test def test_assertions_assertPatternLiteralTextMatch() = { runner.runOneTest("assertPatternLiteralTextMatch") }
  @Test def test_assertions_assertPatternCombinedTextMatch() = { runner.runOneTest("assertPatternCombinedTextMatch") }
  @Test def test_assertions_assertPatternCombinedTextMatch2() = { runner.runOneTest("assertPatternCombinedTextMatch2") }
  @Test def test_assertions_assertPatternCombinedTextMatch3() = { runner.runOneTest("assertPatternCombinedTextMatch3") }

  @Test def test_assertions_assertPatternPass() { runner.runOneTest("assertPatternPass") }
  @Test def test_assertions_assertPatternFail() { runner.runOneTest("assertPatternFail") } 
  @Test def test_assertions_assertPatternPass2() { runner.runOneTest("assertPatternPass2") }
  @Test def test_assertions_assertPatternPass3() { runner.runOneTest("assertPatternPass3") }
  @Test def test_assertions_assertPatternFail2() { runner.runOneTest("assertPatternFail2") }
  @Test def test_assertOnSequence() { runner.runOneTest("assertOnSequence") }

}
