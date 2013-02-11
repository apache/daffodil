package edu.illinois.ncsa.daffodil.section07.assertions

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.debugger.Debugger

class TestAssertions extends JUnitSuite {
  val testDir = "/edu.illinois.ncsa.daffodil/section07/assertions/"
  val tdml = testDir + "assert.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml), validateTDMLFile = false)

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

  @Test def test_assertOnGroupRef() { runner.runOneTest("assertOnGroupRef") }
  @Test def test_assertOnElemRef() { runner.runOneTest("assertOnElemRef") }

  @Test def test_assertPatternMatch() { runner.runOneTest("assertPatternMatch") }
  @Test def test_assertPatternMatch2() { runner.runOneTest("assertPatternMatch2") }
  
  @Test def test_assertMultFormsFail() { runner.runOneTest("assertMultFormsFail") }
  @Test def test_assertMultFormsFail2() { runner.runOneTest("assertMultFormsFail2") }
  @Test def test_assertPatternAndExp() { runner.runOneTest("assertPatternAndExp") }
  @Test def test_assertPatternAndExp2() { runner.runOneTest("assertPatternAndExp2") }
  @Test def test_assertOnSimpleType() { runner.runOneTest("assertOnSimpleType") }
  @Test def test_assertPass2() { runner.runOneTest("assertPass2") }

  @Test def test_assertExpressionRef() { runner.runOneTest("assertExpressionRef") }
  @Test def test_assertExpressionRefFail() { runner.runOneTest("assertExpressionRefFail") }
//  @Test def test_assertExpressionEmpty() { runner.runOneTest("assertExpressionEmpty") }
//  @Test def test_assertPatternEmpty() { runner.runOneTest("assertPatternEmpty") }
  @Test def test_assertMessage() { runner.runOneTest("assertMessage") }

}
