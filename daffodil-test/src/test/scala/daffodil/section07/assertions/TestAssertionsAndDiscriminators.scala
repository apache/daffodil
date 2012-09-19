package daffodil.section07.assertions

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import junit.framework.Assert._
import daffodil.tdml.DFDLTestSuite
import daffodil.util.Misc
import daffodil.debugger.Debugger

class TestAssertionsAndDiscriminators extends JUnitSuite {
  val testDir = "/daffodil/section07/assertions/"
  val tdml = testDir + "assert.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_assertions_assertPass() { runner.runOneTest("assertPass") }
  @Test def test_assertions_assertFail1() { runner.runOneTest("assertFail1") }
  @Test def test_assertions_assertFail2() { runner.runOneTest("assertFail2") }

  @Test def test_assertions_assertGuidesChoice() { runner.runOneTest("assertGuidesChoice") }
  @Test def test_assertions_discriminatorGuidesChoice() { runner.runOneTest("discriminatorGuidesChoice") }

}