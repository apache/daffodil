package daffodil.section07.assertions

import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert._
import daffodil.tdml.DFDLTestSuite
import daffodil.util.Misc
import daffodil.debugger.Debugger

class TestAssertionsAndDiscriminators extends JUnit3Suite {
  val testDir = "/daffodil/section07/assertions/"
  val tdml = testDir + "assert.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  def test_assertions_assertPass() { runner.runOneTest("assertPass") }
  def test_assertions_assertFail1() { runner.runOneTest("assertFail1") }
  def test_assertions_assertFail2() { runner.runOneTest("assertFail2") }

  def test_assertions_assertGuidesChoice() { runner.runOneTest("assertGuidesChoice") }
  def test_assertions_discriminatorGuidesChoice() { runner.runOneTest("discriminatorGuidesChoice") }

}