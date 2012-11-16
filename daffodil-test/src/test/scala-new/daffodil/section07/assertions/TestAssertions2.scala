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

  @Test def test_assertOnGroupRef() { runner.runOneTest("assertOnGroupRef") }
  @Test def test_assertOnElemRef() { runner.runOneTest("assertOnElemRef") }

}