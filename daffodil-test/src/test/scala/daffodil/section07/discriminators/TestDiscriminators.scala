package daffodil.section07.discriminators

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import junit.framework.Assert._
import daffodil.tdml.DFDLTestSuite
import daffodil.util.Misc
import daffodil.debugger.Debugger

class TestDiscriminators extends JUnitSuite {
  val testDir = "/daffodil/section07/discriminators/"
  val tdml = testDir + "discriminator.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_assertions_discriminatorGuidesChoice() { runner.runOneTest("discriminatorGuidesChoice") }

}