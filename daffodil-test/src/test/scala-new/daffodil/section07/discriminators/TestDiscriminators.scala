package daffodil.section07.discriminators

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import junit.framework.Assert._
import daffodil.tdml.DFDLTestSuite
import daffodil.util.Misc
import daffodil.debugger.Debugger

class TestDiscriminatorsNew extends JUnitSuite {
  val testDir = "/daffodil/section07/discriminators/"
  val tdml = testDir + "discriminator.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_discriminators_discrimPatternPass() { runner.runOneTest("discrimPatternPass") }
  @Test def test_discriminators_discrimPatternFail() { runner.runOneTest("discrimPatternFail") }
  
  @Test def test_discriminators_discrimPatternPass2() { runner.runOneTest("discrimPatternPass2") }
  @Test def test_discriminators_discrimPatternFail2() { runner.runOneTest("discrimPatternFail2") }
  @Test def test_discriminators_discrimPatternFail3() { runner.runOneTest("discrimPatternFail3") }
  @Test def test_discriminators_discrimPatternFail4() { runner.runOneTest("discrimPatternFail4") }
  @Test def test_discriminators_discrimPatternFail5() { runner.runOneTest("discrimPatternFail5") }
  @Test def test_discriminators_discrimPatternFail6() { runner.runOneTest("discrimPatternFail6") }
}