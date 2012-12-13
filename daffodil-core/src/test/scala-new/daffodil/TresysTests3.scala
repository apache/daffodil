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
  @Test def test_hiddenGroup1() { runnerSQ.runOneTest("hiddenGroup1") }

  val ab = testDir + "AB.tdml"
  lazy val runnerAB = new DFDLTestSuite(Misc.getRequiredResource(ab))
  // Runs, but it is too slow to use in regression tests
  // @Test def test_AB006() { runnerAB.runOneTest("AB006") }

  val ac = testDir + "AC.tdml"
  lazy val runnerAC = new DFDLTestSuite(Misc.getRequiredResource(ac))
  @Test def test_AC000() { runnerAC.runOneTest("AC000") } // OK

  val ad = testDir + "AD.tdml"
  lazy val runnerAD = new DFDLTestSuite(Misc.getRequiredResource(ad))
  @Test def test_AD000() { runnerAD.runOneTest("AD000") } // OK

  val af = testDir + "AF.tdml"
  lazy val runnerAF = new DFDLTestSuite(Misc.getRequiredResource(af))
  @Test def test_AF000() { runnerAF.runOneTest("AF000") }
  @Test def test_AF001() { runnerAF.runOneTest("AF001") }
  @Test def test_AF002() { runnerAF.runOneTest("AF002") }

  val ag = testDir + "AG.tdml"
  lazy val runnerAG = new DFDLTestSuite(Misc.getRequiredResource(ag))
  @Test def test_AG000() { runnerAG.runOneTest("AG000") } // OK
  @Test def test_AG001() { runnerAG.runOneTest("AG001") } // OK
  @Test def test_AG002() { runnerAG.runOneTest("AG002") } // OK

  val ah = testDir + "AH.tdml"
  lazy val runnerAH = new DFDLTestSuite(Misc.getRequiredResource(ah))
  @Test def test_AH000() { runnerAH.runOneTest("AH000") }
  @Test def test_AH001() { runnerAH.runOneTest("AH001") }
  @Test def test_AH002() { runnerAH.runOneTest("AH002") }

  val ak = testDir + "AK.tdml"
  lazy val runnerAK = new DFDLTestSuite(Misc.getRequiredResource(ak))
  @Test def test_AK000() { runnerAK.runOneTest("AK000") }
  @Test def test_AK001() { runnerAK.runOneTest("AK001") }

  val as = testDir + "AS.tdml"
  lazy val runnerAS = new DFDLTestSuite(Misc.getRequiredResource(as))
  @Test def test_AS000() { runnerAS.runOneTest("AS000") } //OK

}
