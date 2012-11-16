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

class TresysTests2 extends JUnitSuite {
  val testDir = "/test-suite/tresys-contributed/"

  // This test passes now. Left this here to cut/paste for running other tests.    
  //  val ai = testDir + "AI.tdml"
  //  lazy val runnerAI = new DFDLTestSuite(Misc.getRequiredResource(ai))
  //
  //  @Test def test_AI000() { runnerAI.runOneTest("AI000") }

  //  lazy val runnerMB = new DFDLTestSuite(Misc.getRequiredResource(testDir + "mixed-binary-text.tdml"))
  //
  //  @Test def test_codingErrorAction() { runnerMB.runOneTest("codingErrorAction") }

  lazy val runnerBF = new DFDLTestSuite(Misc.getRequiredResource(testDir + "bitFlagExpression.tdml"))

  @Test def test_testNone() {
    // LoggingDefaults.setLoggingLevel(LogLevel.Compile)
    runnerBF.runOneTest("testNone")
  }
  @Test def test_testOne() { runnerBF.runOneTest("testOne") }
  @Test def test_testMany() { runnerBF.runOneTest("testMany") }

  val ab = testDir + "AB.tdml"
  lazy val runnerAB = new DFDLTestSuite(Misc.getRequiredResource(ab))

  @Test def test_AB006() { runnerAB.runOneTest("AB006") }
  @Test def test_AB007() { runnerAB.runOneTest("AB007") }

  val ac = testDir + "AC.tdml"
  lazy val runnerAC = new DFDLTestSuite(Misc.getRequiredResource(ac))
  @Test def test_AC000() { runnerAC.runOneTest("AC000") }

  val ad = testDir + "AD.tdml"
  lazy val runnerAD = new DFDLTestSuite(Misc.getRequiredResource(ad))
  @Test def test_AD000() { runnerAD.runOneTest("AD000") }

  val ae = testDir + "AE.tdml"
  lazy val runnerAE = new DFDLTestSuite(Misc.getRequiredResource(ae))
  @Test def test_AE000() { runnerAE.runOneTest("AE000") }

  val af = testDir + "AF.tdml"
  lazy val runnerAF = new DFDLTestSuite(Misc.getRequiredResource(af))
  @Test def test_AF000() { runnerAF.runOneTest("AF000") }
  @Test def test_AF001() { runnerAF.runOneTest("AF001") }
  @Test def test_AF002() { runnerAF.runOneTest("AF002") }

  val ag = testDir + "AG.tdml"
  lazy val runnerAG = new DFDLTestSuite(Misc.getRequiredResource(ag))
  @Test def test_AG000() { runnerAG.runOneTest("AG000") }
  @Test def test_AG001() { runnerAG.runOneTest("AG001") }
  @Test def test_AG002() { runnerAG.runOneTest("AG002") }

  val ah = testDir + "AH.tdml"
  lazy val runnerAH = new DFDLTestSuite(Misc.getRequiredResource(ah))
  @Test def test_AH000() { runnerAH.runOneTest("AH000") }
  @Test def test_AH001() { runnerAH.runOneTest("AH001") }
  @Test def test_AH002() { runnerAH.runOneTest("AH002") }

  val ak = testDir + "AK.tdml"
  lazy val runnerAK = new DFDLTestSuite(Misc.getRequiredResource(ak))
  @Test def test_AK000() { runnerAK.runOneTest("AK000") }
  @Test def test_AK001() { runnerAK.runOneTest("AK001") }

  val al = testDir + "AL.tdml"
  lazy val runnerAL = new DFDLTestSuite(Misc.getRequiredResource(al))
  @Test def test_AL000() { runnerAL.runOneTest("AL000") }

  val am = testDir + "AM.tdml"
  lazy val runnerAM = new DFDLTestSuite(Misc.getRequiredResource(am))
  @Test def test_AM000() { runnerAM.runOneTest("AM000") }
  @Test def test_AM001() { runnerAM.runOneTest("AM001") }

  val ao = testDir + "AO.tdml"
  lazy val runnerAO = new DFDLTestSuite(Misc.getRequiredResource(ao))
  @Test def test_AO000() { runnerAO.runOneTest("AO000") }
  @Test def test_AO001() { runnerAO.runOneTest("AO001") }
  @Test def test_AO002() { runnerAO.runOneTest("AO002") }
  @Test def test_AO003() { runnerAO.runOneTest("AO003") }
  @Test def test_AO004() { runnerAO.runOneTest("AO004") }

  val ap = testDir + "AP.tdml"
  lazy val runnerAP = new DFDLTestSuite(Misc.getRequiredResource(ap))
  @Test def test_AP000() { runnerAP.runOneTest("AP000") }

  val as = testDir + "AS.tdml"
  lazy val runnerAS = new DFDLTestSuite(Misc.getRequiredResource(as))
  @Test def test_AS000() { runnerAS.runOneTest("AS000") }

  val at = testDir + "AT.tdml"
  lazy val runnerAT = new DFDLTestSuite(Misc.getRequiredResource(at))
  @Test def test_AT000() { runnerAT.runOneTest("AT000") }

  val au = testDir + "AU.tdml"
  lazy val runnerAU = new DFDLTestSuite(Misc.getRequiredResource(au))
  @Test def test_AU000() { runnerAU.runOneTest("AU000") }

  val av0 = testDir + "AV000.tdml"
  lazy val runnerAV000 = new DFDLTestSuite(Misc.getRequiredResource(av0))
  @Test def test_AV000() { runnerAV000.runOneTest("AV000") }

  val av1 = testDir + "AV001.tdml"
  lazy val runnerAV001 = new DFDLTestSuite(Misc.getRequiredResource(av1))
  @Test def test_AV001() { runnerAV001.runOneTest("AV001") }

  val av2 = testDir + "AV002.tdml"
  lazy val runnerAV002 = new DFDLTestSuite(Misc.getRequiredResource(av2))
  @Test def test_AV002() { runnerAV002.runOneTest("AV002") }

  val av3 = testDir + "AV003.tdml"
  lazy val runnerAV003 = new DFDLTestSuite(Misc.getRequiredResource(av3))
  @Test def test_AV003() { runnerAV003.runOneTest("AV003") }

  val aw = testDir + "AW.tdml"
  lazy val runnerAW = new DFDLTestSuite(Misc.getRequiredResource(aw))
  @Test def test_AW000() { runnerAW.runOneTest("AW000") }
  @Test def test_AW001() { runnerAW.runOneTest("AW001") }

  val ax = testDir + "AX.tdml"
  lazy val runnerAX = new DFDLTestSuite(Misc.getRequiredResource(ax))
  @Test def test_AX000() { runnerAX.runOneTest("AX000") }

  val ay = testDir + "AY.tdml"
  lazy val runnerAY = new DFDLTestSuite(Misc.getRequiredResource(ay))
  @Test def test_AY000() { runnerAY.runOneTest("AY000") }

  val az = testDir + "AZ.tdml"
  lazy val runnerAZ = new DFDLTestSuite(Misc.getRequiredResource(az))
  @Test def test_AZ000() { runnerAZ.runOneTest("AZ000") }

  val ba = testDir + "BA.tdml"
  lazy val runnerBA = new DFDLTestSuite(Misc.getRequiredResource(ba))
  @Test def test_BA000() { runnerBA.runOneTest("BA000") }

  val bb = testDir + "BB.tdml"
  lazy val runnerBB = new DFDLTestSuite(Misc.getRequiredResource(bb))
  @Test def test_BB000() { runnerBB.runOneTest("BB000") }

  val bc = testDir + "BC.tdml"
  lazy val runnerBC = new DFDLTestSuite(Misc.getRequiredResource(bc))
  @Test def test_BC000() { runnerBC.runOneTest("BC000") }

  val bd = testDir + "BD.tdml"
  lazy val runnerBD = new DFDLTestSuite(Misc.getRequiredResource(bd))
  @Test def test_BD000() { runnerBD.runOneTest("BD000") }

  val be = testDir + "BE.tdml"
  lazy val runnerBE = new DFDLTestSuite(Misc.getRequiredResource(be))
  @Test def test_BE000() { runnerBE.runOneTest("BE000") }
  @Test def test_BE001() { runnerBE.runOneTest("BE001") }

  val bf = testDir + "BF.tdml"
  lazy val runnerBF1 = new DFDLTestSuite(Misc.getRequiredResource(bf))
  @Test def test_BF000() { runnerBF1.runOneTest("BF000") }
  @Test def test_BF001() { runnerBF1.runOneTest("BF001") }

  val bg = testDir + "BG.tdml"
  lazy val runnerBG = new DFDLTestSuite(Misc.getRequiredResource(bg))
  @Test def test_BG000() { runnerBG.runOneTest("BG000") }
}
