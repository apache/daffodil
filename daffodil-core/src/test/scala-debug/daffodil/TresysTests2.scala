package daffodil

import org.scalatest.junit.JUnit3Suite
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

class TresysTests2 extends JUnit3Suite {
  val testDir = "/test-suite/tresys-contributed/"

  // This test passes now. Left this here to cut/paste for running other tests.    
  //  val ai = testDir + "AI.tdml"
  //  lazy val runnerAI = new DFDLTestSuite(Misc.getRequiredResource(ai))
  //
  //  def test_AI000() { runnerAI.runOneTest("AI000") }

  lazy val runnerBF = new DFDLTestSuite(Misc.getRequiredResource(testDir + "bitFlagExpression.tdml"))
  
  def test_testNone() { 
    LoggingDefaults.setLoggingLevel(LogLevel.Compile)
    runnerBF.runOneTest("testNone") }
  def test_testOne() { runnerBF.runOneTest("testOne") }
  def test_testMany() { runnerBF.runOneTest("testMany") }
  
  val ab = testDir + "AB.tdml"
  lazy val runnerAB = new DFDLTestSuite(Misc.getRequiredResource(ab))
  def test_AB006() { runnerAB.runOneTest("AB006") }
  
  val am = testDir + "AM.tdml"
  lazy val runnerAM = new DFDLTestSuite(Misc.getRequiredResource(am))
  def test_AM000() { runnerAM.runOneTest("AM000") }
  def test_AM001() { runnerAM.runOneTest("AM001") }
}
