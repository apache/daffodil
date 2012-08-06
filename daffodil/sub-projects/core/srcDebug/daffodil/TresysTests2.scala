package daffodil

import java.io.File
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

class TresysTests2 extends JUnit3Suite {
  val testDir = "test-suite/tresys-contributed/"

  // This test passes now. Left this here to cut/paste for running other tests.    
  //  val ai = testDir + "AI.tdml"
  //  val runnerAI = new DFDLTestSuite(new File(ai))
  //
  //  def test_AI000() { runnerAI.runOneTest("AI000") }

  val runnerBF = new DFDLTestSuite(new File(testDir + "bitFlagExpression.tdml"))
  
  def test_testNone() { 
    LoggingDefaults.setLoggingLevel(LogLevel.Compile)
    runnerBF.runOneTest("testNone") }
  def test_testOne() { runnerBF.runOneTest("testOne") }
  def test_testMany() { runnerBF.runOneTest("testMany") }
  

}
