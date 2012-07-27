package daffodil

import java.io.File
import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import scala.xml._
import daffodil.dsom.Compiler
import tdml.DFDLTestSuite
import daffodil.util.LogLevel
import daffodil.util.LoggingDefaults

class TresysTests2 extends JUnit3Suite {
  val testDir = "test-suite/tresys-contributed/"
    
// This test passes now. Left this here to cut/paste for running other tests.    
//  val ai = testDir + "AI.tdml"
//  val runnerAI = new DFDLTestSuite(new File(ai))
//
//  def test_AI000() { runnerAI.runOneTest("AI000") }
 
}
