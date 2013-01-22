package daffodil.section15.choice_groups

import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File
import daffodil.debugger.Debugger

class TestChoiceDebug extends JUnitSuite {
  val testDir = "/daffodil/section15/choice_groups/"
  val aa = testDir + "choice.tdml"

  lazy val runnerCH = new DFDLTestSuite(Misc.getRequiredResource(aa))
  
  @Test def test_choiceWithSequence() { runnerCH.runOneTest("choiceWithSequence") }

}
