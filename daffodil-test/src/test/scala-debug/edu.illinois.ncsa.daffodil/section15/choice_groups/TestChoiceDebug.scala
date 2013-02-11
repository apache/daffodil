package edu.illinois.ncsa.daffodil.section15.choice_groups

import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File
import edu.illinois.ncsa.daffodil.debugger.Debugger

class TestChoiceDebug extends JUnitSuite {
  val testDir = "/edu.illinois.ncsa.daffodil/section15/choice_groups/"
  val aa = testDir + "choice.tdml"

  lazy val runnerCH = new DFDLTestSuite(Misc.getRequiredResource(aa))
  
  @Test def test_choiceWithSequence() { runnerCH.runOneTest("choiceWithSequence") }

}
