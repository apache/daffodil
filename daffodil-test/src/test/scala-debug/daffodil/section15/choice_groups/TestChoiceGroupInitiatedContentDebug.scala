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

class TestChoiceGroupInitiatedContentDebug extends JUnitSuite {

  val testDir_01 = "/daffodil/section15/choice_groups/"
  val tdml_01 = testDir_01 + "ChoiceGroupInitiatedContent.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))

//  @Test def test_5() { runner_01.runOneTest("initiatedContentChoice5") }
//  @Test def test_6() { runner_01.runOneTest("initiatedContentChoice6") }
//  @Test def test_8() { runner_01.runOneTest("initiatedContentChoice8") }
//  @Test def test_9() { runner_01.runOneTest("initiatedContentChoice9") }

}
