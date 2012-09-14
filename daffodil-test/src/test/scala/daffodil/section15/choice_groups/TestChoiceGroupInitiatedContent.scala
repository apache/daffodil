package daffodil.section15.choice_groups

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File
import daffodil.debugger.Debugger

class TestChoiceGroupInitiatedContent extends JUnit3Suite {

  val testDir_01 = "/daffodil/section15/choice_groups/"
  val tdml_01 = testDir_01 + "ChoiceGroupInitiatedContent.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))

  def test_1() { runner_01.runOneTest("initiatedContent1") }
  def test_2() { runner_01.runOneTest("initiatedContent2") }
  def test_3() { runner_01.runOneTest("initiatedContent3") }
  def test_4() { runner_01.runOneTest("initiatedContent4") }



}
