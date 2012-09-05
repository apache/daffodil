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

class TestChoice extends JUnit3Suite {
  val testDir = "/daffodil/section15/choice_groups/"
  val aa = testDir + "choice.tdml"
  
  lazy val runnerCH= new DFDLTestSuite(Misc.getRequiredResource(aa))
  def test_basicChoice() { runnerCH.runOneTest("basic")}
  def test_choice2() { runnerCH.runOneTest("choice2")}
  def test_choice3() { runnerCH.runOneTest("choice3")}
  def test_choice4() { runnerCH.runOneTest("choice4") }
  
  def test_choice5() { runnerCH.runOneTest("choice5")}
  def test_choice6() { runnerCH.runOneTest("choice6")}
  def test_choiceFail1() { runnerCH.runOneTest("choiceFail1")}
  def test_choiceDelim1() { 
    // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
    runnerCH.runOneTest("choiceDelim1")}
  def test_choiceDelim2() { runnerCH.runOneTest("choiceDelim2")}
  def test_choiceDelimFloat() { runnerCH.runOneTest("choiceDelimFloat")}
  def test_choiceDelimString() { runnerCH.runOneTest("choiceDelimString")}
  def test_choiceDelimStringwSp() { runnerCH.runOneTest("choiceDelimStringwSp")}
  def test_choiceDelimInt() { runnerCH.runOneTest("choiceDelimInt")}

  def test_nestedChoice1() { 
    // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
    runnerCH.runOneTest("nestedChoice1")}
  
 
  def test_nestedChoiceAllString() { runnerCH.runOneTest("nestedChoiceAllString")}
  def test_nestedChoiceAllFloat() { runnerCH.runOneTest("nestedChoiceAllFloat")}
  def test_nestedChoiceAllInt() { runnerCH.runOneTest("nestedChoiceAllInt")}
}