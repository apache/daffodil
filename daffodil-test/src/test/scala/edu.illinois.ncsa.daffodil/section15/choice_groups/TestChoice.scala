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

class TestChoice extends JUnitSuite {
  val testDir = "/daffodil/section15/choice_groups/"
  val aa = testDir + "choice.tdml"

  lazy val runnerCH = new DFDLTestSuite(Misc.getRequiredResource(aa))
  @Test def test_basicChoice() { runnerCH.runOneTest("basic") }
  @Test def test_choice2() { runnerCH.runOneTest("choice2") }
  @Test def test_choice3() { runnerCH.runOneTest("choice3") }
  @Test def test_choice4() { runnerCH.runOneTest("choice4") }
  
//  @Test def test_choiceWithSequence() { runnerCH.runOneTest("choiceWithSequence") }

  @Test def test_choiceWithInitsAndTermsInt() { runnerCH.runOneTest("choiceWithInitsAndTermsInt") }
  @Test def test_choiceWithInitsAndTermsStr() { runnerCH.runOneTest("choiceWithInitsAndTermsStr") }
  @Test def test_choiceWithInitsAndTermsError() { runnerCH.runOneTest("choiceWithInitsAndTermsError") }
  @Test def test_choiceWithInitsAndTermsSeqInt() { runnerCH.runOneTest("choiceWithInitsAndTermsSeqInt") }
  @Test def test_choiceWithInitsAndTermsSeqStr() { runnerCH.runOneTest("choiceWithInitsAndTermsSeqStr") }
  @Test def test_nestedChoiceWithInitsAndTermsNestedInt() { runnerCH.runOneTest("nestedChoiceWithInitsAndTermsNestedInt") }
  @Test def test_nestedChoiceWithInitsAndTermsNestedStr() { runnerCH.runOneTest("nestedChoiceWithInitsAndTermsNestedStr") }
  @Test def test_nestedChoiceWithInitsAndTermsInt() { runnerCH.runOneTest("nestedChoiceWithInitsAndTermsInt") }

  @Test def test_choice5() { runnerCH.runOneTest("choice5") }
  @Test def test_choice6() { runnerCH.runOneTest("choice6") }
  @Test def test_choiceFail1() { runnerCH.runOneTest("choiceFail1") }
  @Test def test_choiceDelim1() {
    // Logging@Test defaults.setLoggingLevel(LogLevel.Debug)
    runnerCH.runOneTest("choiceDelim1")
  }
  @Test def test_choiceDelim2() { runnerCH.runOneTest("choiceDelim2") }
  @Test def test_choiceDelimFloat() { runnerCH.runOneTest("choiceDelimFloat") }
  @Test def test_choiceDelimString() { runnerCH.runOneTest("choiceDelimString") }
  @Test def test_choiceDelimStringwSp() { runnerCH.runOneTest("choiceDelimStringwSp") }
  @Test def test_choiceDelimInt() { runnerCH.runOneTest("choiceDelimInt") }

  @Test def test_nestedChoice1() {
    // Logging@Test defaults.setLoggingLevel(LogLevel.Debug)
    runnerCH.runOneTest("nestedChoice1")
  }

  @Test def test_nestedChoiceAllString() { runnerCH.runOneTest("nestedChoiceAllString") }
  @Test def test_nestedChoiceAllFloat() { runnerCH.runOneTest("nestedChoiceAllFloat") }
  @Test def test_nestedChoiceAllInt() { runnerCH.runOneTest("nestedChoiceAllInt") }

  @Test def test_choiceInSeq1() { runnerCH.runOneTest("choiceInSequenceWithSeparators1") }
  @Test def test_choiceInSeq2() { runnerCH.runOneTest("choiceInSequenceWithSeparators2") }

  @Test def test_seqInChoiceInSeq1() { runnerCH.runOneTest("sequenceInChoiceInSequenceWithSeparators1") }
  @Test def test_seqInChoiceInSeq2() { runnerCH.runOneTest("sequenceInChoiceInSequenceWithSeparators2") }
  @Test def test_seqInChoiceInSeq3() { runnerCH.runOneTest("sequenceInChoiceInSequenceWithSameSeparators1") }
  @Test def test_seqInChoiceInSeq4() { runnerCH.runOneTest("sequenceInChoiceInSequenceWithSameSeparators2") }


  val testDir1 = "/daffodil/ibm-tests/"
  val tdml1 = testDir1 + "dpaext2.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml1))
  
  @Test def test_choices_basic_15_01() { runner.runOneTest("choices_basic_15_01") }
  @Test def test_choices_basic_15_02() { runner.runOneTest("choices_basic_15_02") }
  @Test def test_choices_basic_15_03() { runner.runOneTest("choices_basic_15_03") }
}
