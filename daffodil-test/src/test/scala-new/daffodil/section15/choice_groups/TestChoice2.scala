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

class TestChoice2 extends JUnitSuite {
  val testDir = "/daffodil/section15/choice_groups/"
  val tdml = testDir + "choice.tdml"
  
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_choiceInSeq1() { runner.runOneTest("choiceInSequenceWithSeparators1") }
  @Test def test_choiceInSeq2() { runner.runOneTest("choiceInSequenceWithSeparators2") }

  @Test def test_seqInChoiceInSeq1() { runner.runOneTest("sequenceInChoiceInSequenceWithSeparators1") }
  @Test def test_seqInChoiceInSeq2() { runner.runOneTest("sequenceInChoiceInSequenceWithSeparators2") }

  @Test def test_choiceWithInitsAndTermsInt() { runner.runOneTest("choiceWithInitsAndTermsInt") }
  @Test def test_choiceWithInitsAndTermsStr() { runner.runOneTest("choiceWithInitsAndTermsStr") }
  @Test def test_choiceWithInitsAndTermsError() { runner.runOneTest("choiceWithInitsAndTermsError") }
  
  @Test def test_choiceWithSequence1() { runner.runOneTest("choiceWithSequence1") }
  @Test def test_choiceWithSequence2() { runner.runOneTest("choiceWithSequence2") }
  
  @Test def test_choiceWithChoiceInt() { runner.runOneTest("choiceWithChoiceInt") }
  @Test def test_choiceWithChoiceFloat() { runner.runOneTest("choiceWithChoiceFloat") }
  @Test def test_choiceWithChoiceString() { runner.runOneTest("choiceWithChoiceString") }

  @Test def test_choiceWithArrayInt() { runner.runOneTest("choiceWithArrayInts") }
  @Test def test_choiceWithArrayFloat() { runner.runOneTest("choiceWithArrayFloats") }
  @Test def test_choiceWithArrayString() { runner.runOneTest("choiceWithChoiceString") }

}
