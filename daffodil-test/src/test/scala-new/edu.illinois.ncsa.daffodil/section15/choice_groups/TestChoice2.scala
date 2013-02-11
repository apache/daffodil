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

class TestChoice2 extends JUnitSuite {
  val testDir = "/edu.illinois.ncsa.daffodil/section15/choice_groups/"
  val tdml = testDir + "choice.tdml"
  
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_choiceWithSequence1() { runner.runOneTest("choiceWithSequence1") }
  @Test def test_choiceWithSequence2() { runner.runOneTest("choiceWithSequence2") }
  
  @Test def test_choiceWithChoiceInt() { runner.runOneTest("choiceWithChoiceInt") }
  @Test def test_choiceWithChoiceFloat() { runner.runOneTest("choiceWithChoiceFloat") }
  @Test def test_choiceWithChoiceString() { runner.runOneTest("choiceWithChoiceString") }

  @Test def test_choiceWithArrayInt() { runner.runOneTest("choiceWithArrayInts") }
  @Test def test_choiceWithArrayFloat() { runner.runOneTest("choiceWithArrayFloats") }
  @Test def test_choiceWithArrayString() { runner.runOneTest("choiceWithChoiceString") }

}
