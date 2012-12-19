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

}
