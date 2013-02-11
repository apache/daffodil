package daffodil.section14.sequence_groups

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

class TestSequenceGroups extends JUnitSuite {
  val testDir = "/daffodil/ibm-tests/"
  val aa = testDir + "dpaext1.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_multiple_delimiters2() { runner.runOneTest("multiple_delimiters2") }

  val tdml2 = testDir + "dpaext2.tdml"
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))
  @Test def test_sequences_separated_14_03() { runner2.runOneTest("sequences_separated_14_03") }
  @Test def test_sequences_separated_14_05() { runner2.runOneTest("sequences_separated_14_05") }
  @Test def test_sequences_separated_14_06() { runner2.runOneTest("sequences_separated_14_06") }

  val testDir_01 = "/daffodil/section14/sequence_groups/"
  val tdml_01 = testDir_01 + "SequenceGroupDelimiters.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))

  @Test def test_SeqGrp_01() { runner_01.runOneTest("SeqGrp_01") }
  @Test def test_SeqGrp_02() { runner_01.runOneTest("SeqGrp_02") }
  @Test def test_SeqGrp_03() { runner_01.runOneTest("SeqGrp_03") }
  @Test def test_SeqGrp_04() { runner_01.runOneTest("SeqGrp_04") }
  @Test def test_prefix() { runner_01.runOneTest("prefix") }
  @Test def test_prefix_01() { runner_01.runOneTest("prefix_01") }
  @Test def test_NumSeq_02() { runner_01.runOneTest("NumSeq_02") }
  @Test def test_groupRefInheritProps() { runner_01.runOneTest("groupRefInheritProps") }
  @Test def test_sequenceWithinSequence() { runner_01.runOneTest("sequenceWithinSequence") }

  val tdml_02 = testDir_01 + "SequenceGroup.tdml"
  lazy val runner_02 = new DFDLTestSuite(Misc.getRequiredResource(tdml_02))

  @Test def test_SeqGrp_05() { runner_02.runOneTest("SeqGrp_05") }

  @Test def test_hiddenGroup1() { runner_02.runOneTest("hiddenGroup1") }
  @Test def test_hiddenGroupSchemaFail() { runner_02.runOneTest("hiddenGroupSchemaFail") }
  @Test def test_hiddenGroupWithAssert() { runner_02.runOneTest("hiddenGroupWithAssert") }
  @Test def test_hiddenGroupWithAssert2() { runner_02.runOneTest("hiddenGroupWithAssert2") }
  @Test def test_hiddenGroupNested() { runner_02.runOneTest("hiddenGroupNested") }
  @Test def test_hiddenGroupNested2() { runner_02.runOneTest("hiddenGroupNested2") }
  @Test def test_hiddenGroupChoice() { runner_02.runOneTest("hiddenGroupChoice") }
  @Test def test_hiddenGroupChoice2() { runner_02.runOneTest("hiddenGroupChoice2") }
  @Test def test_hiddenGroupIgnoredProps() { runner_02.runOneTest("hiddenGroupIgnoredProps") }
  @Test def test_hiddenGroupAttributeNotation() { runner_02.runOneTest("hiddenGroupAttributeNotation") }
  @Test def test_hiddenGroupElementNotation() { runner_02.runOneTest("hiddenGroupElementNotation") }
//  @Test def test_hiddenGroupLoop() { runner_02.runOneTest("hiddenGroupLoop") }

  @Test def test_AC000() { runner_02.runOneTest("AC000") }
  @Test def test_AD000() { runner_02.runOneTest("AD000") }
  @Test def test_AS000() { runner_02.runOneTest("AS000") }
  
}
