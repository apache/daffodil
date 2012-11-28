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
}
