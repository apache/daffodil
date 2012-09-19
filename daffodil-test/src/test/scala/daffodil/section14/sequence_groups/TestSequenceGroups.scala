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
  
  val testDir_01 = "/daffodil/section14/sequence_groups/"
  val tdml_01 = testDir_01 + "SequenceGroupDelimiters.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))
  
  @Test def test_SeqGrp_01() { runner_01.runOneTest("SeqGrp_01") }
  @Test def test_SeqGrp_02() { runner_01.runOneTest("SeqGrp_02") }
  @Test def test_SeqGrp_03() { runner_01.runOneTest("SeqGrp_03") }
  @Test def test_SeqGrp_04() { runner_01.runOneTest("SeqGrp_04") }
  }
