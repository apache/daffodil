package edu.illinois.ncsa.daffodil.section14.sequence_groups

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

class TestSequenceGroupInitiatedContent extends JUnitSuite {

  val testDir_01 = "/edu.illinois.ncsa.daffodil/section14/sequence_groups/"
  val tdml_01 = testDir_01 + "SequenceGroupInitiatedContent.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))

  @Test def test_baseline() { runner_01.runOneTest("initiatedContentSeqBaseline") }
  @Test def test_1() { runner_01.runOneTest("initiatedContentSeq1") }
  @Test def test_2() { runner_01.runOneTest("initiatedContentSeq2") }
  @Test def test_3() { runner_01.runOneTest("initiatedContentSeq3") }

}
