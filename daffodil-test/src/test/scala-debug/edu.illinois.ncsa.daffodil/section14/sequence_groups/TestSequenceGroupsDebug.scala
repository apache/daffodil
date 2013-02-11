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
import daffodil.util.Logging
import daffodil.util.Logging

class TestSequenceGroupsDebug extends JUnitSuite {
  
  val testDir_01 = "/daffodil/section14/sequence_groups/"
  val tdml_03 = testDir_01 + "SequenceGroup.tdml"
  lazy val runner_03 = new DFDLTestSuite(Misc.getRequiredResource(tdml_03))
  
  @Test def test_hiddenGroupLoop() { runner_03.runOneTest("hiddenGroupLoop") }

}

