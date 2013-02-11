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
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util.Logging

class TestSequenceGroupsDebug extends JUnitSuite {
  
  val testDir_01 = "/edu.illinois.ncsa.daffodil/section14/sequence_groups/"
  val tdml_03 = testDir_01 + "SequenceGroup.tdml"
  lazy val runner_03 = new DFDLTestSuite(Misc.getRequiredResource(tdml_03))
  
  @Test def test_hiddenGroupLoop() { runner_03.runOneTest("hiddenGroupLoop") }

}

