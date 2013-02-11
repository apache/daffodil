package edu.illinois.ncsa.daffodil.section12.delimiter_properties

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

class TestDelimiterProperties_01 extends JUnitSuite {

  val testDir_02 = "/edu.illinois.ncsa.daffodil/section12/delimiter_properties/"
  val tdml_02 = testDir_02 + "DelimiterProperties.tdml"
  lazy val r = new DFDLTestSuite(Misc.getRequiredResource(tdml_02))

  @Test def test_BadErrorMsgWhenRequiredFieldIsMissingAndSeparatorIsPrefixOfTerminator() = {
    r.runOneTest("BadErrorMsgWhenRequiredFieldIsMissingAndSeparatorIsPrefixOfTerminator")
  }
}
