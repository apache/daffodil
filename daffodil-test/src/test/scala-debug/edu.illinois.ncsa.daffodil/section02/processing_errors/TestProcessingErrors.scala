package edu.illinois.ncsa.daffodil.section02.processing_errors

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

class TestProcessingErrors extends JUnitSuite {
  val testDir = "/edu.illinois.ncsa.daffodil/section02/processing_errors/"
  val aa = testDir + "ProcessingErrors.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_upaInvalidSchema() { runner.runOneTest("upaInvalidSchema") }
  @Test def test_upaInvalidSchema2() { runner.runOneTest("upaInvalidSchema2") }

}
