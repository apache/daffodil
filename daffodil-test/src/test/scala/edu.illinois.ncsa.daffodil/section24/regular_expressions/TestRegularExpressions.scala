package edu.illinois.ncsa.daffodil.section24.regular_expressions

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

class TestRegularExpressions extends JUnitSuite {
  val testDir = "/edu.illinois.ncsa.daffodil/section24/regular_expressions/"
  val tdml = testDir + "RegularExpressions.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def testRegEx_01() { runner.runOneTest("testRegEx_01") }
  @Test def testRegEx_02() { runner.runOneTest("testRegEx_02") }
}