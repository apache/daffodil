package daffodil.section24.regular_expressions

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

class TestRegularExpressions extends JUnitSuite {
	val testDir = "/daffodil/section24/regular_expressions/"
	val tdml = testDir + "RegularExpressions.tdml"
	lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))
  
	@Test def testRegEx_01() { runner.runOneTest("testRegEx_01") }
	@Test def testRegEx_02() { runner.runOneTest("testRegEx_02") }
}