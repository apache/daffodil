package daffodil.section23.dfdl_expressions

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestDFDLExpressionsDebug extends JUnit3Suite {
  val testDir = "/daffodil/section23/dfdl_expressions/"
  val tdml = testDir + "expressions.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))
  
  // ArrayOptElem_02 should pass when binary unsignedInt is implemented.
  def test_ArrayOptElem_02() { runner.runOneTest("ArrayOptElem_02") }
//  def test_expressions_lke2_rel() { runner.runOneTest("lke2_rel") }
  def test_expressions_lke3_rel() { runner.runOneTest("lke3_rel") }
  }
