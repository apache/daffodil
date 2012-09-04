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

class TestDFDLExpressions extends JUnit3Suite {
  val testDir = "/daffodil/section23/dfdl_expressions/"
  val tdml = testDir + "expressions.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))
  
  def test_expressions_lke1_rel() { runner.runOneTest("lke1_rel") }
  def test_expressions_lke1_abs() { runner.runOneTest("lke1_abs") }
  def test_expressions_ocke1() { runner.runOneTest("ocke1") }
  def test_expressions_ocke2() { runner.runOneTest("ocke2") }
  //def test_ArrayOptElem_01() { runner.runOneTest("ArrayOptElem_01") }
  def test_expression_type_error1() { runner.runOneTest("expression-type-error1")}
  def test_expression_type_error2() { runner.runOneTest("expression-type-error2")}
  def test_expression_type_error3() { runner.runOneTest("expression-type-error3")}
  }
