package daffodil.section5.simple_types

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.dsom.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestSimpleTypes extends JUnit3Suite {
  val testDir = "src/daffodil/section5/simple_types/"
  val aa = testDir + "SimpleTypes.tdml"
  val runner = new DFDLTestSuite(new File(aa))
  
  def test_Long1() { runner.runOneTest("Long1") }
  def test_schema_types_5_04() { runner.runOneTest("schema_types_5_04") }
  def test_BigInteger1() { runner.runOneTest("BigInteger1") }
  def test_Integer01() { runner.runOneTest("Integer01") }
  def test_Int01() { runner.runOneTest("Int01") }
  def test_schema_types_5_03() { runner.runOneTest("schema_types_5_03") }
  def test_schema_types_5_02() { runner.runOneTest("schema_types_5_02") }
  def test_int_error() { runner.runOneTest("int_error") }
  def test_warning_exercise() { 
    val exc = intercept[Exception] {
    	runner.runOneTest("warning_exercise") }
    	assertTrue(exc.getMessage().contains("Did not find"))
  	}
  def test_UnsignedNumbers1() { runner.runOneTest("UnsignedNumbers1") }
  }