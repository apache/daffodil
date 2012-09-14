package daffodil.section05.simple_types

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestSimpleTypes extends JUnit3Suite {
  val testDir = "/daffodil/section05/simple_types/"
  val aa = testDir + "SimpleTypes.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
  
  def test_Long1() { runner.runOneTest("Long1") }
  def test_BigInteger1() { runner.runOneTest("BigInteger1") }
  def test_Integer01() { runner.runOneTest("Integer01") }
  def test_Int01() { runner.runOneTest("Int01") }
  def test_int_error() { runner.runOneTest("int_error") }
 
  // Test warning_exercise moved to scala-debug until warnings are implemented.
  
  def test_UnsignedNumbers1() { runner.runOneTest("UnsignedNumbers1") }
  def test_unsignedLong_01() {runner.runOneTest("unsignedLong_01")}
  def test_unsignedLong_02() {runner.runOneTest("unsignedLong_02")}
  def test_unsignedLong_03() {runner.runOneTest("unsignedLong_03")}
  def test_Long2() {runner.runOneTest("Long2")}
  def test_Long3() {runner.runOneTest("Long3")}
  def test_Long4() {runner.runOneTest("Long4")}
  def test_int_error_02() { runner.runOneTest("int_error_02") }
  def test_int_error_03() { runner.runOneTest("int_error_03") }
  def test_short_01() { runner.runOneTest("short_01") }
  def test_short_02() { runner.runOneTest("short_02") }
  def test_unsignedInt_01() { runner.runOneTest("unsignedInt_01") }
  def test_unsignedInt_02() { runner.runOneTest("unsignedInt_02") }
  def test_unsignedShort_01() { runner.runOneTest("unsignedShort_01") }
  def test_unsignedByte_01() { runner.runOneTest("unsignedByte_01") }
  def test_unsignedByte_02() { runner.runOneTest("unsignedByte_02") }
  
  // Test range checking for signed integers too!
  def test_byte_01() { runner.runOneTest("byte_01") }
  def test_byte_02() { runner.runOneTest("byte_02") }

  
  val aj = testDir + "AJ.tdml"
  lazy val runnerAJ = new DFDLTestSuite(Misc.getRequiredResource(aj))
  
  def test_AJ000() { runnerAJ.runOneTest("AJ000") }
  def test_AJ001() { runnerAJ.runOneTest("AJ001") }
  
  val testDir_01 = "/daffodil/ibm-tests/"
  val aa_01 = testDir_01 + "dpaext1.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(aa_01))
  def test_schema_types_5_01() { runner_01.runOneTest("schema_types_5_01") }
  def test_schema_types_5_02() { runner_01.runOneTest("schema_types_5_02") }
  def test_schema_types_5_03() { runner_01.runOneTest("schema_types_5_03") }
  def test_schema_types_5_04() { runner_01.runOneTest("schema_types_5_04") }
  def test_schema_types_5_05() { runner_01.runOneTest("schema_types_5_05") }
  }
