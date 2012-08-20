package daffodil.section12.lengthKind

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestLengthKindDelimited extends JUnit3Suite {
  val testDir = "/daffodil/section12/lengthKind/"
  val aa = testDir + "DelimitedTests.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
  
  def test_NumSeq_01() { runner.runOneTest("NumSeq_01") }
  def test_NumSeq_03() { runner.runOneTest("NumSeq_03") }
  def test_NumSeq_04() { runner.runOneTest("NumSeq_04") }
  def test_lengthKindDelimited_01() { runner.runOneTest("lengthKindDelimited_01") }
  def test_lengthKindDelimited_02() { runner.runOneTest("lengthKindDelimited_02") }
  
  val ab = testDir + "AB.tdml"
  lazy val runnerAB = new DFDLTestSuite(Misc.getRequiredResource(ab))
    
  def test_AB000() { runnerAB.runOneTest("AB000") }
  def test_AB001() { runnerAB.runOneTest("AB001") }
  def test_AB002() { runnerAB.runOneTest("AB002") }
  def test_AB003() { runnerAB.runOneTest("AB003") }

  val an = testDir + "AN.tdml"
  lazy val runnerAN = new DFDLTestSuite(Misc.getRequiredResource(an))
  
  def test_AN000() { runnerAN.runOneTest("AN000") }
  def test_AN001() { runnerAN.runOneTest("AN001") }
  
  val testDir_01 = "/daffodil/ibm-tests/"
  val tdml_01 = testDir_01 + "dpaext1.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))
  
  def test_introduction_1_02() { runner_01.runOneTest("introduction_1_02") }
  def test_length_delimited_12_03() { runner_01.runOneTest("length_delimited_12_03") }
  def test_length_delimited_12_02() { runner_01.runOneTest("length_delimited_12_02") }
  def test_multiple_delimiters() { runner_01.runOneTest("multiple_delimiters") }
  
  }
