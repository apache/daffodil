package daffodil.section12.lengthKind

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.dsom.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestLengthKindDelimited extends JUnit3Suite {
  val testDir = "srcTest/daffodil/section12/lengthKind/"
  val aa = testDir + "DelimitedTests.tdml"
  val runner = new DFDLTestSuite(new File(aa))
  
  def test_length_delimited_12_02() { runner.runOneTest("length_delimited_12_02") }
  def test_length_delimited_12_03() { runner.runOneTest("length_delimited_12_03") }
  def test_introduction_1_02() { runner.runOneTest("introduction_1_02") }
  def test_multiple_delimiters() { runner.runOneTest("multiple_delimiters") }
  def test_NumSeq_01() { runner.runOneTest("NumSeq_01") }
  def test_NumSeq_03() { runner.runOneTest("NumSeq_03") }
  def test_NumSeq_04() { runner.runOneTest("NumSeq_04") }
  
  val ab = testDir + "AB.tdml"
  val runnerAB = new DFDLTestSuite(new File(ab))
    
  def test_AB000() { runnerAB.runOneTest("AB000") }
  def test_AB001() { runnerAB.runOneTest("AB001") }
  def test_AB002() { runnerAB.runOneTest("AB002") }
  def test_AB003() { runnerAB.runOneTest("AB003") }

  val an = testDir + "AN.tdml"
  val runnerAN = new DFDLTestSuite(new File(an))
  
  def test_AN000() { runnerAN.runOneTest("AN000") }
  def test_AN001() { runnerAN.runOneTest("AN001") }
  
  }
