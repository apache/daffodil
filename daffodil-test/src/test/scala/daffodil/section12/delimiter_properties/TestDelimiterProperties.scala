package daffodil.section12.delimiter_properties

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestDelimiterProperties extends JUnit3Suite {
  val testDir_01 = "/daffodil/ibm-tests/"
  val tdml_01 = testDir_01 + "dpaext1.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))
  
  //def test_delimiter_12_01() { runner_01.runOneTest("delimiter_12_01") }
  //def test_delimiter_12_02() { runner_01.runOneTest("delimiter_12_02") }
  def test_delimiter_12_03() { runner_01.runOneTest("delimiter_12_03") }
  def test_delimiter_12_04() { runner_01.runOneTest("delimiter_12_04") }
  
  val testDir_02 = "/daffodil/section12/delimiter_properties/"
  val tdml_02 = testDir_02 + "DelimiterProperties.tdml"
  lazy val runner_02 = new DFDLTestSuite(Misc.getRequiredResource(tdml_02))
  
  def test_DelimProp_01() { runner_02.runOneTest("DelimProp_01") }
  def testParseSequence4() { runner_02.runOneTest("ParseSequence4") }
  def testParseSequence5() { runner_02.runOneTest("ParseSequence5") }
  def testParseSequence_4a() { runner_02.runOneTest("ParseSequence_4a") }
  def test_DelimProp_02() { runner_02.runOneTest("DelimProp_02") }
  //def test_DelimProp_03() { runner_02.runOneTest("DelimProp_03") }
  def test_DelimProp_04() { runner_02.runOneTest("DelimProp_04") }
  def test_DelimProp_05() { runner_02.runOneTest("DelimProp_05") }
  //def test_DelimProp_06() { runner_02.runOneTest("DelimProp_06") }
  def test_DelimProp_09() { runner_02.runOneTest("DelimProp_09") }
  def test_DelimProp_10() { runner_02.runOneTest("DelimProp_10") }
  }
