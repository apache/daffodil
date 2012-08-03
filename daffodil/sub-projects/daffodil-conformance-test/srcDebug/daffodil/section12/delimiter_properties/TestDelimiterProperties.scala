package daffodil.section12.delimiter_properties

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.dsom.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestDelimiterProperties_01 extends JUnit3Suite {
  val testDir_01 = "srcTest/daffodil/ibm-tests/"
  val tdml_01 = testDir_01 + "dpaext1.tdml"
  val runner_01 = new DFDLTestSuite(new File(tdml_01))
  
  def test_delimiter_12_03() { runner_01.runOneTest("delimiter_12_03") }
  
  val testDir_02 = "srcDebug/daffodil/section12/delimiter_properties/"
  val tdml_02 = testDir_02 + "DelimiterProperties.tdml"
  val runner_02 = new DFDLTestSuite(new File(tdml_02))
  
  def test_DelimProp_01() { runner_02.runOneTest("DelimProp_01") }
  def testParseSequence4() { runner_02.runOneTest("ParseSequence4") }
  def testParseSequence5() { runner_02.runOneTest("ParseSequence5") }
  def testParseSequence_4a() { runner_02.runOneTest("ParseSequence_4a") }
  def test_DelimProp_02() { runner_02.runOneTest("DelimProp_02") }
  def test_DelimProp_03() { runner_02.runOneTest("DelimProp_03") }
  }
