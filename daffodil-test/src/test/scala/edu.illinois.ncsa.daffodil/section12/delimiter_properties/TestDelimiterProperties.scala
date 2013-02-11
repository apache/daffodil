package edu.illinois.ncsa.daffodil.section12.delimiter_properties

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
import edu.illinois.ncsa.daffodil.debugger.Debugger

class TestDelimiterProperties extends JUnitSuite {

  val testDir_01 = "/edu.illinois.ncsa.daffodil/ibm-tests/"
  val tdml_01 = testDir_01 + "dpaext1.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))

  @Test def test_delimiter_12_01() { runner_01.runOneTest("delimiter_12_01") }
  //@Test def test_delimiter_12_02() { runner_01.runOneTest("delimiter_12_02") }
  @Test def test_delimiter_12_03() { runner_01.runOneTest("delimiter_12_03") }
  @Test def test_delimiter_12_04() { runner_01.runOneTest("delimiter_12_04") }

  val testDir_02 = "/edu.illinois.ncsa.daffodil/section12/delimiter_properties/"
  val tdml_02 = testDir_02 + "DelimiterProperties.tdml"
  lazy val runner_02 = new DFDLTestSuite(Misc.getRequiredResource(tdml_02))

  @Test def test_DelimProp_01() = { runner_02.runOneTest("DelimProp_01") }
  @Test def testParseSequence4() { runner_02.runOneTest("ParseSequence4") }
  @Test def testParseSequence5() { runner_02.runOneTest("ParseSequence5") }
  //@Test def testParseSequence_4a() { runner_02.runOneTest("ParseSequence_4a") }
  @Test def test_DelimProp_02() { runner_02.runOneTest("DelimProp_02") }
  @Test def test_DelimProp_03() { runner_02.runOneTest("DelimProp_03") }
  @Test def test_DelimProp_04() { runner_02.runOneTest("DelimProp_04") }
  @Test def test_DelimProp_05() { runner_02.runOneTest("DelimProp_05") }
  @Test def test_DelimProp_06() { runner_02.runOneTest("DelimProp_06") }
  @Test def test_DelimProp_07() { runner_02.runOneTest("DelimProp_07") }
  @Test def test_initiatedContentSimple1() { runner_02.runOneTest("initiatedContentSimple1") }
  @Test def test_Lesson4_initiators_terminators() { runner_02.runOneTest("Lesson4_initiators_terminators") }

  @Test def test_DelimProp_10() = {
    runner_02.runOneTest("DelimProp_10")
  }
  @Test def test_DelimProp_10_01() = {
    runner_02.runOneTest("DelimProp_10_01")
  }

  @Test def test_E1() = {
    runner_02.runOneTest("E1")
  }
}
