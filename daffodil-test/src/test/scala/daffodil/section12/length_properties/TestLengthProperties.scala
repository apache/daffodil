package daffodil.section12.length_properties

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestLengthProperties extends JUnit3Suite {
  val testDir_01 = "/daffodil/ibm-tests/"
  val tdml_01 = testDir_01 + "dpaext1.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml_01))
  
  def test_length_explicit_12_01() { runner_01.runOneTest("length_explicit_12_01") }
  def test_length_explicit_12_02() { runner_01.runOneTest("length_explicit_12_02") }
  
  val testDir_02 = "/daffodil/section12/length_properties/"
  val tdml_02 = testDir_02 + "LengthProperties.tdml"
  lazy val runner_02 = new DFDLTestSuite(Misc.getRequiredResource(tdml_02))
  
  //def test_LengthProp_01() { runner_02.runOneTest("LengthProp_01") }
  }
