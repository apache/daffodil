package daffodil.section11.content_framing_properties

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestContentFramingPropertiesDebug extends JUnit3Suite {
  val testDir_01 = "/daffodil/ibm-tests/"
  val tdml1 = testDir_01 + "dpaext1.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))
  
  def test_encoding_11_01() { runner1.runOneTest("encoding_11_01") }
  //def test_encoding_11_02() { runner1.runOneTest("encoding_11_02") }
  //def test_encoding_11_03() { runner1.runOneTest("encoding_11_03") }
  
  }
