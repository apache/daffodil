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

class TestDelimiterProperties_01 extends JUnit3Suite {
  
  val testDir_02 = "/daffodil/section12/delimiter_properties/"
  val tdml_02 = testDir_02 + "DelimiterProperties.tdml"
  lazy val runner_02 = new DFDLTestSuite(Misc.getRequiredResource(tdml_02))
  
  // def test_DelimProp_03() { runner_02.runOneTest("DelimProp_03") }
  def test_DelimProp_06() { runner_02.runOneTest("DelimProp_06") }
  }
