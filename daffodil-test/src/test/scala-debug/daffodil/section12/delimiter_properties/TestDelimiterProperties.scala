package daffodil.section12.delimiter_properties

import junit.framework.Assert._
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestDelimiterProperties_01 extends JUnitSuite {
  
  val testDir_02 = "/daffodil/section12/delimiter_properties/"
  val tdml_02 = testDir_02 + "DelimiterProperties.tdml"
  lazy val runner_02 = new DFDLTestSuite(Misc.getRequiredResource(tdml_02))
  
    
  // @Test def test_DelimProp_03() { runner_02.runOneTest("DelimProp_03") }
  @Test def test_DelimProp_06() { runner_02.runOneTest("DelimProp_06") }
  @Test def test_DelimProp_07() { runner_02.runOneTest("DelimProp_07") }
  
  }
