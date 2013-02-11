package edu.illinois.ncsa.daffodil.section07.property_syntax

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

class TestPropertySyntax extends JUnitSuite {
  val testDir = "/edu.illinois.ncsa.daffodil/ibm-tests/"
  val tdml = testDir + "dpaext1.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_property_syntax_7_01() { runner.runOneTest("property_syntax_7_01") }
  @Test def test_property_syntax_7_02() { runner.runOneTest("property_syntax_7_02") }
  @Test def test_property_syntax_7_03() { runner.runOneTest("property_syntax_7_03") }
  
  val testDir1 = "/edu.illinois.ncsa.daffodil/section07/property_syntax/"
  val aa = testDir1 + "PropertySyntax.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(aa))
  
  @Test def test_ShortAndLongForm() { runner1.runOneTest("ShortAndLongForm") }
  @Test def test_ShortAnnotationAndElementForm() { runner1.runOneTest("ShortAnnotationAndElementForm") }
  @Test def test_AnnotationAndElementForm() { runner1.runOneTest("AnnotationAndElementForm") }
  @Test def test_ShortAndElementForm() { runner1.runOneTest("ShortAndElementForm") }
  @Test def test_Lesson3_attribute_form() { runner1.runOneTest("Lesson3_attribute_form") }
  @Test def test_Lesson3_element_form() { runner1.runOneTest("Lesson3_element_form") }
  @Test def test_Lesson3_short_form() { runner1.runOneTest("Lesson3_short_form") }
//  @Test def test_encodingEmptyFail() { runner1.runOneTestNoTDMLValidation("encodingEmptyFail") }

}
