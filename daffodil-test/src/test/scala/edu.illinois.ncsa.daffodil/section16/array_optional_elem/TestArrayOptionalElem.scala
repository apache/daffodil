package edu.illinois.ncsa.daffodil.section16.array_optional_elem

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

class TestArrayOptionalElem extends JUnitSuite {
  val testDir = "/edu.illinois.ncsa.daffodil/section16/array_optional_elem/"
  val aa = testDir + "ArrayOptionalElem.tdml"

  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
  
  @Test def test_error01() { runner.runOneTest("error01") }
  @Test def test_postfixNoErr() { runner.runOneTest("postfixNoErr") }

  @Test def test_optionalElem() { runner.runOneTest("optionalElem") }
  @Test def test_optionalWithSeparators() { runner.runOneTest("optionalWithSeparators") }
  @Test def test_Lesson6_optional_element() { runner.runOneTest("Lesson6_optional_element") }
  @Test def test_Lesson6_optional_element_01() { runner.runOneTest("Lesson6_optional_element_01") }
  @Test def test_Lesson6_fixed_array() { runner.runOneTest("Lesson6_fixed_array") }
  @Test def test_Lesson6_variable_array() { runner.runOneTest("Lesson6_variable_array") }
  @Test def test_Lesson6_variable_array_01() { runner.runOneTest("Lesson6_variable_array_01") }
  @Test def test_Lesson6_variable_array_02() { runner.runOneTest("Lesson6_variable_array_02") }

  val testDir01 = "/edu.illinois.ncsa.daffodil/section05/facets/"
  val ab = testDir01 + "Facets.tdml"
  lazy val runner01 = new DFDLTestSuite(Misc.getRequiredResource(ab))
  @Test def test_leftOverData_Neg() { runner01.runOneTest("leftOverData_Neg") }
  
  val testDir1 = "/edu.illinois.ncsa.daffodil/ibm-tests/"
  val tdml1 = testDir1 + "dpaext2.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))
  @Test def test_arrays_16_01() { runner1.runOneTest("arrays_16_01") }
}
