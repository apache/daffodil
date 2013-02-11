package edu.illinois.ncsa.daffodil.section11.content_framing_properties

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

class TestContentFramingPropertiesDebug extends JUnitSuite {
  val testDir_01 = "/edu.illinois.ncsa.daffodil/ibm-tests/"
  val tdml1 = testDir_01 + "dpaext1.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))

  @Test def test_encoding_11_01() { runner1.runOneTest("encoding_11_01") }
  //@Test def test_encoding_11_02() { runner1.runOneTest("encoding_11_02") }
  //@Test def test_encoding_11_03() { runner1.runOneTest("encoding_11_03") }

  val testDir_02 = "/edu.illinois.ncsa.daffodil/section11/content_framing_properties/"
  val tdml2 = testDir_02 + "ContentFramingProps.tdml"
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))
  @Test def test_encoding_property_expression() { runner2.runOneTest("encoding_property_expression") }

}
