package daffodil.section07.property_syntax

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

class TestPropertySyntax extends JUnitSuite {
  val testDir = "/daffodil/ibm-tests/"
  val tdml = testDir + "dpaext1.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_property_syntax_7_01() { runner.runOneTest("property_syntax_7_01") }
  @Test def test_property_syntax_7_02() { runner.runOneTest("property_syntax_7_02") }
  @Test def test_property_syntax_7_03() { runner.runOneTest("property_syntax_7_03") }
  
  val testDir1 = "/daffodil/section07/property_syntax/"
  val aa = testDir1 + "PropertySyntax.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(aa))
  
  @Test def test_ShortAndLongForm() { runner1.runOneTest("ShortAndLongForm") }

}
