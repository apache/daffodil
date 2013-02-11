package daffodil.section11.content_framing_properties

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

class TestContentFramingProperties extends JUnitSuite {
  val testDir_01 = "/daffodil/ibm-tests/"
  val tdml1 = testDir_01 + "dpaext1.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))

  //@Test def test_encoding_11_01() { runner1.runOneTest("encoding_11_01") }
  @Test def test_encoding_11_02() { runner1.runOneTest("encoding_11_02") }
  @Test def test_encoding_11_03() { runner1.runOneTest("encoding_11_03") }

  val testDir_02 = "/daffodil/section11/content_framing_properties/"
  val tdml2 = testDir_02 + "ContentFramingProps.tdml"
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2))

  @Test def test_UTF_16_01() { runner2.runOneTest("UTF_16_01") }
  @Test def test_xml_illegal_char_01() { runner2.runOneTest("xml_illegal_chars_01") }
  @Test def test_xml_utf8_4byte_chars() { runner2.runOneTest("xml_utf8_4byte_chars") }
  @Test def test_xml_illegal_char_02() { runner2.runOneTest("xml_illegal_chars_02") }
  @Test def test_xml_illegal_char() { runner2.runOneTest("xml_illegal_chars") }
  @Test def test_xml_utf8_4byte_chars_01() { runner2.runOneTest("xml_utf8_4byte_chars_01") }
  
  /*** DFDL-379 US-ASCII-7-bit-packed text ***/
  @Test def test_packed7BitASCII1() { runner2.runOneTest("packed7BitASCII1") }
  @Test def test_packed7BitASCII2() { runner2.runOneTest("packed7BitASCII2") }
  @Test def test_packed7BitASCII3() = { runner2.runOneTest("packed7BitASCII3") }
  @Test def test_packed7BitASCII4() = { runner2.runOneTest("packed7BitASCII4") }
  @Test def test_packed7BitASCII5() = { runner2.runOneTest("packed7BitASCII5") }
  @Test def test_packed7BitASCII6() = { runner2.runOneTest("packed7BitASCII6") }

  @Test def test_packed7BitASCII7() = { runner2.runOneTest("packed7BitASCII7") }
  @Test def test_packed7BitASCII8() = { runner2.runOneTest("packed7BitASCII8") }
  @Test def test_packed7BitASCII9() = { runner2.runOneTest("packed7BitASCII9") }

}
