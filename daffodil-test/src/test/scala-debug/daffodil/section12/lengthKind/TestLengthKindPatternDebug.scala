package daffodil.section12.lengthKind

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

class TestLengthKindPatternDebug extends JUnitSuite {
  val testDir = "/daffodil/section12/lengthKind/"
  val aa = testDir + "PatternTests.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
  
  @Test def test_lengthKindPattern_04() { runner.runOneTest("lengthKindPattern_04") }
  @Test def test_LengthPatternIllegalBits() { runner.runOneTest("LengthPatternIllegalBits") }
  @Test def test_LengthPatternIllegalBits_01() { runner.runOneTest("LengthPatternIllegalBits_01") }
  @Test def test_LengthPatternIllegalBits_02() { runner.runOneTest("LengthPatternIllegalBits_02") }
  @Test def test_LengthPatternLegalBits_01() { runner.runOneTest("LengthPatternLegalBits_01") }
  @Test def test_LengthPatternLegalBits_02() { runner.runOneTest("LengthPatternLegalBits_02") }
  //@Test def test_ExplicitLength() { runner.runOneTest("ExplicitLength") }

  }