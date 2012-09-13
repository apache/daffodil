package daffodil.section12.lengthKind

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.compiler.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestLengthKindPatternDebug extends JUnit3Suite {
  val testDir = "/daffodil/section12/lengthKind/"
  val aa = testDir + "PatternTests.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
  
  def test_lengthKindPattern_04() { runner.runOneTest("lengthKindPattern_04") }
  def test_LengthPatternIllegalBits() { runner.runOneTest("LengthPatternIllegalBits") }
  def test_LengthPatternIllegalBits_01() { runner.runOneTest("LengthPatternIllegalBits_01") }
  def test_LengthPatternIllegalBits_02() { runner.runOneTest("LengthPatternIllegalBits_02") }
  def test_LengthPatternLegalBits_01() { runner.runOneTest("LengthPatternLegalBits_01") }
  def test_LengthPatternLegalBits_02() { runner.runOneTest("LengthPatternLegalBits_02") }
  //def test_ExplicitLength() { runner.runOneTest("ExplicitLength") }

  }