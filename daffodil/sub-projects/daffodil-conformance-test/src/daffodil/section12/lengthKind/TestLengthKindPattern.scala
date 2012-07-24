package daffodil.section12.lengthKind

import junit.framework.Assert._
import org.scalatest.junit.JUnit3Suite
import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import daffodil.dsom.Compiler
import daffodil.util._
import daffodil.tdml.DFDLTestSuite
import java.io.File

class TestLengthKindPattern extends JUnit3Suite {
  val testDir = "src/daffodil/section12/lengthKind/"
  val aa = testDir + "PatternTests.tdml"
  val runner = new DFDLTestSuite(new File(aa))
  
  def test_AI000_rev() { runner.runOneTest("AI000_rev") }
  def testLengthKindPattern() { runner.runOneTest("LengthKindPattern") }
  def testLengthKindPatternCompound() { runner.runOneTest("LengthKindPatternCompound") }
  def test_lengthKindPattern_01() { runner.runOneTest("lengthKindPattern_01") }
  def test_lengthKindPattern_02() { runner.runOneTest("lengthKindPattern_02") }
  def test_lengthKindPattern_03() { runner.runOneTest("lengthKindPattern_03") }

  }