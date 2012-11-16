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

//class TestLengthKindPatternDebug extends JUnitSuite {
//  val testDir = "/daffodil/section12/lengthKind/"
//  val aa = testDir + "PatternTests.tdml"
//  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))
//  
////  @Test def test_LengthPatternNil_NoNil() { runner.runOneTest("LengthPatternNil_NoNil") }
////  @Test def test_LengthPatternNil_FindsNil() { runner.runOneTest("LengthPatternNil_FindsNil") }
////  @Test def test_LengthPatternNil_EmptyStringAllowed() { runner.runOneTest("LengthPatternNil_EmptyStringAllowed") }
////  @Test def test_nested_patterns() { runner.runOneTest("nested_patterns") }
////  @Test def test_nested_patterns_01() { runner.runOneTest("nested_patterns_01") }
//
//  }