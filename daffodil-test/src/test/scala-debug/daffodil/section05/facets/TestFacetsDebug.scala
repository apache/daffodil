package daffodil.section05.facets

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
import daffodil.debugger.Debugger

class TestFacetsDebug extends JUnitSuite {
  val testDir = "/daffodil/section05/facets/"
  val aa = testDir + "Facets.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  // Note, these won't pass until Decimal is implemented
  //@Test def test_totalDigits_Pass_Decimal{ runner.runOneTest("checkTotalDigits_Pass_Decimal") }
  //@Test def test_totalDigits_Fail_Decimal{ runner.runOneTest("checkTotalDigits_Fail_Decimal") }
  //@Test def test_fractionDigits_Pass{ runner.runOneTest("checkFractionDigits_Pass") }
  //@Test def test_fractionDigits_Fail{ runner.runOneTest("checkFractionDigits_Fail") }
  //@Test def test_fractionDigits_Pass_LessDigits{ runner.runOneTest("checkFractionDigits_Pass_LessDigits")} 
  //@Test def test_totalDigitsAndFractionDigits_Pass { runner.runOneTest("checkTotalDigitsFractionDigits_Pass")}  
  //@Test def test_totalDigitsAndFractionDigits_Fail { runner.runOneTest("checkTotalDigitsFractionDigits_Fail")}  

//  @Test def test_fractionDigitsPass() { runner.runOneTest("fractionDigitsPass") }
//  @Test def test_fractionDigitsFail() { runner.runOneTest("fractionDigitsFail") }
//  @Test def test_fractionDigitsFailNeg() { runner.runOneTest("fractionDigitsFailNeg") }
//  @Test def test_fractionTotalDigitsFail() { runner.runOneTest("fractionTotalDigitsFail") }
//  @Test def test_fractionTotalDigitsPass() { runner.runOneTest("fractionTotalDigitsPass") }
//  @Test def test_fractionTotalDigitsFail2() { runner.runOneTest("fractionTotalDigitsFail2") }
//  @Test def test_fractionTotalDigitsFail3() { runner.runOneTest("fractionTotalDigitsFail3") }
//  @Test def test_fractionDigitsFailNotInt() { runner.runOneTest("fractionDigitsFailNotInt") }

//  @Test def test_totalDigits01() { runner.runOneTest("totalDigits01") }
//  @Test def test_totalDigits02() { runner.runOneTest("totalDigits02") }
//  @Test def test_totalDigits03() { runner.runOneTest("totalDigits03") }
//  @Test def test_totalDigits04() { runner.runOneTest("totalDigits04") }

//  @Test def test_totalDigits05b() { runner.runOneTest("totalDigits05b") }

//  @Test def test_facetEnum06() { runner.runOneTest("facetEnum06") }
  
//  @Test def test_minMaxInEx02() { runner.runOneTest("minMaxInEx02") }
//  @Test def test_minMaxInEx04() { runner.runOneTest("minMaxInEx04") }
//  @Test def test_minMaxInEx08() { runner.runOneTest("minMaxInEx08") }
//  @Test def test_minMaxInEx10() { runner.runOneTest("minMaxInEx10") }
//  @Test def test_minMaxInEx14() { runner.runOneTest("minMaxInEx14") }

// Issues with date and time and how they are interpreted with min/max/in/exclusive facets

//  @Test def test_minMaxInExdateTime01() { runner.runOneTest("minMaxInExdateTime01") }
//  @Test def test_minMaxInExdateTime02() { runner.runOneTest("minMaxInExdateTime02") }
//  @Test def test_minMaxInExdateTime03() { runner.runOneTest("minMaxInExdateTime03") }
//  @Test def test_minMaxInExdateTime04() { runner.runOneTest("minMaxInExdateTime04") }
//  @Test def test_minMaxInExdateTime05() { runner.runOneTest("minMaxInExdateTime05") }
//  @Test def test_minMaxInExdateTime06() { runner.runOneTest("minMaxInExdateTime06") }

}
