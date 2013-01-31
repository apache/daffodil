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

class TestFacetsNew extends JUnitSuite {
  val testDir = "/daffodil/section05/facets/"
  val aa = testDir + "Facets.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_arraysMinOccursZero() { runner.runOneTest("arraysMinOccursZero") }
  @Test def test_arraysOccurInRange_01() { runner.runOneTest("arraysOccursInRange_01") }
  @Test def test_arraysOccurInRange_02() { runner.runOneTest("arraysOccursInRange_02") }
  @Test def test_arraysOccurInRange_03() { runner.runOneTest("arraysOccursInRange_03") }
  @Test def test_arraysOccurInRange_04() { runner.runOneTest("arraysOccursInRange_04") }
  @Test def test_arraysOccurInRange_05() { runner.runOneTest("arraysOccursInRange_05") }
  @Test def test_arraysOccursOutOfRange_01() { runner.runOneTest("arraysOccursOutOfRange_01") }
  @Test def test_arraysOccursOutOfRange_02() { runner.runOneTest("arraysOccursOutOfRange_02") }
  
  @Test def test_minLength_Pass { runner.runOneTest("checkMinLength_Pass") }
  @Test def test_minLength_Fail { runner.runOneTest("checkMinLength_Fail") }
  @Test def test_minLength_Fail_NotString { runner.runOneTest("checkMinLength_Fail_NotString") }
  @Test def test_minLength_Fail_Combining { runner.runOneTest("checkMinLength_Fail_Combining") }
  @Test def test_minLength_Pass_Combining { runner.runOneTest("checkMinLength_Pass_Combining") }
  @Test def test_maxLength_Pass { runner.runOneTest("checkMaxLength_Pass") }
  @Test def test_maxLength_Fail { runner.runOneTest("checkMaxLength_Fail") }
  @Test def test_maxLength_Fail_NotString { runner.runOneTest("checkMaxLength_Fail_NotString") }
  @Test def test_maxLength_Fail_Combining { runner.runOneTest("checkMaxLength_Fail_Combining") }
  @Test def test_maxLength_Pass_Combining { runner.runOneTest("checkMaxLength_Pass_Combining") }
  @Test def test_totalDigits_Pass{ runner.runOneTest("checkTotalDigits_Pass") }
  @Test def test_totalDigits_Fail{ runner.runOneTest("checkTotalDigits_Fail") }
  
  // Note, these won't pass until Decimal is implemented
  //@Test def test_totalDigits_Pass_Decimal{ runner.runOneTest("checkTotalDigits_Pass_Decimal") }
  //@Test def test_totalDigits_Fail_Decimal{ runner.runOneTest("checkTotalDigits_Fail_Decimal") }
  //@Test def test_fractionDigits_Pass{ runner.runOneTest("checkFractionDigits_Pass") }
  //@Test def test_fractionDigits_Fail{ runner.runOneTest("checkFractionDigits_Fail") }
  //@Test def test_fractionDigits_Pass_LessDigits{ runner.runOneTest("checkFractionDigits_Pass_LessDigits")} 
  //@Test def test_totalDigitsAndFractionDigits_Pass { runner.runOneTest("checkTotalDigitsFractionDigits_Pass")}  
  //@Test def test_totalDigitsAndFractionDigits_Fail { runner.runOneTest("checkTotalDigitsFractionDigits_Fail")}  

  @Test def test_minInclusive_Pass { runner.runOneTest("checkMinInclusive_Pass")}
  @Test def test_maxInclusive_Pass { runner.runOneTest("checkMaxInclusive_Pass")}
  @Test def test_minInclusive_Fail { runner.runOneTest("checkMinInclusive_Fail")}
  @Test def test_maxInclusive_Fail { runner.runOneTest("checkMaxInclusive_Fail")}
  @Test def test_maxInclusive_Pass_MaxInt { runner.runOneTest("checkMaxInclusive_Pass_MaxInt")}  
  @Test def test_maxInclusive_Fail_MaxInt { runner.runOneTest("checkMaxInclusive_Fail_MaxInt")}
  @Test def test_minInclusive_Fail_MinInt { runner.runOneTest("checkMinInclusive_Fail_MinInt")}  
  
  @Test def test_minExclusive_Fail { runner.runOneTest("checkMinExclusive_Fail")}
  @Test def test_maxExclusive_Fail { runner.runOneTest("checkMaxExclusive_Fail")}
  @Test def test_minExclusive_Pass { runner.runOneTest("checkMinExclusive_Pass")}
  @Test def test_maxExclusive_Pass { runner.runOneTest("checkMaxExclusive_Pass")}
  @Test def test_combining_Pass { runner.runOneTest("checkCombining_Pass")}
  @Test def test_combining_Fail { runner.runOneTest("checkCombining_Fail")}
  @Test def test_combining_Fail_1 { runner.runOneTest("checkCombining_Fail_1")}
  
  @Test def test_enumeration_Pass { runner.runOneTest("checkEnumeration_Pass")}
  @Test def test_enumeration_Fail { runner.runOneTest("checkEnumeration_Fail")}
  @Test def test_enumeration_Pass_Subset { runner.runOneTest("checkEnumeration_Pass_Subset")}
  @Test def test_enumeration_Fail_Subset { runner.runOneTest("checkEnumeration_Fail_Subset")}
  
  // Satisfied that Date and Time should also work if DateTime works.
  @Test def test_maxInclusive_Pass_DateTime { runner.runOneTest("checkMaxInclusive_Pass_DateTime")}
  @Test def test_maxInclusive_Fail_DateTime { runner.runOneTest("checkMaxInclusive_Fail_DateTime")}

}
