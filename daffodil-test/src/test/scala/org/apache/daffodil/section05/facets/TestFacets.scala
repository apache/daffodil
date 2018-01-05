/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.section05.facets

import org.junit.Test
import org.junit._
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestFacets {

  val testDir = "/org/apache/daffodil/section05/facets/"
  val runner = Runner(testDir, "Facets.tdml", validateTDMLFile = false, validateDFDLSchemas = false)

  @AfterClass def tearDown() { runner.reset }

}

class TestFacets {
  import TestFacets._

  @Test def test_minMaxInExdateTime01() { runner.runOneTest("minMaxInExdateTime01") }
  @Test def test_minMaxInExdateTime02() { runner.runOneTest("minMaxInExdateTime02") }
  @Test def test_minMaxInExdateTime03() { runner.runOneTest("minMaxInExdateTime03") }
  @Test def test_minMaxInExdateTime04() { runner.runOneTest("minMaxInExdateTime04") }
  @Test def test_minMaxInExdateTime05() { runner.runOneTest("minMaxInExdateTime05") }
  @Test def test_minMaxInExdateTime06() { runner.runOneTest("minMaxInExdateTime06") }

  @Test def test_minMaxInEx08b() { runner.runOneTest("minMaxInEx08b") }
  @Test def test_minMaxInEx15() { runner.runOneTest("minMaxInEx15") }
  @Test def test_minMaxInEx16() { runner.runOneTest("minMaxInEx16") }

  @Test def test_maxLength07() { runner.runOneTest("maxLength07") }
  @Test def test_maxLength08() { runner.runOneTest("maxLength08") }
  @Test def test_maxLength09() { runner.runOneTest("maxLength09") }
  @Test def test_maxLength10() { runner.runOneTest("maxLength10") }
  @Test def test_maxLength11() { runner.runOneTest("maxLength11") }

  @Test def test_minMaxInExChoice01() { runner.runOneTest("minMaxInExChoice01") }
  @Test def test_minMaxInExChoice02() { runner.runOneTest("minMaxInExChoice02") }
  @Test def test_minMaxInEx17() { runner.runOneTest("minMaxInEx17") }
  @Test def test_minMaxInEx18() { runner.runOneTest("minMaxInEx18") }

  @Test def test_facetEnum09() { runner.runOneTest("facetEnum09") }
  @Test def test_facetEnum10() { runner.runOneTest("facetEnum10") }
  @Test def test_facetEnum11() { runner.runOneTest("facetEnum11") }
  @Test def test_facetEnum12() { runner.runOneTest("facetEnum12") }
  @Test def test_facetCombo01() { runner.runOneTest("facetCombo01") }
  @Test def test_facetCombo02() { runner.runOneTest("facetCombo02") }
  @Test def test_facetCombo03() { runner.runOneTest("facetCombo03") }
  @Test def test_facetCombo04() { runner.runOneTest("facetCombo04") }
  @Test def test_facetEnumChoice01() { runner.runOneTest("facetEnumChoice01") }
  @Test def test_facetEnumChoice02() { runner.runOneTest("facetEnumChoice02") }
  @Test def test_facetEnumChoice03() { runner.runOneTest("facetEnumChoice03") }

  @Test def test_facetPattern01() { runner.runOneTest("facetPattern01") }
  @Test def test_facetPattern02() { runner.runOneTest("facetPattern02") }
  @Test def test_facetPattern03() { runner.runOneTest("facetPattern03") }
  @Test def test_facetPattern04() { runner.runOneTest("facetPattern04") }
  @Test def test_facetPattern05() { runner.runOneTest("facetPattern05") }
  @Test def test_facetPattern06() { runner.runOneTest("facetPattern06") }
  @Test def test_facetPattern07() { runner.runOneTest("facetPattern07") }
  @Test def test_facetPattern08() { runner.runOneTest("facetPattern08") }
  @Test def test_facetPattern09() { runner.runOneTest("facetPattern09") }
  @Test def test_facetPattern10() { runner.runOneTest("facetPattern10") }

  @Test def test_facetEnum01() { runner.runOneTest("facetEnum01") }
  @Test def test_facetEnum02() { runner.runOneTest("facetEnum02") }
  @Test def test_facetEnum03() { runner.runOneTest("facetEnum03") }
  @Test def test_facetEnum04() { runner.runOneTest("facetEnum04") }
  @Test def test_facetEnum05() { runner.runOneTest("facetEnum05") }
  @Test def test_facetEnum06() { runner.runOneTest("facetEnum06") }
  @Test def test_facetEnum07() { runner.runOneTest("facetEnum07") }
  @Test def test_facetEnum08() { runner.runOneTest("facetEnum08") }

  @Test def test_maxLength01() { runner.runOneTest("maxLength01") }
  @Test def test_maxLength02() { runner.runOneTest("maxLength02") }
  @Test def test_maxLength03() { runner.runOneTest("maxLength03") }
  @Test def test_maxLength04() { runner.runOneTest("maxLength04") }
  @Test def test_maxLength05() { runner.runOneTest("maxLength05") }
  @Test def test_maxLength06() { runner.runOneTest("maxLength06") }

  @Test def test_totalDigits03() { runner.runOneTest("totalDigits03") }
  @Test def test_totalDigits04() { runner.runOneTest("totalDigits04") }
  @Test def test_totalDigits05() { runner.runOneTest("totalDigits05") }
  @Test def test_totalDigits06() { runner.runOneTest("totalDigits06") }
  @Test def test_totalDigits07() { runner.runOneTest("totalDigits07") }
  @Test def test_totalDigits08() { runner.runOneTest("totalDigits08") }

  @Test def test_fractionDigitsFailNeg() { runner.runOneTest("fractionDigitsFailNeg") }
  @Test def test_fractionTotalDigitsFail() { runner.runOneTest("fractionTotalDigitsFail") }
  @Test def test_fractionDigitsFailNotInt() { runner.runOneTest("fractionDigitsFailNotInt") }

  @Test def test_arraysMinOccursZero() { runner.runOneTest("arraysMinOccursZero") }
  @Test def test_arraysOccursInRange_01() { runner.runOneTest("arraysOccursInRange_01") }
  @Test def test_arraysOccursInRange_02() { runner.runOneTest("arraysOccursInRange_02") }
  @Test def test_arraysOccursInRange_03() { runner.runOneTest("arraysOccursInRange_03") }
  @Test def test_arraysOccursInRange_04() { runner.runOneTest("arraysOccursInRange_04") }
  @Test def test_arraysOccursInRange_05() { runner.runOneTest("arraysOccursInRange_05") }
  @Test def test_arraysOccursOutOfRange_01() { runner.runOneTest("arraysOccursOutOfRange_01") }
  @Test def test_arraysOccursOutOfRange_02() { runner.runOneTest("arraysOccursOutOfRange_02") }

  @Test def test_checkMinInclusive_Pass { runner.runOneTest("checkMinInclusive_Pass") }
  @Test def test_checkMaxInclusive_Pass { runner.runOneTest("checkMaxInclusive_Pass") }
  @Test def test_checkMinInclusive_Fail { runner.runOneTest("checkMinInclusive_Fail") }
  @Test def test_checkMaxInclusive_Fail { runner.runOneTest("checkMaxInclusive_Fail") }
  @Test def test_checkMaxInclusive_Pass_MaxInt { runner.runOneTest("checkMaxInclusive_Pass_MaxInt") }
  @Test def test_checkMaxInclusive_Fail_MaxInt { runner.runOneTest("checkMaxInclusive_Fail_MaxInt") }
  @Test def test_checkMinInclusive_Fail_MinInt { runner.runOneTest("checkMinInclusive_Fail_MinInt") }

  @Test def test_checkMinExclusive_Fail { runner.runOneTest("checkMinExclusive_Fail") }
  @Test def test_checkMaxExclusive_Fail { runner.runOneTest("checkMaxExclusive_Fail") }
  @Test def test_checkMinExclusive_Pass { runner.runOneTest("checkMinExclusive_Pass") }
  @Test def test_checkMaxExclusive_Pass { runner.runOneTest("checkMaxExclusive_Pass") }
  @Test def test_checkCombining_Pass { runner.runOneTest("checkCombining_Pass") }
  @Test def test_checkCombining_Fail { runner.runOneTest("checkCombining_Fail") }
  @Test def test_checkCombining_Fail_1 { runner.runOneTest("checkCombining_Fail_1") }

  @Test def test_checkEnumeration_Pass { runner.runOneTest("checkEnumeration_Pass") }
  @Test def test_checkEnumeration_Fail { runner.runOneTest("checkEnumeration_Fail") }
  @Test def test_checkEnumeration_Pass_Subset { runner.runOneTest("checkEnumeration_Pass_Subset") }
  @Test def test_checkEnumeration_Fail_Subset { runner.runOneTest("checkEnumeration_Fail_Subset") }

  // Satisfied that Date and Time should also work if DateTime works.
  @Test def test_maxInclusive_Pass_DateTime { runner.runOneTest("checkMaxInclusive_Pass_DateTime") }
  @Test def test_maxInclusive_Fail_DateTime { runner.runOneTest("checkMaxInclusive_Fail_DateTime") }

  @Test def test_checkMinLength_Pass { runner.runOneTest("checkMinLength_Pass") }
  @Test def test_checkMinLength_Fail { runner.runOneTest("checkMinLength_Fail") }
  @Test def test_checkMinLength_Fail_NotString { runner.runOneTest("checkMinLength_Fail_NotString") }
  @Test def test_checkMinLength_Fail_Combining { runner.runOneTest("checkMinLength_Fail_Combining") }
  @Test def test_checkMinLength_Pass_Combining { runner.runOneTest("checkMinLength_Pass_Combining") }
  @Test def test_checkMaxLength_Pass { runner.runOneTest("checkMaxLength_Pass") }
  @Test def test_checkMaxLength_Fail { runner.runOneTest("checkMaxLength_Fail") }
  @Test def test_checkMaxLength_Fail_NotString { runner.runOneTest("checkMaxLength_Fail_NotString") }
  @Test def test_checkMaxLength_Fail_Combining { runner.runOneTest("checkMaxLength_Fail_Combining") }
  @Test def test_checkMaxLength_Pass_Combining { runner.runOneTest("checkMaxLength_Pass_Combining") }
  @Test def test_checkTotalDigits_Pass { runner.runOneTest("checkTotalDigits_Pass") }
  @Test def test_checkTotalDigits_Fail { runner.runOneTest("checkTotalDigits_Fail") }

  @Test def test_minMaxInEx01() { runner.runOneTest("minMaxInEx01") }

  @Test def test_minMaxInEx02() { runner.runOneTest("minMaxInEx02") }
  @Test def test_minMaxInEx03() { runner.runOneTest("minMaxInEx03") }
  @Test def test_minMaxInEx04() { runner.runOneTest("minMaxInEx04") }
  @Test def test_minMaxInEx05() { runner.runOneTest("minMaxInEx05") }
  @Test def test_minMaxInEx06() { runner.runOneTest("minMaxInEx06") }
  @Test def test_minMaxInEx07() { runner.runOneTest("minMaxInEx07") }
  @Test def test_minMaxInEx08() { runner.runOneTest("minMaxInEx08") }
  @Test def test_minMaxInEx09() { runner.runOneTest("minMaxInEx09") }
  @Test def test_minMaxInEx10() { runner.runOneTest("minMaxInEx10") }
  @Test def test_minMaxInEx11() { runner.runOneTest("minMaxInEx11") }
  @Test def test_minMaxInEx12() { runner.runOneTest("minMaxInEx12") }
  @Test def test_minMaxInEx13() { runner.runOneTest("minMaxInEx13") }
  @Test def test_minMaxInEx14() { runner.runOneTest("minMaxInEx14") }

  @Test def test_correctNumCells0() { runner.runOneTest("correctNumCells0") }
  @Test def test_extraCellsParsed() { runner.runOneTest("extraCellsParsed") }
  @Test def test_scalarElements() { runner.runOneTest("scalarElements") }
  @Test def test_scalarElementsExtra() { runner.runOneTest("scalarElementsExtra") }
  @Test def test_lessThanMinBasic() { runner.runOneTest("lessThanMinBasic") }
  @Test def test_moreThanMaxBasic() { runner.runOneTest("moreThanMaxBasic") }
  @Test def test_arrayElements() = { runner.runOneTest("arrayElements") }
  @Test def test_upToMaxParsed() { runner.runOneTest("upToMaxParsed") }
  @Test def test_lessThanMinCells() { runner.runOneTest("lessThanMinCells") }
  @Test def test_moreThanMinCells() { runner.runOneTest("moreThanMinCells") }
  @Test def test_lessThanMinCellsAfterLargeRow() { runner.runOneTest("lessThanMinCellsAfterLargeRow") }
  @Test def test_largeNumRows() { runner.runOneTest("largeNumRows") }
  @Test def test_lessThanMinCellsAfterLargeRow_Neg() { runner.runOneTest("lessThanMinCellsAfterLargeRow_Neg") }
  @Test def test_fixedUnboundedMax() { runner.runOneTest("fixedUnboundedMax") }
  @Test def test_minMaxDoNotMatch() { runner.runOneTest("minMaxDoNotMatch") }

  @Test def test_maxOccursPass() { runner.runOneTest("checkMaxOccurs_Pass") }
  @Test def test_maxOccursFail() { runner.runOneTest("checkMaxOccurs_Fail") }
  @Test def test_maxOccursUnboundedPass() { runner.runOneTest("checkMaxOccursUnbounded_Pass") }

  @Test def test_testBinary() { runner.runOneTest("testBinary") }
  @Test def test_totalDigits_Pass_Decimal { runner.runOneTest("checkTotalDigits_Pass_Decimal") }
  @Test def test_totalDigits_Fail_Decimal { runner.runOneTest("checkTotalDigits_Fail_Decimal") }
  @Test def test_fractionDigits_Pass { runner.runOneTest("checkFractionDigits_Pass") }
  @Test def test_fractionDigits_Fail { runner.runOneTest("checkFractionDigits_Fail") }
  @Test def test_fractionDigits_Pass_LessDigits { runner.runOneTest("checkFractionDigits_Pass_LessDigits") }
  @Test def test_totalDigitsAndFractionDigits_Pass { runner.runOneTest("checkTotalDigitsFractionDigits_Pass") }
  @Test def test_totalDigitsAndFractionDigits_Pass2 { runner.runOneTest("checkTotalDigitsFractionDigits_Pass2") }

  @Test def test_fractionDigitsPass() { runner.runOneTest("fractionDigitsPass") }
  @Test def test_fractionDigitsFail() { runner.runOneTest("fractionDigitsFail") }
  @Test def test_fractionTotalDigitsPass() { runner.runOneTest("fractionTotalDigitsPass") }
  @Test def test_fractionTotalDigitsFail2() { runner.runOneTest("fractionTotalDigitsFail2") }
  @Test def test_fractionTotalDigitsPass2() { runner.runOneTest("fractionTotalDigitsPass2") }
  @Test def test_fractionTotalDigitsFail3() { runner.runOneTest("fractionTotalDigitsFail3") }

  @Test def test_totalDigits01() { runner.runOneTest("totalDigits01") }
  @Test def test_totalDigits02() { runner.runOneTest("totalDigits02") }

  @Test def test_totalDigits05b() { runner.runOneTest("totalDigits05b") }
  @Test def test_totalDigits09() { runner.runOneTest("totalDigits09") }

  @Test def test_patternRegexDFDL708_01() { runner.runOneTest("patternRegexDFDL708_01") }
  @Test def test_patternRegexDFDL708_02() { runner.runOneTest("patternRegexDFDL708_02") }
  @Test def test_patternRegexDFDL708_03() { runner.runOneTest("patternRegexDFDL708_03") }
  @Test def test_patternRegexDFDL708_04() { runner.runOneTest("patternRegexDFDL708_04") }
}
