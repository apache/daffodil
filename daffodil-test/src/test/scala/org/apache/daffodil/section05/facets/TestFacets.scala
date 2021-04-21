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
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestFacets {

  val testDir = "/org/apache/daffodil/section05/facets/"
  val runner = Runner(testDir, "Facets.tdml", validateTDMLFile = false, validateDFDLSchemas = false)
  val runnerV = Runner(testDir, "Facets.tdml", validateTDMLFile = false, validateDFDLSchemas = true)

  @AfterClass def tearDown(): Unit = {
    runner.reset
    runnerV.reset
  }

}

class TestFacets {
  import TestFacets._

  @Test def test_minMaxInExdateTime01(): Unit = { runner.runOneTest("minMaxInExdateTime01") }
  @Test def test_minMaxInExdateTime02(): Unit = { runner.runOneTest("minMaxInExdateTime02") }
  @Test def test_minMaxInExdateTime03(): Unit = { runner.runOneTest("minMaxInExdateTime03") }
  @Test def test_minMaxInExdateTime04(): Unit = { runner.runOneTest("minMaxInExdateTime04") }
  @Test def test_minMaxInExdateTime05(): Unit = { runner.runOneTest("minMaxInExdateTime05") }
  @Test def test_minMaxInExdateTime06(): Unit = { runner.runOneTest("minMaxInExdateTime06") }

  @Test def test_minMaxInEx08b(): Unit = { runner.runOneTest("minMaxInEx08b") }
  @Test def test_minMaxInEx15(): Unit = { runner.runOneTest("minMaxInEx15") }
  @Test def test_minMaxInEx16(): Unit = { runner.runOneTest("minMaxInEx16") }

  @Test def test_maxLength07(): Unit = { runner.runOneTest("maxLength07") }
  @Test def test_maxLength08(): Unit = { runnerV.runOneTest("maxLength08") }
  @Test def test_maxLength09(): Unit = { runnerV.runOneTest("maxLength09") }
  @Test def test_maxLength10(): Unit = { runner.runOneTest("maxLength10") }
  @Test def test_maxLength11(): Unit = { runner.runOneTest("maxLength11") }

  @Test def test_minMaxInExChoice01(): Unit = { runner.runOneTest("minMaxInExChoice01") }
  @Test def test_minMaxInExChoice02(): Unit = { runner.runOneTest("minMaxInExChoice02") }
  @Test def test_minMaxInEx17(): Unit = { runner.runOneTest("minMaxInEx17") }
  @Test def test_minMaxInEx18(): Unit = { runner.runOneTest("minMaxInEx18") }

  @Test def test_facetEnum09(): Unit = { runner.runOneTest("facetEnum09") }
  @Test def test_facetEnum10(): Unit = { runner.runOneTest("facetEnum10") }
  @Test def test_facetEnum11(): Unit = { runner.runOneTest("facetEnum11") }
  @Test def test_facetEnum12(): Unit = { runner.runOneTest("facetEnum12") }
  @Test def test_facetCombo01(): Unit = { runner.runOneTest("facetCombo01") }
  @Test def test_facetCombo02(): Unit = { runner.runOneTest("facetCombo02") }
  @Test def test_facetCombo03(): Unit = { runner.runOneTest("facetCombo03") }
  @Test def test_facetCombo04(): Unit = { runner.runOneTest("facetCombo04") }
  @Test def test_facetEnumChoice01(): Unit = { runner.runOneTest("facetEnumChoice01") }
  @Test def test_facetEnumChoice02(): Unit = { runner.runOneTest("facetEnumChoice02") }
  @Test def test_facetEnumChoice03(): Unit = { runner.runOneTest("facetEnumChoice03") }

  @Test def test_facetPattern01(): Unit = { runner.runOneTest("facetPattern01") }
  @Test def test_facetPattern02(): Unit = { runner.runOneTest("facetPattern02") }
  @Test def test_facetPattern03(): Unit = { runner.runOneTest("facetPattern03") }
  @Test def test_facetPattern04(): Unit = { runner.runOneTest("facetPattern04") }
  @Test def test_facetPattern05(): Unit = { runner.runOneTest("facetPattern05") }
  @Test def test_facetPattern06(): Unit = { runner.runOneTest("facetPattern06") }
  @Test def test_facetPattern07(): Unit = { runner.runOneTest("facetPattern07") }
  @Test def test_facetPattern08(): Unit = { runner.runOneTest("facetPattern08") }
  @Test def test_facetPattern09(): Unit = { runner.runOneTest("facetPattern09") }
  @Test def test_facetPattern10(): Unit = { runner.runOneTest("facetPattern10") }

  @Test def test_facetEnum01(): Unit = { runner.runOneTest("facetEnum01") }
  @Test def test_facetEnum02(): Unit = { runner.runOneTest("facetEnum02") }
  @Test def test_facetEnum03(): Unit = { runner.runOneTest("facetEnum03") }
  @Test def test_facetEnum04(): Unit = { runner.runOneTest("facetEnum04") }
  @Test def test_facetEnum05(): Unit = { runner.runOneTest("facetEnum05") }
  @Test def test_facetEnum06(): Unit = { runner.runOneTest("facetEnum06") }
  @Test def test_facetEnum07(): Unit = { runner.runOneTest("facetEnum07") }
  @Test def test_facetEnum08(): Unit = { runner.runOneTest("facetEnum08") }

  @Test def test_maxLength01(): Unit = { runner.runOneTest("maxLength01") }
  @Test def test_maxLength02(): Unit = { runner.runOneTest("maxLength02") }
  @Test def test_maxLength03(): Unit = { runner.runOneTest("maxLength03") }
  @Test def test_maxLength04(): Unit = { runner.runOneTest("maxLength04") }
  @Test def test_maxLength05(): Unit = { runner.runOneTest("maxLength05") }
  @Test def test_maxLength06(): Unit = { runner.runOneTest("maxLength06") }

  @Test def test_totalDigits03(): Unit = { runnerV.runOneTest("totalDigits03") }
  @Test def test_totalDigits04(): Unit = { runnerV.runOneTest("totalDigits04") }
  @Test def test_totalDigits05(): Unit = { runnerV.runOneTest("totalDigits05") }
  @Test def test_totalDigits06(): Unit = { runnerV.runOneTest("totalDigits06") }
  @Test def test_totalDigits07(): Unit = { runnerV.runOneTest("totalDigits07") }
  @Test def test_totalDigits08(): Unit = { runnerV.runOneTest("totalDigits08") }

  @Test def test_fractionDigitsFailNeg(): Unit = { runnerV.runOneTest("fractionDigitsFailNeg") }
  @Test def test_fractionTotalDigitsFail(): Unit = { runner.runOneTest("fractionTotalDigitsFail") }
  @Test def test_fractionDigitsFailNotInt(): Unit = { runnerV.runOneTest("fractionDigitsFailNotInt") }

  @Test def test_arraysMinOccursZero(): Unit = { runner.runOneTest("arraysMinOccursZero") }
  @Test def test_arraysOccursInRange_01(): Unit = { runner.runOneTest("arraysOccursInRange_01") }
  @Test def test_arraysOccursInRange_02(): Unit = { runner.runOneTest("arraysOccursInRange_02") }
  @Test def test_arraysOccursInRange_03(): Unit = { runner.runOneTest("arraysOccursInRange_03") }
  @Test def test_arraysOccursInRange_04(): Unit = { runner.runOneTest("arraysOccursInRange_04") }
  @Test def test_arraysOccursInRange_05(): Unit = { runner.runOneTest("arraysOccursInRange_05") }
  @Test def test_arraysOccursOutOfRange_01(): Unit = { runner.runOneTest("arraysOccursOutOfRange_01") }
  @Test def test_arraysOccursOutOfRange_02(): Unit = { runner.runOneTest("arraysOccursOutOfRange_02") }

  @Test def test_checkMinInclusive_Pass(): Unit = { runner.runOneTest("checkMinInclusive_Pass") }
  @Test def test_checkMaxInclusive_Pass(): Unit = { runner.runOneTest("checkMaxInclusive_Pass") }
  @Test def test_checkMinInclusive_Fail(): Unit = { runner.runOneTest("checkMinInclusive_Fail") }
  @Test def test_checkMaxInclusive_Fail(): Unit = { runner.runOneTest("checkMaxInclusive_Fail") }
  @Test def test_checkMaxInclusive_Pass_MaxInt(): Unit = { runner.runOneTest("checkMaxInclusive_Pass_MaxInt") }
  @Test def test_checkMaxInclusive_Fail_MaxInt(): Unit = { runner.runOneTest("checkMaxInclusive_Fail_MaxInt") }
  @Test def test_checkMinInclusive_Fail_MinInt(): Unit = { runner.runOneTest("checkMinInclusive_Fail_MinInt") }

  @Test def test_checkMinInclusiveDecimal_Pass(): Unit = { runner.runOneTest("checkMinInclusiveDecimal_Pass") }
  @Test def test_checkMaxInclusiveDecimal_Pass(): Unit = { runner.runOneTest("checkMaxInclusiveDecimal_Pass") }
  @Test def test_checkMinInclusiveDecimal_Fail(): Unit = { runner.runOneTest("checkMinInclusiveDecimal_Fail") }
  @Test def test_checkMaxInclusiveDecimal_Fail(): Unit = { runner.runOneTest("checkMaxInclusiveDecimal_Fail") }

  @Test def test_checkMinExclusiveDecimal_Pass(): Unit = { runner.runOneTest("checkMinExclusiveDecimal_Pass") }
  @Test def test_checkMaxExclusiveDecimal_Pass(): Unit = { runner.runOneTest("checkMaxExclusiveDecimal_Pass") }
  @Test def test_checkMinExclusiveDecimal_Fail(): Unit = { runner.runOneTest("checkMinExclusiveDecimal_Fail") }
  @Test def test_checkMaxExclusiveDecimal_Fail(): Unit = { runner.runOneTest("checkMaxExclusiveDecimal_Fail") }

  @Test def test_checkMinExclusive_Fail(): Unit = { runner.runOneTest("checkMinExclusive_Fail") }
  @Test def test_checkMaxExclusive_Fail(): Unit = { runner.runOneTest("checkMaxExclusive_Fail") }
  @Test def test_checkMinExclusive_Pass(): Unit = { runner.runOneTest("checkMinExclusive_Pass") }
  @Test def test_checkMaxExclusive_Pass(): Unit = { runner.runOneTest("checkMaxExclusive_Pass") }
  @Test def test_checkCombining_Pass(): Unit = { runner.runOneTest("checkCombining_Pass") }
  @Test def test_checkCombining_Fail(): Unit = { runner.runOneTest("checkCombining_Fail") }
  @Test def test_checkCombining_Fail_1(): Unit = { runner.runOneTest("checkCombining_Fail_1") }

  @Test def test_checkEnumeration_Pass(): Unit = { runner.runOneTest("checkEnumeration_Pass") }
  @Test def test_checkEnumeration_Fail(): Unit = { runner.runOneTest("checkEnumeration_Fail") }
  @Test def test_checkEnumeration_Pass_Subset(): Unit = { runner.runOneTest("checkEnumeration_Pass_Subset") }
  @Test def test_checkEnumeration_Fail_Subset(): Unit = { runner.runOneTest("checkEnumeration_Fail_Subset") }

  // Satisfied that Date and Time should also work if DateTime works.
  @Test def test_maxInclusive_Pass_DateTime(): Unit = { runner.runOneTest("checkMaxInclusive_Pass_DateTime") }
  @Test def test_maxInclusive_Fail_DateTime(): Unit = { runner.runOneTest("checkMaxInclusive_Fail_DateTime") }

  @Test def test_checkMinLength_Pass(): Unit = { runner.runOneTest("checkMinLength_Pass") }
  @Test def test_checkMinLength_Fail(): Unit = { runner.runOneTest("checkMinLength_Fail") }
  @Test def test_checkMinLength_Fail_NotString(): Unit = { runner.runOneTest("checkMinLength_Fail_NotString") }
  @Test def test_checkMinLength_Fail_Combining(): Unit = { runner.runOneTest("checkMinLength_Fail_Combining") }
  @Test def test_checkMinLength_Pass_Combining(): Unit = { runner.runOneTest("checkMinLength_Pass_Combining") }
  @Test def test_checkMaxLength_Pass(): Unit = { runner.runOneTest("checkMaxLength_Pass") }
  @Test def test_checkMaxLength_Fail(): Unit = { runner.runOneTest("checkMaxLength_Fail") }
  @Test def test_checkMaxLength_Fail_NotString(): Unit = { runner.runOneTest("checkMaxLength_Fail_NotString") }
  @Test def test_checkMaxLength_Fail_Combining(): Unit = { runner.runOneTest("checkMaxLength_Fail_Combining") }
  @Test def test_checkMaxLength_Pass_Combining(): Unit = { runner.runOneTest("checkMaxLength_Pass_Combining") }
  @Test def test_checkTotalDigits_Pass(): Unit = { runner.runOneTest("checkTotalDigits_Pass") }
  @Test def test_checkTotalDigits_Fail(): Unit = { runner.runOneTest("checkTotalDigits_Fail") }

  @Test def test_minMaxInEx01(): Unit = { runner.runOneTest("minMaxInEx01") }

  @Test def test_minMaxInEx02(): Unit = { runner.runOneTest("minMaxInEx02") }
  @Test def test_minMaxInEx03(): Unit = { runner.runOneTest("minMaxInEx03") }
  @Test def test_minMaxInEx04(): Unit = { runner.runOneTest("minMaxInEx04") }
  @Test def test_minMaxInEx05(): Unit = { runner.runOneTest("minMaxInEx05") }
  @Test def test_minMaxInEx06(): Unit = { runner.runOneTest("minMaxInEx06") }
  @Test def test_minMaxInEx07(): Unit = { runner.runOneTest("minMaxInEx07") }
  @Test def test_minMaxInEx08(): Unit = { runner.runOneTest("minMaxInEx08") }
  @Test def test_minMaxInEx09(): Unit = { runner.runOneTest("minMaxInEx09") }
  @Test def test_minMaxInEx10(): Unit = { runner.runOneTest("minMaxInEx10") }
  @Test def test_minMaxInEx11(): Unit = { runner.runOneTest("minMaxInEx11") }
  @Test def test_minMaxInEx12(): Unit = { runner.runOneTest("minMaxInEx12") }
  @Test def test_minMaxInEx13(): Unit = { runner.runOneTest("minMaxInEx13") }
  @Test def test_minMaxInEx14(): Unit = { runner.runOneTest("minMaxInEx14") }

  @Test def test_correctNumCells0(): Unit = { runner.runOneTest("correctNumCells0") }
  @Test def test_extraCellsParsed(): Unit = { runner.runOneTest("extraCellsParsed") }
  @Test def test_scalarElements(): Unit = { runner.runOneTest("scalarElements") }
  @Test def test_scalarElementsExtra(): Unit = { runner.runOneTest("scalarElementsExtra") }
  @Test def test_lessThanMinBasic(): Unit = { runner.runOneTest("lessThanMinBasic") }
  @Test def test_moreThanMaxBasic(): Unit = { runner.runOneTest("moreThanMaxBasic") }
  @Test def test_arrayElements() = { runner.runOneTest("arrayElements") }
  @Test def test_upToMaxParsed(): Unit = { runner.runOneTest("upToMaxParsed") }
  @Test def test_lessThanMinCells(): Unit = { runner.runOneTest("lessThanMinCells") }
  @Test def test_moreThanMinCells(): Unit = { runner.runOneTest("moreThanMinCells") }
  @Test def test_lessThanMinCellsAfterLargeRow(): Unit = { runner.runOneTest("lessThanMinCellsAfterLargeRow") }
  @Test def test_largeNumRows(): Unit = { runner.runOneTest("largeNumRows") }
  @Test def test_lessThanMinCellsAfterLargeRow_Neg(): Unit = { runner.runOneTest("lessThanMinCellsAfterLargeRow_Neg") }
  @Test def test_fixedUnboundedMax(): Unit = { runner.runOneTest("fixedUnboundedMax") }
  @Test def test_minMaxDoNotMatch(): Unit = { runner.runOneTest("minMaxDoNotMatch") }

  @Test def test_maxOccursPass(): Unit = { runner.runOneTest("checkMaxOccurs_Pass") }
  @Test def test_maxOccursFail(): Unit = { runner.runOneTest("checkMaxOccurs_Fail") }
  @Test def test_maxOccursUnboundedPass(): Unit = { runner.runOneTest("checkMaxOccursUnbounded_Pass") }

  @Test def test_testBinary(): Unit = { runner.runOneTest("testBinary") }
  @Test def test_totalDigits_Pass_Decimal(): Unit = { runner.runOneTest("checkTotalDigits_Pass_Decimal") }
  @Test def test_totalDigits_Pass_Negative(): Unit = { runner.runOneTest("test_totalDigits_Pass_Negative") }
  @Test def test_totalDigits_Fail_Decimal(): Unit = { runner.runOneTest("checkTotalDigits_Fail_Decimal") }
  @Test def test_totalDigits_Fail_Negative(): Unit = { runner.runOneTest("test_totalDigits_Fail_Negative") }
  @Test def test_fractionDigits_Pass(): Unit = { runner.runOneTest("checkFractionDigits_Pass") }
  @Test def test_fractionDigits_Fail(): Unit = { runner.runOneTest("checkFractionDigits_Fail") }
  @Test def test_fractionDigits_Pass_LessDigits(): Unit = { runner.runOneTest("checkFractionDigits_Pass_LessDigits") }
  @Test def test_totalDigitsAndFractionDigits_Pass(): Unit = { runner.runOneTest("checkTotalDigitsFractionDigits_Pass") }
  @Test def test_totalDigitsAndFractionDigits_Pass2(): Unit = { runner.runOneTest("checkTotalDigitsFractionDigits_Pass2") }

  @Test def test_fractionDigitsPass(): Unit = { runner.runOneTest("fractionDigitsPass") }
  @Test def test_fractionDigitsFail(): Unit = { runner.runOneTest("fractionDigitsFail") }
  @Test def test_fractionTotalDigitsPass(): Unit = { runner.runOneTest("fractionTotalDigitsPass") }
  @Test def test_fractionTotalDigitsFail2(): Unit = { runner.runOneTest("fractionTotalDigitsFail2") }
  @Test def test_fractionTotalDigitsPass2(): Unit = { runner.runOneTest("fractionTotalDigitsPass2") }
  @Test def test_fractionTotalDigitsFail3(): Unit = { runner.runOneTest("fractionTotalDigitsFail3") }

  @Test def test_totalDigits01(): Unit = { runner.runOneTest("totalDigits01") }
  @Test def test_totalDigits02(): Unit = { runner.runOneTest("totalDigits02") }

  @Test def test_totalDigits05b(): Unit = { runner.runOneTest("totalDigits05b") }
  @Test def test_totalDigits09(): Unit = { runnerV.runOneTest("totalDigits09") }
  @Test def test_totalDigits10(): Unit = { runnerV.runOneTest("totalDigits10") }

  @Test def test_patternRegexDFDL708_01(): Unit = { runner.runOneTest("patternRegexDFDL708_01") }
  @Test def test_patternRegexDFDL708_02(): Unit = { runner.runOneTest("patternRegexDFDL708_02") }
  @Test def test_patternRegexDFDL708_03(): Unit = { runner.runOneTest("patternRegexDFDL708_03") }
  @Test def test_patternRegexDFDL708_04(): Unit = { runner.runOneTest("patternRegexDFDL708_04") }
}
