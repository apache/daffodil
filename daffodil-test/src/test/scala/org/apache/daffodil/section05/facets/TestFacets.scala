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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Test

object TestFacets extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section05/facets/Facets.tdml"
  override def createRunner() =
    Runner(tdmlDir, tdmlFile, validateTDMLFile = false, validateDFDLSchemas = false)
}

object TestFacetsValidate extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section05/facets/Facets.tdml"

  override def createRunner() =
    Runner(tdmlDir, tdmlFile, validateTDMLFile = false, validateDFDLSchemas = true)
}

class TestFacets extends TdmlTests {
  val tdmlSuite = TestFacets

  @Test def minMaxInExdateTime01 = test
  @Test def minMaxInExdateTime02 = test
  @Test def minMaxInExdateTime03 = test
  @Test def minMaxInExdateTime04 = test
  @Test def minMaxInExdateTime05 = test
  @Test def minMaxInExdateTime06 = test

  @Test def minMaxInEx08b = test
  @Test def minMaxInEx15 = test
  @Test def minMaxInEx16 = test

  @Test def maxLength07 = test
  @Test def maxLength10 = test
  @Test def maxLength11 = test

  @Test def minMaxInExChoice01 = test
  @Test def minMaxInExChoice02 = test
  @Test def minMaxInEx17 = test
  @Test def minMaxInEx18 = test

  @Test def facetEnum09 = test
  @Test def facetEnum10 = test
  @Test def facetEnum11 = test
  @Test def facetEnum12 = test
  @Test def facetCombo01 = test
  @Test def facetCombo02 = test
  @Test def facetCombo03 = test
  @Test def facetCombo04 = test
  @Test def facetEnumChoice01 = test
  @Test def facetEnumChoice02 = test
  @Test def facetEnumChoice03 = test

  @Test def facetPattern01 = test
  @Test def facetPattern02 = test
  @Test def facetPattern03 = test
  @Test def facetPattern04 = test
  @Test def facetPattern05 = test
  @Test def facetPattern06 = test
  @Test def facetPattern07 = test
  @Test def facetPattern08 = test
  @Test def facetPattern09 = test
  @Test def facetPattern10 = test

  @Test def facetEnum01 = test
  @Test def facetEnum02 = test
  @Test def facetEnum03 = test
  @Test def facetEnum04 = test
  @Test def facetEnum05 = test
  @Test def facetEnum06 = test
  @Test def facetEnum07 = test
  @Test def facetEnum08 = test

  @Test def maxLength01 = test
  @Test def maxLength02 = test
  @Test def maxLength03 = test
  @Test def maxLength04 = test
  @Test def maxLength05 = test
  @Test def maxLength06 = test

  @Test def fractionTotalDigitsFail = test

  @Test def arraysMinOccursZero = test
  @Test def arraysOccursInRange_01 = test
  @Test def arraysOccursInRange_02 = test
  @Test def arraysOccursInRange_03 = test
  @Test def arraysOccursInRange_04 = test
  @Test def arraysOccursInRange_05 = test
  @Test def arraysOccursOutOfRange_01 = test
  @Test def arraysOccursOutOfRange_02 = test

  @Test def checkMinInclusive_Pass = test
  @Test def checkMaxInclusive_Pass = test
  @Test def checkMinInclusive_Fail = test
  @Test def checkMaxInclusive_Fail = test
  @Test def checkMaxInclusive_Pass_MaxInt = test
  @Test def checkMaxInclusive_Fail_MaxInt = test
  @Test def checkMinInclusive_Fail_MinInt = test

  @Test def checkMinInclusiveDecimal_Pass = test
  @Test def checkMaxInclusiveDecimal_Pass = test
  @Test def checkMinInclusiveDecimal_Fail = test
  @Test def checkMaxInclusiveDecimal_Fail = test

  @Test def checkMinExclusiveDecimal_Pass = test
  @Test def checkMaxExclusiveDecimal_Pass = test
  @Test def checkMinExclusiveDecimal_Fail = test
  @Test def checkMaxExclusiveDecimal_Fail = test

  @Test def checkMinExclusive_Fail = test
  @Test def checkMaxExclusive_Fail = test
  @Test def checkMinExclusive_Pass = test
  @Test def checkMaxExclusive_Pass = test
  @Test def checkCombining_Pass = test
  @Test def checkCombining_Fail = test
  @Test def checkCombining_Fail_1 = test

  @Test def checkEnumeration_Pass = test
  @Test def checkEnumeration_Fail = test
  @Test def checkEnumeration_Pass_Subset = test
  @Test def checkEnumeration_Fail_Subset = test

  // Satisfied that Date and Time should also work if DateTime works.
  @Test def checkMaxInclusive_Pass_DateTime = test
  @Test def checkMaxInclusive_Fail_DateTime = test

  @Test def checkMinLength_Pass = test
  @Test def checkMinLength_Fail = test
  @Test def checkMinLength_Fail_NotString = test
  @Test def checkMinLength_Fail_Combining = test
  @Test def checkMinLength_Pass_Combining = test
  @Test def checkMaxLength_Pass = test
  @Test def checkMaxLength_Fail = test
  @Test def checkMaxLength_Fail_NotString = test
  @Test def checkMaxLength_Fail_Combining = test
  @Test def checkMaxLength_Pass_Combining = test
  @Test def checkTotalDigits_Pass = test
  @Test def checkTotalDigits_Fail = test

  @Test def minMaxInEx01 = test

  @Test def minMaxInEx02 = test
  @Test def minMaxInEx03 = test
  @Test def minMaxInEx04 = test
  @Test def minMaxInEx05 = test
  @Test def minMaxInEx06 = test
  @Test def minMaxInEx07 = test
  @Test def minMaxInEx08 = test
  @Test def minMaxInEx09 = test
  @Test def minMaxInEx10 = test
  @Test def minMaxInEx11 = test
  @Test def minMaxInEx12 = test
  @Test def minMaxInEx13 = test
  @Test def minMaxInEx14 = test

  @Test def correctNumCells0 = test
  @Test def extraCellsParsed = test
  @Test def scalarElements = test
  @Test def scalarElementsExtra = test
  @Test def lessThanMinBasic = test
  @Test def moreThanMaxBasic = test
  @Test def arrayElements = test
  @Test def upToMaxParsed = test
  @Test def lessThanMinCells = test
  @Test def moreThanMinCells = test
  @Test def lessThanMinCellsAfterLargeRow = test
  @Test def largeNumRows = test
  @Test def lessThanMinCellsAfterLargeRow_Neg = test
  @Test def fixedUnboundedMax = test
  @Test def minMaxDoNotMatch = test

  @Test def checkMaxOccurs_Pass = test
  @Test def checkMaxOccurs_Fail = test
  @Test def checkMaxOccursUnbounded_Pass = test

  @Test def testBinary = test
  @Test def checkTotalDigits_Pass_Decimal = test
  @Test def test_totalDigits_Pass_Negative = test
  @Test def checkTotalDigits_Fail_Decimal = test
  @Test def test_totalDigits_Fail_Negative = test
  @Test def checkFractionDigits_Pass = test
  @Test def checkFractionDigits_Fail = test
  @Test def checkFractionDigits_Pass_LessDigits = test
  @Test def checkTotalDigitsFractionDigits_Pass = test
  @Test def checkTotalDigitsFractionDigits_Pass2 = test

  @Test def fractionDigitsPass = test
  @Test def fractionDigitsFail = test
  @Test def fractionTotalDigitsPass = test
  @Test def fractionTotalDigitsFail2 = test
  @Test def fractionTotalDigitsPass2 = test
  @Test def fractionTotalDigitsFail3 = test

  @Test def totalDigits01 = test
  @Test def totalDigits02 = test

  @Test def totalDigits05b = test

  @Test def patternRegexDFDL708_01 = test
  @Test def patternRegexDFDL708_02 = test
  @Test def patternRegexDFDL708_03 = test
  @Test def patternRegexDFDL708_04 = test
}

class TestFacetsValidate extends TdmlTests {
  val tdmlSuite = TestFacetsValidate

  @Test def maxLength08 = test
  @Test def maxLength09 = test
  @Test def totalDigits03 = test
  @Test def totalDigits04 = test
  @Test def totalDigits05 = test
  @Test def totalDigits06 = test
  @Test def totalDigits07 = test
  @Test def totalDigits08 = test

  @Test def fractionDigitsFailNeg = test
  @Test def fractionDigitsFailNotInt = test
  @Test def totalDigits09 = test
  @Test def totalDigits10 = test
}
