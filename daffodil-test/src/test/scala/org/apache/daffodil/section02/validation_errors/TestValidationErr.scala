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

package org.apache.daffodil.section02.validation_errors

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Ignore
import org.junit.Test

object TestValidationErr extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section02/validation_errors/Validation.tdml"
}

class TestValidationErr extends TdmlTests {
  val tdmlSuite = TestValidationErr

  @Test def facetPattern01_validation_off = test
  @Test def facetPattern01_fail_limited = test
  @Test def facetPattern01_fail_full = test
  @Test def facetPattern01_success_limited = test
  @Test def facetPattern01_success_full = test

  @Test def arrayElements_fail_limited = test
  @Test def arrayElements_fail_full = test

  @Test def checkMinLength_Fail_Combining_limited = test
  @Test def checkEnumeration_Pass_limited = test
  @Test def checkEnumeration_Fail_limited = test
  @Test def checkEnumeration_Fail_on = test
  @Test def checkMaxInclusive_Fail_DateTime_limited = test

  @Test def checkMinExclusive_Fail_off = test
  @Test def checkMinExclusive_Fail_limited = test
  @Test def checkMinExclusive_Fail_on = test

  @Test def checkCombining_Fail_off = test
  @Test def checkCombining_Fail_limited = test
  @Test def checkCombining_Fail_on = test

  @Test def checkMaxLength_Fail_Combining_limited = test

  @Test def checkTotalDigits_Fail_off = test
  @Test def checkTotalDigits_Fail_limited = test
  @Test def checkTotalDigits_Fail_on = test

  @Test def facetCombos_fail_off = test
  @Test def facetCombos_fail_limited = test
  @Test def facetCombos_fail_on = test

  @Test def facetCombos_fail_off_02 = test
  @Test def facetCombos_fail_limited_02 = test
  @Test def facetCombos_fail_on_02 = test

  @Test def assert_validation_on = test
  @Test def assert_validation_limited = test
  @Test def assert_validation_off = test

  @Test def minOccurs_notValidationErr_limited = test
  @Test def choice_ignoreValidationErr_01 = test
  @Test def choice_ignoreValidationErr_02 = test
  @Test def choice_ignoreValidationErr_03 = test

  @Test def choice_errorNotSuppressed_01 = test
  @Test def choice_errorNotSuppressed_02 = test
  @Test def choice_errorNotSuppressed_03 = test
  @Test def choice_errorNotSuppressed_04 = test
  @Test def choice_errorNotSuppressed_05 = test

  // DFDL-903
  @Ignore @Test def choice_errorNotSuppressed_validationErrorCheck = test

  @Test def validation_inputValueCalc_01 = test
  @Test def validation_inputValueCalc_02 = test
  @Test def validation_inputValueCalc_03 = test
  @Test def validation_inputValueCalc_04 = test
  @Test def validation_inputValueCalc_05 = test
  @Test def validation_inputValueCalc_06 = test
  @Test def validation_inputValueCalc_07 = test
  @Test def validation_inputValueCalc_08 = test

  @Test def validation_testFacets_01 = test
  @Test def validation_testFacets_02 = test
  @Test def validation_testFacets_03 = test
  @Test def validation_testFacets_04 = test
  @Test def validation_testFacets2_01 = test
  @Test def validation_testFacets2_02 = test

  @Test def validation_testFacets3_01 = test

  @Test def floatExclusiveValid = test
  @Test def floatExclusiveInf = test
  @Test def floatExclusiveNegInf = test
  @Test def floatExclusiveNaN = test
  @Test def floatInclusiveValid = test
  @Test def floatInclusiveInf = test
  @Test def floatInclusiveNegInf = test
  @Test def floatInclusiveNaN = test

  @Test def doubleExclusiveValid = test
  @Test def doubleExclusiveInf = test
  @Test def doubleExclusiveNegInf = test
  @Test def doubleExclusiveNaN = test
  @Test def doubleInclusiveValid = test
  @Test def doubleInclusiveInf = test
  @Test def doubleInclusiveNegInf = test
  @Test def doubleInclusiveNaN = test

  @Test def optional_element_1 = test

  @Test def runtimeSdeWithRestriction_1 = test
}
