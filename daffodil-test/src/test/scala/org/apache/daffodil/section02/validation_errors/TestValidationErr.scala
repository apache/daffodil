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

import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass
import org.junit.Test

object TestValidationErr {
  val testDir = "/org/apache/daffodil/section02/validation_errors/"
  val runner = Runner(testDir, "Validation.tdml")

  @AfterClass def shutDown() {
    runner.reset
  }
}
class TestValidationErr {
  import TestValidationErr._
  @Test def test_facetPattern01_validation_off() { runner.runOneTest("facetPattern01_validation_off") }
  @Test def test_facetPattern01_fail_limited() { runner.runOneTest("facetPattern01_fail_limited") }
  @Test def test_facetPattern01_fail_full() { runner.runOneTest("facetPattern01_fail_full") }
  @Test def test_facetPattern01_success_limited() { runner.runOneTest("facetPattern01_success_limited") }
  @Test def test_facetPattern01_success_full() { runner.runOneTest("facetPattern01_success_full") }

  @Test def test_arrayElements_fail_limited() { runner.runOneTest("arrayElements_fail_limited") }
  @Test def test_arrayElements_fail_full() { runner.runOneTest("arrayElements_fail_full") }

  @Test def test_checkMinLength_Fail_Combining_limited() { runner.runOneTest("checkMinLength_Fail_Combining_limited") }
  @Test def test_checkEnumeration_Pass_limited() { runner.runOneTest("checkEnumeration_Pass_limited") }
  @Test def test_checkEnumeration_Fail_limited() { runner.runOneTest("checkEnumeration_Fail_limited") }
  @Test def test_checkEnumeration_Fail_on() { runner.runOneTest("checkEnumeration_Fail_on") }
  @Test def test_checkMaxInclusive_Fail_DateTime_limited() { runner.runOneTest("checkMaxInclusive_Fail_DateTime_limited") }

  @Test def test_checkMinExclusive_Fail_off() { runner.runOneTest("checkMinExclusive_Fail_off") }
  @Test def test_checkMinExclusive_Fail_limited() { runner.runOneTest("checkMinExclusive_Fail_limited") }
  @Test def test_checkMinExclusive_Fail_on() { runner.runOneTest("checkMinExclusive_Fail_on") }

  @Test def test_checkCombining_Fail_off() { runner.runOneTest("checkCombining_Fail_off") }
  @Test def test_checkCombining_Fail_limited() { runner.runOneTest("checkCombining_Fail_limited") }
  @Test def test_checkCombining_Fail_on() { runner.runOneTest("checkCombining_Fail_on") }

  @Test def test_checkMaxLength_Fail_Combining_limited() { runner.runOneTest("checkMaxLength_Fail_Combining_limited") }

  @Test def test_checkTotalDigits_Fail_off() { runner.runOneTest("checkTotalDigits_Fail_off") }
  @Test def test_checkTotalDigits_Fail_limited() { runner.runOneTest("checkTotalDigits_Fail_limited") }
  @Test def test_checkTotalDigits_Fail_on() { runner.runOneTest("checkTotalDigits_Fail_on") }

  @Test def test_facetCombos_fail_off() { runner.runOneTest("facetCombos_fail_off") }
  @Test def test_facetCombos_fail_limited() { runner.runOneTest("facetCombos_fail_limited") }
  @Test def test_facetCombos_fail_on() { runner.runOneTest("facetCombos_fail_on") }

  @Test def test_facetCombos_fail_off_02() { runner.runOneTest("facetCombos_fail_off_02") }
  @Test def test_facetCombos_fail_limited_02() { runner.runOneTest("facetCombos_fail_limited_02") }
  @Test def test_facetCombos_fail_on_02() { runner.runOneTest("facetCombos_fail_on_02") }

  @Test def test_assert_validation_on() { runner.runOneTest("assert_validation_on") }
  @Test def test_assert_validation_limited() { runner.runOneTest("assert_validation_limited") }
  @Test def test_assert_validation_off() { runner.runOneTest("assert_validation_off") }

  @Test def test_minOccurs_notValidationErr_limited() { runner.runOneTest("minOccurs_notValidationErr_limited") }
  @Test def test_choice_ignoreValidationErr_01() { runner.runOneTest("choice_ignoreValidationErr_01") }
  @Test def test_choice_ignoreValidationErr_02() { runner.runOneTest("choice_ignoreValidationErr_02") }
  @Test def test_choice_ignoreValidationErr_03() { runner.runOneTest("choice_ignoreValidationErr_03") }

  @Test def test_choice_errorNotSuppressed_01() { runner.runOneTest("choice_errorNotSuppressed_01") }
  @Test def test_choice_errorNotSuppressed_02() { runner.runOneTest("choice_errorNotSuppressed_02") }
  @Test def test_choice_errorNotSuppressed_03() { runner.runOneTest("choice_errorNotSuppressed_03") }
  @Test def test_choice_errorNotSuppressed_04() { runner.runOneTest("choice_errorNotSuppressed_04") }
  @Test def test_choice_errorNotSuppressed_05() { runner.runOneTest("choice_errorNotSuppressed_05") }

  // DFDL-903
  //  @Test def test_choice_errorNotSuppressed_validationErrorCheck() { runner.runOneTest("choice_errorNotSuppressed_validationErrorCheck") }

  @Test def test_validation_inputValueCalc_01() { runner.runOneTest("validation_inputValueCalc_01") }
  @Test def test_validation_inputValueCalc_02() { runner.runOneTest("validation_inputValueCalc_02") }
  @Test def test_validation_inputValueCalc_03() { runner.runOneTest("validation_inputValueCalc_03") }
}
