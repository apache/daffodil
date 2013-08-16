package edu.illinois.ncsa.daffodil.section02.validation_errors

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

import junit.framework.Assert._
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import org.junit.Test

class TestValidationErr {
  val testDir = "/edu/illinois/ncsa/daffodil/section02/validation_errors/"
  val aa = testDir + "Validation.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

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
//  @Test def test_checkEnumeration_Fail_on() { runner.runOneTest("checkEnumeration_Fail_on") }
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
//  @Test def test_choice_ignoreValidationErr_01() { runner.runOneTest("choice_ignoreValidationErr_01") }
  @Test def test_choice_ignoreValidationErr_02() { runner.runOneTest("choice_ignoreValidationErr_02") }
  @Test def test_choice_ignoreValidationErr_03() { runner.runOneTest("choice_ignoreValidationErr_03") }
  
  @Test def test_choice_errorNotSuppressed_01() { runner.runOneTest("choice_errorNotSuppressed_01") }
}
