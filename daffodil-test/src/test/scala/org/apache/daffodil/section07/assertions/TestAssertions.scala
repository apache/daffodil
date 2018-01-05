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

package org.apache.daffodil.section07.assertions

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestAssertions {
  val testDir = "/org/apache/daffodil/section07/assertions/"
  val runner = Runner(testDir, "assert.tdml", validateTDMLFile = false)

  @AfterClass def tearDown {
    runner.reset
  }

}

class TestAssertions {

  import TestAssertions._

  @Test def test_assertPass() { runner.runOneTest("assertPass") }
  @Test def test_assertFail1() { runner.runOneTest("assertFail1") }
  @Test def test_assertFail2() { runner.runOneTest("assertFail2") }

  // DFDL-1043
  // @Test def test_assertFailShowsValue2() { runner.runOneTest("assertFailShowsValue2") }
  @Test def test_assertFailShowsDetails() { runner.runOneTest("assertFailShowsDetails") }
  @Test def test_assertWithWhitespace() { runner.runOneTest("assertWithWhitespace") }
  @Test def test_assertWithWhitespaceAndCdata() { runner.runOneTest("assertWithWhitespaceAndCdata") }

  @Test def test_assertGuidesChoice() { runner.runOneTest("assertGuidesChoice") }

  @Test def test_assertPatternLiteralTextMatch() = { runner.runOneTest("assertPatternLiteralTextMatch") }
  @Test def test_assertPatternCombinedTextMatch() = { runner.runOneTest("assertPatternCombinedTextMatch") }
  @Test def test_assertPatternCombinedTextMatch2() = { runner.runOneTest("assertPatternCombinedTextMatch2") }
  @Test def test_assertPatternCombinedTextMatch3() = { runner.runOneTest("assertPatternCombinedTextMatch3") }

  @Test def test_assertPatternPass() { runner.runOneTest("assertPatternPass") }
  @Test def test_assertPatternFail() { runner.runOneTest("assertPatternFail") }
  @Test def test_assertPatternPass2() { runner.runOneTest("assertPatternPass2") }
  @Test def test_assertPatternPass3() { runner.runOneTest("assertPatternPass3") }
  @Test def test_assertPatternFail2() { runner.runOneTest("assertPatternFail2") }
  @Test def test_assertPatternInitsTerms() { runner.runOneTest("assertPatternInitsTerms") }
  @Test def test_assertOnSequence() { runner.runOneTest("assertOnSequence") }

  @Test def test_assertOnGroupRef() { runner.runOneTest("assertOnGroupRef") }
  @Test def test_assertOnElemRef() { runner.runOneTest("assertOnElemRef") }

  @Test def test_assertPatternMatch() { runner.runOneTest("assertPatternMatch") }
  @Test def test_assertPatternMatch2() { runner.runOneTest("assertPatternMatch2") }

  @Test def test_assertMultFormsFail() { runner.runOneTest("assertMultFormsFail") }
  @Test def test_assertMultFormsFail2() { runner.runOneTest("assertMultFormsFail2") }
  @Test def test_assertPatternAndExp() { runner.runOneTest("assertPatternAndExp") }
  @Test def test_assertPatternAndExp2() { runner.runOneTest("assertPatternAndExp2") }
  @Test def test_assertOnSimpleType() { runner.runOneTest("assertOnSimpleType") }
  @Test def test_assertPass2() { runner.runOneTest("assertPass2") }
  @Test def test_assertPatternEmpty() { runner.runOneTest("assertPatternEmpty") }

  // DFDL-474
  //  @Test def test_assertExpressionEmpty() { runner.runOneTest("assertExpressionEmpty") }

  @Test def test_assertExpressionRef() { runner.runOneTest("assertExpressionRef") }
  @Test def test_assertExpressionRefFail() { runner.runOneTest("assertExpressionRefFail") }
  @Test def test_assertMessage() { runner.runOneTest("assertMessage") }
  @Test def test_unparseAssertionIgnored() { runner.runOneTest("unparseAssertionIgnored") }

  @Test def test_testPatternX() { runner.runOneTest("testPatternX") }
  @Test def test_testPatternHex() { runner.runOneTest("testPatternHex") }
  @Test def test_testPatternFreeFormat() { runner.runOneTest("testPatternFreeFormat") }
  @Test def test_testPatternUnicode() { runner.runOneTest("testPatternUnicode") }
  @Test def test_testPatternUregexUword() { runner.runOneTest("testPatternUregexUword") }
  @Test def test_testPatternWordChar() { runner.runOneTest("testPatternWordChar") }

  // JIRA DFDL-1672
  @Test def testNumberFormatErrorInExprRuntime() { runner.runOneTest("testNumberFormatErrorInExprRuntime") }
  @Test def testNumberFormatErrorInExprCompileTime() { runner.runOneTest("testNumberFormatErrorInExprCompileTime") }
}
