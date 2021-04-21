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

  @AfterClass def tearDown(): Unit = {
    runner.reset
  }

}

class TestAssertions {

  import TestAssertions._

  @Test def test_assertPass(): Unit = { runner.runOneTest("assertPass") }
  @Test def test_assertFail1(): Unit = { runner.runOneTest("assertFail1") }
  @Test def test_assertFail2(): Unit = { runner.runOneTest("assertFail2") }
  @Test def test_assertFail2_recoverable(): Unit = { runner.runOneTest("assertFail2_recoverable") }

  // DAFFODIL-752
  //@Test def test_assertFailShowsValue() { runner.runOneTest("assertFailShowsValue") }

  @Test def test_assertFailShowsValue2(): Unit = { runner.runOneTest("assertFailShowsValue2") }
  @Test def test_assertFailShowsDetails(): Unit = { runner.runOneTest("assertFailShowsDetails") }
  @Test def test_assertWithWhitespace(): Unit = { runner.runOneTest("assertWithWhitespace") }
  @Test def test_assertWithWhitespaceAndCdata(): Unit = { runner.runOneTest("assertWithWhitespaceAndCdata") }

  @Test def test_assertGuidesChoice(): Unit = { runner.runOneTest("assertGuidesChoice") }

  @Test def test_assertPatternLiteralTextMatch() = { runner.runOneTest("assertPatternLiteralTextMatch") }
  @Test def test_assertPatternCombinedTextMatch() = { runner.runOneTest("assertPatternCombinedTextMatch") }
  @Test def test_assertPatternCombinedTextMatch2() = { runner.runOneTest("assertPatternCombinedTextMatch2") }
  @Test def test_assertPatternCombinedTextMatch3() = { runner.runOneTest("assertPatternCombinedTextMatch3") }

  @Test def test_assertPatternPass(): Unit = { runner.runOneTest("assertPatternPass") }
  @Test def test_assertPatternFail(): Unit = { runner.runOneTest("assertPatternFail") }
  @Test def test_assertPatternPass2(): Unit = { runner.runOneTest("assertPatternPass2") }
  @Test def test_assertPatternPass3(): Unit = { runner.runOneTest("assertPatternPass3") }
  @Test def test_assertPatternFail2(): Unit = { runner.runOneTest("assertPatternFail2") }
  @Test def test_assertPatternFail2_recoverable(): Unit = { runner.runOneTest("assertPatternFail2_recoverable") }
  @Test def test_assertPatternInitsTerms(): Unit = { runner.runOneTest("assertPatternInitsTerms") }
  @Test def test_assertOnSequence(): Unit = { runner.runOneTest("assertOnSequence") }

  @Test def test_assertOnGroupRef(): Unit = { runner.runOneTest("assertOnGroupRef") }
  @Test def test_assertOnElemRef(): Unit = { runner.runOneTest("assertOnElemRef") }

  @Test def test_assertPatternMatch(): Unit = { runner.runOneTest("assertPatternMatch") }
  @Test def test_assertPatternMatch2(): Unit = { runner.runOneTest("assertPatternMatch2") }

  @Test def test_assertMultFormsFail(): Unit = { runner.runOneTest("assertMultFormsFail") }
  @Test def test_assertMultFormsFail2(): Unit = { runner.runOneTest("assertMultFormsFail2") }
  @Test def test_assertPatternAndExp(): Unit = { runner.runOneTest("assertPatternAndExp") }
  @Test def test_assertPatternAndExp2(): Unit = { runner.runOneTest("assertPatternAndExp2") }
  @Test def test_assertOnSimpleType(): Unit = { runner.runOneTest("assertOnSimpleType") }
  @Test def test_assertPass2(): Unit = { runner.runOneTest("assertPass2") }
  @Test def test_assertPatternEmpty(): Unit = { runner.runOneTest("assertPatternEmpty") }

  // DFDL-474
  //  @Test def test_assertExpressionEmpty() { runner.runOneTest("assertExpressionEmpty") }

  @Test def test_assertExpressionRef(): Unit = { runner.runOneTest("assertExpressionRef") }
  @Test def test_assertExpressionRefFail(): Unit = { runner.runOneTest("assertExpressionRefFail") }
  @Test def test_assertMessage(): Unit = { runner.runOneTest("assertMessage") }
  @Test def test_unparseAssertionIgnored(): Unit = { runner.runOneTest("unparseAssertionIgnored") }

  // DFDL-2001
  //@Test def test_testPatternX() { runner.runOneTest("testPatternX") }
  //@Test def test_testPatternUnicode() { runner.runOneTest("testPatternUnicode") }
  @Test def test_testPatternHex(): Unit = { runner.runOneTest("testPatternHex") }
  @Test def test_testPatternFreeFormat(): Unit = { runner.runOneTest("testPatternFreeFormat") }
  @Test def test_testPatternUregexUword(): Unit = { runner.runOneTest("testPatternUregexUword") }
  @Test def test_testPatternWordChar(): Unit = { runner.runOneTest("testPatternWordChar") }

  // JIRA DFDL-1672
  @Test def testNumberFormatErrorInExprRuntime(): Unit = { runner.runOneTest("testNumberFormatErrorInExprRuntime") }
  @Test def testNumberFormatErrorInExprCompileTime(): Unit = { runner.runOneTest("testNumberFormatErrorInExprCompileTime") }


  @Test def test_assertWithMessageExpression_01(): Unit = { runner.runOneTest("test_assertWithMessageExpression_01") }
  @Test def test_assertWithMessageExpression_02(): Unit = { runner.runOneTest("test_assertWithMessageExpression_02") }

}
