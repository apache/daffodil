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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Ignore
import org.junit.Test

object TestAssertions extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section07/assertions/assert.tdml"
}

class TestAssertions extends TdmlTests {
  val tdmlSuite = TestAssertions

  @Test def assertPass = test
  @Test def assertFail1 = test
  @Test def assertFail2 = test
  @Test def assertFail2_recoverable = test

  // DAFFODIL-752
  @Ignore @Test def assertFailShowsValue = test

  @Test def assertFailShowsValue2 = test
  @Test def assertFailShowsDetails = test
  @Test def assertWithWhitespace = test
  @Test def assertWithWhitespaceAndCdata = test

  @Test def assertGuidesChoice = test

  @Test def assertPatternLiteralTextMatch = test
  @Test def assertPatternCombinedTextMatch = test
  @Test def assertPatternCombinedTextMatch2 = test
  @Test def assertPatternCombinedTextMatch3 = test

  @Test def assertPatternPass = test
  @Test def assertPatternFail = test
  @Test def assertPatternPass2 = test
  @Test def assertPatternPass3 = test
  @Test def assertPatternFail2 = test
  @Test def assertPatternFail2_recoverable = test
  @Test def assertPatternInitsTerms = test
  @Test def assertOnSequence = test

  @Test def assertOnGroupRef = test
  @Test def assertOnElemRef = test

  @Test def assertPatternMatch = test
  @Test def assertPatternMatch2 = test

  @Test def assertMultFormsFail = test
  @Test def assertMultFormsFail2 = test
  @Test def assertPatternAndExp = test
  @Test def assertPatternAndExp2 = test
  @Test def assertOnSimpleType = test
  @Test def assertPass2 = test
  @Test def assertPatternEmpty = test

  // DFDL-474
  @Ignore @Test def assertExpressionEmpty = test

  @Test def assertExpressionRef = test
  @Test def assertExpressionRefFail = test
  @Test def assertMessage = test
  @Test def unparseAssertionIgnored = test

  // DFDL-2001
  @Ignore @Test def testPatternX = test
  @Ignore @Test def testPatternUnicode = test
  @Test def testPatternHex = test
  @Test def testPatternFreeFormat = test
  @Test def testPatternUregexUword = test
  @Test def testPatternWordChar = test

  // JIRA DFDL-1672
  @Test def testNumberFormatErrorInExprRuntime = test
  @Test def testNumberFormatErrorInExprCompileTime = test

  @Test def test_assertWithMessageExpression_01 = test
  @Test def test_assertWithMessageExpression_02 = test
}
