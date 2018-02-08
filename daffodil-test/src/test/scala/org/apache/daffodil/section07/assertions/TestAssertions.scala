/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.section07.assertions

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestAssertions {
  val testDir = "/edu/illinois/ncsa/daffodil/section07/assertions/"
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
