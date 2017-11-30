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

package edu.illinois.ncsa.daffodil.section16.array_optional_elem

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestUnparseArrayFixedOptionalElemNew {
  val testDir = "/edu/illinois/ncsa/daffodil/section16/array_optional_elem/"

  val runner_fixed = Runner(testDir, "UnparseArrayFixedOptionalElem.tdml")

  val runner_imp = Runner(testDir, "UnparseArrayImplicitOptionalElem.tdml")

  val runner_parsed = Runner(testDir, "UnparseArrayParsedOptionalElem.tdml")

  val runner_expr = Runner(testDir, "UnparseArrayExpressionConstant.tdml")

  /**
   * Avoid memory leak of adding more and more test suites to static objects as we run more and more test suites.
   */
  @AfterClass def tearDown() {
    runner_fixed.reset
    runner_imp.reset
    runner_parsed.reset
    runner_expr.reset
  }
}
class TestUnparseArrayFixedOptionalElemNew {
  import TestUnparseArrayFixedOptionalElemNew._

  //DFDL-1301
  @Test def test_fixedUnparseArrayTooManyElements01() { runner_fixed.runOneTest("fixedUnparseArrayTooManyElements01") }

  @Test def test_impOptArrayThenScalar02parse() { runner_imp.runOneTest("impOptArrayThenScalar02parse") }
}
