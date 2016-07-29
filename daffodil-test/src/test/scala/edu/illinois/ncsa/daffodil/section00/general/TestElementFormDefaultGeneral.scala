/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.section00.general

import org.junit.Test
import org.junit._
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestElementFormDefaultGeneral {
  
  val testDir = "/edu/illinois/ncsa/daffodil/section00/general/"
  val runner = Runner(testDir, "testElementFormDefault.tdml")

  @AfterClass def shutDown() { 
    runner.reset
  }

}

class TestElementFormDefaultGeneral {
  import TestElementFormDefaultGeneral._
 
  @Test def test_delimOptPresentQualified01() { runner.runOneTest("delimOptPresentQualified01") }
  @Test def test_delimOptPresentQualified02() { runner.runOneTest("delimOptPresentQualified02") }
  @Test def test_delimOptPresentQualified03() { runner.runOneTest("delimOptPresentQualified03") }
  @Test def test_delimOptPresentQualified04() { runner.runOneTest("delimOptPresentQualified04") }
  @Test def test_delimOptPresentQualified05() { runner.runOneTest("delimOptPresentQualified05") }

  @Test def test_delimOptPresentUnqualified01() { runner.runOneTest("delimOptPresentUnqualified01") }
  @Test def test_delimOptPresentUnqualified02() { runner.runOneTest("delimOptPresentUnqualified02") }
  @Test def test_delimOptPresentUnqualified03() { runner.runOneTest("delimOptPresentUnqualified03") }
  @Test def test_delimOptPresentUnqualified04() { runner.runOneTest("delimOptPresentUnqualified04") }
  @Test def test_delimOptPresentMissing() { runner.runOneTest("delimOptPresentMissing") }

  @Test def test_delimOptPresentGlobalQualified01() { runner.runOneTest("delimOptPresentGlobalQualified01") }
  @Test def test_delimOptPresentGlobalQualified02() { runner.runOneTest("delimOptPresentGlobalQualified02") }

}
