/* Copyright (c) 2012-2017 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.section15.choice_groups

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestChoiceNew {
  val testDir = "/edu/illinois/ncsa/daffodil/section15/choice_groups/"

  val runnerCH = Runner(testDir, "choice.tdml")

  @AfterClass def shutDown {
    runnerCH.reset
  }
}

class TestChoiceNew {

  import TestChoiceNew._

  // DFDL-641
  @Test def test_direct_dispatch_01() { runnerCH.runOneTest("direct_dispatch_01") }
  @Test def test_direct_dispatch_02() { runnerCH.runOneTest("direct_dispatch_02") }
  @Test def test_direct_dispatch_03() { runnerCH.runOneTest("direct_dispatch_03") }
  @Test def test_direct_dispatch_04() { runnerCH.runOneTest("direct_dispatch_04") }
  @Test def test_direct_dispatch_05() { runnerCH.runOneTest("direct_dispatch_05") }
  @Test def test_direct_dispatch_06() { runnerCH.runOneTest("direct_dispatch_06") }
  @Test def test_direct_dispatch_07() { runnerCH.runOneTest("direct_dispatch_07") }
  @Test def test_direct_dispatch_08() { runnerCH.runOneTest("direct_dispatch_08") }
  @Test def test_direct_dispatch_09() { runnerCH.runOneTest("direct_dispatch_09") }
  @Test def test_direct_dispatch_10() { runnerCH.runOneTest("direct_dispatch_10") }
  @Test def test_direct_dispatch_11() { runnerCH.runOneTest("direct_dispatch_11") }
  @Test def test_direct_dispatch_12() { runnerCH.runOneTest("direct_dispatch_12") }
  @Test def test_direct_dispatch_13() { runnerCH.runOneTest("direct_dispatch_13") }
  @Test def test_direct_dispatch_14() { runnerCH.runOneTest("direct_dispatch_14") }
  @Test def test_direct_dispatch_15() { runnerCH.runOneTest("direct_dispatch_15") }
  @Test def test_direct_dispatch_16() { runnerCH.runOneTest("direct_dispatch_16") }
}
