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

object TestChoice2 {
  val testDir = "/edu/illinois/ncsa/daffodil/section15/choice_groups/"

  val runner = Runner(testDir, "choice1765.tdml")
  val runner1773 = Runner(testDir, "choice1773.tdml")

  @AfterClass def shutDown {
    runner.reset
    runner1773.reset
  }
}

class TestChoice2 {

  import TestChoice2._

  // DFDL-1765
  @Test def test_backtrack1(): Unit = { runner.runOneTest("backtrack1") }
  @Test def test_backtrack2(): Unit = { runner.runOneTest("backtrack2") }
  @Test def test_backtrack3(): Unit = { runner.runOneTest("backtrack2") }
  @Test def test_backtrack4(): Unit = { runner.runOneTest("backtrack4") }

  // DFDL-1773
  @Test def test_choiceSlotAmbiguous1(): Unit = { runner1773.runOneTest("choiceSlotAmbiguous1") }
  @Test def test_choiceSlotAmbiguous2(): Unit = { runner1773.runOneTest("choiceSlotAmbiguous2") }

  // DAFFODIL-1773
  @Test def test_queryStyle1(): Unit = { runner1773.runOneTest("queryStyle1") }
  @Test def test_queryStyle2(): Unit = { runner1773.trace.runOneTest("queryStyle2") }
}
