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

package edu.illinois.ncsa.daffodil.section10.representation_properties

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestRepProps2 {
  val testDir = "/edu/illinois/ncsa/daffodil/section10/representation_properties/"
  val runner = Runner(testDir, "encodings.tdml")

  @AfterClass def shutDown {
    runner.reset
  }
}

class TestRepProps2 {
  import TestRepProps2._

  @Test def test_ebcdic1() = { runner.runOneTest("ebcdic1") }
  @Test def test_bits1() = { runner.runOneTest("bits1") }
  @Test def test_bits1a() = { runner.runOneTest("bits1a") }
  @Test def test_bits2() = { runner.runOneTest("bits2") }
  @Test def test_bits2a() = { runner.runOneTest("bits2a") }

  @Test def test_bitsTerm1() = { runner.runOneTest("bitsTerm1") }

  // fails Left-over data byte 1 limit(bytes) 2
  @Test def test_bitsTerm2() = { runner.runOneTest("bitsTerm2") }
  @Test def test_bitsTerm3() = { runner.runOneTest("bitsTerm3") }

  @Test def test_sixBit1() = { runner.runOneTest("sixBit1") }

}
