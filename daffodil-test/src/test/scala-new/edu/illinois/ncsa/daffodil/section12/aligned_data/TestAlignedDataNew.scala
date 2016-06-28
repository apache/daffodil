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

package edu.illinois.ncsa.daffodil.section12.aligned_data

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestAlignedDataNew {
  val testDir_01 = "/edu/illinois/ncsa/daffodil/section12/aligned_data/"
  val runner1 = Runner(testDir_01, "Aligned_Data.tdml")
  val runner2 = Runner(testDir_01, "BinaryInput_01.tdml")

  @AfterClass def shutDown {
    runner1.reset
    runner2.reset
  }
}

class TestAlignedDataNew {
  import TestAlignedDataNew._

  // DFDL-930
  @Test def test_alignmentStringBitSkip() = { runner1.runOneTest("alignmentStringBitSkip") }

  // See DFDL-929
  @Test def test_alignmentTerminatorBitSkip() = { runner1.runOneTest("alignmentTerminatorBitSkip") }

  @Test def test_fillByte_01() = { runner1.runOneTest("fillByte_01") }
  @Test def test_fillByte_02() = { runner1.runOneTest("fillByte_02") }
  @Test def test_fillByte_03() = { runner1.runOneTest("fillByte_03") }
  @Test def test_fillByte_04() = { runner1.runOneTest("fillByte_04") }
  @Test def test_fillByte_05() = { runner1.runOneTest("fillByte_05") }
  @Test def test_fillByte_06() = { runner1.runOneTest("fillByte_06") }
}
