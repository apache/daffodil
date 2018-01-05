/* Copyright (c) 2015 Tresys Technology, LLC. All rights reserved.
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
import org.junit.AfterClass
import edu.illinois.ncsa.daffodil.tdml.Runner

object TestUnparserGeneral {
  val testDir = "/edu/illinois/ncsa/daffodil/section00/general/"
  val runner = Runner(testDir, "testUnparserGeneral.tdml")

  @AfterClass def shutDown {
    runner.reset
  }
}
class TestUnparserGeneral {

  import TestUnparserGeneral._

  //DFDL-1420
  //@Test def test_apostrophe_01() { runner.runOneTest("apostrophe_01") }

  //DFDL-1395
  //@Test def test_puaInfosetChars_03() { runner.runOneTest("puaInfosetChars_03") }
  //@Test def test_puaInfosetChars_04() { runner.runOneTest("puaInfosetChars_04") }

  @Test def test_puaInfosetChars_01() { runner.runOneTest("puaInfosetChars_01") }
  @Test def test_puaInfosetChars_02() { runner.runOneTest("puaInfosetChars_02") }

  @Test def test_unparseFixedLengthString01() { runner.runOneTest("unparseFixedLengthString01") }
  @Test def test_unparseFixedLengthString02() { runner.runOneTest("unparseFixedLengthString02") }
  @Test def test_unparseFixedLengthString03() { runner.runOneTest("unparseFixedLengthString03") }

  @Test def test_parseFixedLengthString01() { runner.runOneTest("parseFixedLengthString01") }
  @Test def test_parseFixedLengthStringLength0() { runner.runOneTest("parseFixedLengthStringLength0") }

  @Test def test_negativeUnparseTest01() { runner.runOneTest("negativeUnparseTest01") }
  @Test def test_negativeUnparseTest02() { runner.runOneTest("negativeUnparseTest02") }
  @Test def test_negativeUnparseTest03() { runner.runOneTest("negativeUnparseTest03") }
  @Test def test_negativeUnparseTest04() { runner.runOneTest("negativeUnparseTest04") }
  @Test def test_negativeUnparseTest05() { runner.runOneTest("negativeUnparseTest05") }

  @Test def test_unparseDelimitedString01() { runner.runOneTest("unparseDelimitedString01") }
  @Test def test_unparseDelimitedString02() { runner.runOneTest("unparseDelimitedString02") }
  @Test def test_unparseDelimitedString03() { runner.runOneTest("unparseDelimitedString03") }
  @Test def test_unparseDelimitedString04() { runner.runOneTest("unparseDelimitedString04") }
  @Test def test_unparseDelimitedString05() { runner.runOneTest("unparseDelimitedString05") }
  @Test def test_unparseDelimitedString06() { runner.runOneTest("unparseDelimitedString06") }
  @Test def test_unparseDelimitedString07() { runner.runOneTest("unparseDelimitedString07") }

  @Test def test_parseDelimitedString01() { runner.runOneTest("parseDelimitedString01") }

  // DFDL-1650
  @Test def test_alignmentPaddingOVC1() { runner.runOneTest("alignmentPaddingOVC1") }
  @Test def test_alignmentPaddingOVC2() { runner.runOneTest("alignmentPaddingOVC2") }
  @Test def test_alignmentPaddingOVC3() { runner.runOneTest("alignmentPaddingOVC3") }

}
