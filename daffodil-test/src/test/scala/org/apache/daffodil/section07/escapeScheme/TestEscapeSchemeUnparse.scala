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

package org.apache.daffodil.section07.escapeScheme

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner

object TestEscapeSchemeUnparse {
  val testDir = "/org/apache/daffodil/section07/escapeScheme/"
  val runner = Runner(testDir, "escapeSchemeUnparse.tdml")

  @AfterClass def tearDown() {
    runner.reset
  }
}

class TestEscapeSchemeUnparse {

  import TestEscapeSchemeUnparse._

  @Test def test_unparseDelimitedEscapedString01() { runner.runOneTest("unparseDelimitedEscapedString01") }
  @Test def test_unparseDelimitedEscapedString02() { runner.runOneTest("unparseDelimitedEscapedString02") }
  @Test def test_unparseDelimitedEscapedString03() { runner.runOneTest("unparseDelimitedEscapedString03") }
  @Test def test_unparseDelimitedEscapedString04() { runner.runOneTest("unparseDelimitedEscapedString04") }
  @Test def test_unparseDelimitedEscapedString05() { runner.runOneTest("unparseDelimitedEscapedString05") }
  @Test def test_unparseDelimitedEscapedString06() { runner.runOneTest("unparseDelimitedEscapedString06") }
  @Test def test_unparseDelimitedEscapedString07() { runner.runOneTest("unparseDelimitedEscapedString07") }
  @Test def test_unparseDelimitedEscapedString08() { runner.runOneTest("unparseDelimitedEscapedString08") }
  @Test def test_unparseDelimitedEscapedString09() { runner.runOneTest("unparseDelimitedEscapedString09") }
  @Test def test_unparseDelimitedEscapedString10() { runner.runOneTest("unparseDelimitedEscapedString10") }
  @Test def test_unparseDelimitedEscapedString11() { runner.runOneTest("unparseDelimitedEscapedString11") }
  @Test def test_unparseDelimitedEscapedString12() { runner.runOneTest("unparseDelimitedEscapedString12") }
  @Test def test_unparseDelimitedEscapedString13() { runner.runOneTest("unparseDelimitedEscapedString13") }
  @Test def test_unparseDelimitedEscapedString14() { runner.runOneTest("unparseDelimitedEscapedString14") }

  /* 
   * The following tests demonstrate that for extraEscapedCharacters during Unparsing that:
   * 
   * 1. DFDL Character Class entities are not allowed
   * 2. DFDL raw byte entities are not allowed
   * 3. DFDL hex entities are allowed
   * 4. DFDL basic entities are allowed (like SP, VT, etc)
   * 5. DFDL decimal entities are allowed
   * 6. When an extra escaped character is not present, the text is not escaped.
   * */
  @Test def test_unparseDelimitedEscapedString15() { runner.runOneTest("unparseDelimitedEscapedString15") }
  @Test def test_unparseDelimitedEscapedString16() { runner.runOneTest("unparseDelimitedEscapedString16") }
  @Test def test_unparseDelimitedEscapedString17() { runner.runOneTest("unparseDelimitedEscapedString17") }
  @Test def test_unparseDelimitedEscapedString18() { runner.runOneTest("unparseDelimitedEscapedString18") }
  @Test def test_unparseDelimitedEscapedString19() { runner.runOneTest("unparseDelimitedEscapedString19") }
  @Test def test_unparseDelimitedEscapedString20() { runner.runOneTest("unparseDelimitedEscapedString20") }
  @Test def test_unparseDelimitedEscapedString21() { runner.runOneTest("unparseDelimitedEscapedString21") }

  @Test def test_parseDelimitedEscapedString01() { runner.runOneTest("parseDelimitedEscapedString01") }
  @Test def test_parseDelimitedEscapedString03() { runner.runOneTest("parseDelimitedEscapedString03") }
  @Test def test_parseDelimitedEscapedString04() { runner.runOneTest("parseDelimitedEscapedString04") }
  @Test def test_parseDelimitedEscapedString05() { runner.runOneTest("parseDelimitedEscapedString05") }

}
