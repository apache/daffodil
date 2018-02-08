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

package org.apache.daffodil.section13.text_number_props

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner

object TestTextNumberPropsUnparse {
  val testDir = "/org/apache/daffodil/section13/text_number_props/"

  val runner = Runner(testDir, "TextNumberPropsUnparse.tdml")

  @AfterClass def shutDown {
    runner.reset
  }
}
class TestTextNumberPropsUnparse {

  import TestTextNumberPropsUnparse._

  @Test def test_unparseDelimitedPaddedString01() { runner.runOneTest("unparseDelimitedPaddedString01") }
  @Test def test_unparseDelimitedPaddedString02() { runner.runOneTest("unparseDelimitedPaddedString02") }
  @Test def test_unparseDelimitedPaddedString03() { runner.runOneTest("unparseDelimitedPaddedString03") }
  @Test def test_unparseDelimitedPaddedString04() { runner.runOneTest("unparseDelimitedPaddedString04") }
  @Test def test_unparseDelimitedPaddedString05() { runner.runOneTest("unparseDelimitedPaddedString05") }
  @Test def test_unparseDelimitedPaddedString06() { runner.runOneTest("unparseDelimitedPaddedString06") }
  @Test def test_unparseDelimitedPaddedString07() { runner.runOneTest("unparseDelimitedPaddedString07") }
  @Test def test_unparseDelimitedPaddedString08() { runner.runOneTest("unparseDelimitedPaddedString08") }
  @Test def test_unparseDelimitedPaddedString09() { runner.runOneTest("unparseDelimitedPaddedString09") }
  @Test def test_unparseDelimitedPaddedString11() { runner.runOneTest("unparseDelimitedPaddedString11") }
  @Test def test_unparseDelimitedPaddedString12() { runner.runOneTest("unparseDelimitedPaddedString12") }
  @Test def test_unparseDelimitedPaddedString13() { runner.runOneTest("unparseDelimitedPaddedString13") }
  @Test def test_unparseDelimitedPaddedString14() { runner.runOneTest("unparseDelimitedPaddedString14") }

  @Test def test_unparsePaddedString10() { runner.runOneTest("unparsePaddedString10") }

  @Test def test_unparsePaddedStringTruncate01() { runner.runOneTest("unparsePaddedStringTruncate01") }
  @Test def test_unparsePaddedStringTruncate02() { runner.runOneTest("unparsePaddedStringTruncate02") }
  @Test def test_unparsePaddedStringTruncate03() { runner.runOneTest("unparsePaddedStringTruncate03") }

  @Test def test_parseDelimitedPaddedString01() { runner.runOneTest("parseDelimitedPaddedString01") }

  @Test def test_unparse_int_01() { runner.runOneTest("unparse_int_01") }
  @Test def test_parse_int_01() { runner.runOneTest("parse_int_01") }

  @Test def test_unparse_tnp_01() { runner.runOneTest("unparse_tnp_01") }
  @Test def test_unparse_tnp_02() { runner.runOneTest("unparse_tnp_02") }
  @Test def test_unparse_tnp_03() { runner.runOneTest("unparse_tnp_03") }
  @Test def test_unparse_tnp_04() { runner.runOneTest("unparse_tnp_04") }
}
