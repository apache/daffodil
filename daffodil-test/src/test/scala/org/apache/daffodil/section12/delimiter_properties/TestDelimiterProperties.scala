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

package edu.illinois.ncsa.daffodil.section12.delimiter_properties

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestDelimiterProperties {
  val testDir_01 = "/edu/illinois/ncsa/daffodil/ibm-tests/"
  val runner_01 = Runner(testDir_01, "dpaext1.tdml")

  val testDir_02 = "/edu/illinois/ncsa/daffodil/section12/delimiter_properties/"
  val runner_02 = Runner(testDir_02, "DelimiterProperties.tdml")

  @AfterClass def shutDown {
    runner_01.reset
    runner_02.reset
  }

}

class TestDelimiterProperties {

  import TestDelimiterProperties._

  @Test def test_delimiter_12_01() { runner_01.runOneTest("delimiter_12_01") }
  @Test def test_delimiter_12_02() { runner_01.runOneTest("delimiter_12_02") }
  @Test def test_delimiter_12_03() { runner_01.runOneTest("delimiter_12_03") }
  @Test def test_delimiter_12_04() { runner_01.runOneTest("delimiter_12_04") }

  @Test def test_DelimProp_01() = { runner_02.runOneTest("DelimProp_01") }
  @Test def test_ParseSequence4() { runner_02.runOneTest("ParseSequence4") }
  @Test def test_ParseSequence5() { runner_02.runOneTest("ParseSequence5") }
  //@Test def testParseSequence_4a() { runner_02.runOneTest("ParseSequence_4a") }
  @Test def test_DelimProp_02() { runner_02.runOneTest("DelimProp_02") }
  @Test def test_DelimProp_03() { runner_02.runOneTest("DelimProp_03") }
  @Test def test_DelimProp_04() { runner_02.runOneTest("DelimProp_04") }
  @Test def test_DelimProp_05() { runner_02.runOneTest("DelimProp_05") }
  @Test def test_DelimProp_06() { runner_02.runOneTest("DelimProp_06") }
  @Test def test_DelimProp_07() { runner_02.runOneTest("DelimProp_07") }
  @Test def test_initiatedContentSimple1() { runner_02.runOneTest("initiatedContentSimple1") }
  @Test def test_Lesson4_initiators_terminators() { runner_02.runOneTest("Lesson4_initiators_terminators") }

  @Test def test_DelimProp_10() = { runner_02.runOneTest("DelimProp_10") }
  @Test def test_DelimProp_10_01() = { runner_02.runOneTest("DelimProp_10_01") }

  @Test def test_E1() = { runner_02.runOneTest("E1") }

  @Test def test_ReqFieldMissingAndSepIsPrefixOfTerminator_Prefix() = {
    runner_02.runOneTest("ReqFieldMissingAndSepIsPrefixOfTerminator_Prefix")
  }
  @Test def test_ReqFieldMissingAndSepIsPrefixOfTerminator_Infix() = {
    runner_02.runOneTest("ReqFieldMissingAndSepIsPrefixOfTerminator_Infix")
  }
  @Test def test_ReqFieldMissingAndSepIsPrefixOfTerminator_Postfix() = {
    runner_02.runOneTest("ReqFieldMissingAndSepIsPrefixOfTerminator_Postfix")
  }

  @Test def test_OptionalWSPTermWithExplicitLength() = {
    runner_02.runOneTest("OptionalWSPTermWithExplicitLength")
  }
  @Test def test_OptionalWSPTermWithExplicitLength2() = {
    runner_02.runOneTest("OptionalWSPTermWithExplicitLength2")
  }

  @Test def test_delims_ignorecase_01() = { runner_02.runOneTest("delims_ignorecase_01") }
}
