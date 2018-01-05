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

package edu.illinois.ncsa.daffodil.section12.lengthKind

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestLengthKindDelimited {
  private val testDir = "/edu/illinois/ncsa/daffodil/section12/lengthKind/"

  val runner = Runner(testDir, "DelimitedTests.tdml")
  val runnerAB = Runner(testDir, "AB.tdml")
  val runnerAN = Runner(testDir, "AN.tdml")
  val runner_01 = Runner("/edu/illinois/ncsa/daffodil/ibm-tests/", "dpaext1.tdml")

  @AfterClass def shutDown {
    runner.reset
    runnerAB.reset
    runnerAN.reset
    runner_01.reset
  }

}

class TestLengthKindDelimited {

  import TestLengthKindDelimited._

  @Test def test_delimited_binary_int_seqSep() = { runner.runOneTest("delimited_binary_int_seqSep") }

  @Test def test_DoubleNewLineTerminator() = { runner.runOneTest("TestDoubleNewLineTerminator") }
  @Test def test_DoubleNewLineSeparator() = { runner.runOneTest("TestDoubleNewLineSeparator") }
  @Test def test_DoubleNewLineSeparatorBasic() = { runner.runOneTest("TestDoubleNewLineSeparatorBasic") }

  @Test def test_NumSeq_00a() = { runner.runOneTest("NumSeq_00a") }
  @Test def test_NumSeq_00nl() = { runner.runOneTest("NumSeq_00nl") }
  @Test def test_NumSeq_01() = { runner.runOneTest("NumSeq_01") }
  @Test def test_nested_NumSeq_01() = { runner.runOneTest("nested_NumSeq_01") }
  @Test def test_eofTest1() { runner.runOneTest("eofTest1") }
  @Test def test_NumSeq_03() { runner.runOneTest("NumSeq_03") }
  @Test def test_NumSeq_04() { runner.runOneTest("NumSeq_04") }
  @Test def test_NumSeq_05() { runner.runOneTest("NumSeq_05") }
  @Test def test_NumSeq_06() { runner.runOneTest("NumSeq_06") }
  @Test def test_NumSeq_07() { runner.runOneTest("NumSeq_07") }
  @Test def test_NumSeq_08() { runner.runOneTest("NumSeq_08") }
  @Test def test_NumSeq_09() { runner.runOneTest("NumSeq_09") }
  //  @Test def test_NumSeq_10() { runner.runOneTest("NumSeq_10") }
  @Test def test_lengthKindDelimited_01() { runner.runOneTest("lengthKindDelimited_01") }
  @Test def test_lengthKindDelimited_02() { runner.runOneTest("lengthKindDelimited_02") }
  @Test def test_lengthKindDelimited_03() { runner.runOneTest("lengthKindDelimited_03") }
  @Test def test_lengthKindDelimited_04() { runner.runOneTest("lengthKindDelimited_04") }
  @Test def test_NumSeq_11() { runner.runOneTest("NumSeq_11") }
  @Test def test_NumSeq_12() { runner.runOneTest("NumSeq_12") }
  @Test def test_NumSeq_13() { runner.runOneTest("NumSeq_13") }
  @Test def test_NumSeq_13Fail() { runner.runOneTest("NumSeq_13Fail") }
  @Test def test_NumSeq_14() { runner.runOneTest("NumSeq_14") }
  // Tests that initiator is found when on ElementRef
  @Test def test_refInitiator() { runner.runOneTest("refInitiator") }
  // Tests that initiator is found when on GlobalElmentDecl
  @Test def test_refInitiator2() { runner.runOneTest("refInitiator2") }
  @Test def test_binary_delimited_fail() { runner.runOneTest("binary_delimited_fail") }
  @Test def test_Lesson1_lengthKind_delimited() { runner.runOneTest("Lesson1_lengthKind_delimited") }
  @Test def test_Lesson4_delimited_fixed_length() { runner.runOneTest("Lesson4_delimited_fixed_length") }
  @Test def test_delimited_construct() { runner.runOneTest("delimited_construct") }

  @Test def test_AB000() { runnerAB.runOneTest("AB000") }
  @Test def test_AB001() { runnerAB.runOneTest("AB001") }
  @Test def test_AB002() { runnerAB.runOneTest("AB002") }
  @Test def test_AB003() { runnerAB.runOneTest("AB003") }
  @Test def test_AB004() { runnerAB.runOneTest("AB004") }
  @Test def test_AB005() { runnerAB.runOneTest("AB005") }

  @Test def test_AN000() { runnerAN.runOneTest("AN000") }
  @Test def test_AN001() { runnerAN.runOneTest("AN001") }

  @Test def test_introduction_1_02() { runner_01.runOneTest("introduction_1_02") }
  @Test def test_length_delimited_12_03() { runner_01.runOneTest("length_delimited_12_03") }
  @Test def test_length_delimited_12_02() { runner_01.runOneTest("length_delimited_12_02") }
  @Test def test_multiple_delimiters() { runner_01.runOneTest("multiple_delimiters") }

}
