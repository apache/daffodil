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

package edu.illinois.ncsa.daffodil.section17.calc_value_properties

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestInputValueCalc {
  val testDir = "/edu/illinois/ncsa/daffodil/section17/calc_value_properties/"

  val runner = Runner(testDir, "inputValueCalc.tdml")
  val runnerAR = Runner(testDir, "AR.tdml")
  val runnerAQ = Runner(testDir, "AQ.tdml")
  val runnerAA = Runner(testDir, "AA.tdml")

  @AfterClass def shutDown {
    runner.reset
    runnerAR.reset
    runnerAQ.reset
    runnerAA.reset
  }

}

class TestInputValueCalc {

  import TestInputValueCalc._

  @Test def test_AR000() { runnerAR.runOneTest("AR000") }

  @Test def test_AQ000() { runnerAQ.runOneTest("AQ000") }

  @Test def test_AA000() { runnerAA.runOneTest("AA000") }
  @Test def test_inputValueCalcErrorDiagnostic1() { runnerAA.runOneTest("inputValueCalcErrorDiagnostic1") }
  @Test def test_inputValueCalcErrorDiagnostic2() { runnerAA.runOneTest("inputValueCalcErrorDiagnostic2") }
  @Test def test_inputValueCalcAbsolutePath() { runnerAA.runOneTest("inputValueCalcAbsolutePath") }

  @Test def test_InputValueCalc_01() { runner.runOneTest("InputValueCalc_01") }
  @Test def test_InputValueCalc_02() { runner.runOneTest("InputValueCalc_02") }
  @Test def test_InputValueCalc_03() { runner.runOneTest("InputValueCalc_03") }
  @Test def test_InputValueCalc_04() { runner.runOneTest("InputValueCalc_04") }
  @Test def test_InputValueCalc_05() { runner.runOneTest("InputValueCalc_05") }
  @Test def test_InputValueCalc_06() { runner.runOneTest("InputValueCalc_06") }
  @Test def test_InputValueCalc_08() { runner.runOneTest("InputValueCalc_08") }
  @Test def test_InputValueCalc_09() { runner.runOneTest("InputValueCalc_09") }
  @Test def test_InputValueCalc_10() { runner.runOneTest("InputValueCalc_10") }
  @Test def test_InputValueCalc_11() { runner.runOneTest("InputValueCalc_11") }

  @Test def test_InputValueCalc_Int_Range_Min_Pass() { runner.runOneTest("InputValueCalc_Int_Range_Min_Pass") }
  @Test def test_InputValueCalc_Int_Range_Max_Pass() { runner.runOneTest("InputValueCalc_Int_Range_Max_Pass") }

  @Test def test_InputValueCalc_Short_Range_Min_Pass() { runner.runOneTest("InputValueCalc_Short_Range_Min_Pass") }
  @Test def test_InputValueCalc_Short_Range_Max_Pass() { runner.runOneTest("InputValueCalc_Short_Range_Max_Pass") }

  @Test def test_InputValueCalc_UnsignedInt_Range_Max_Pass() { runner.runOneTest("InputValueCalc_UnsignedInt_Range_Max_Pass") }

  @Test def test_InputValueCalc_UnsignedShort_Range_Max_Pass() { runner.runOneTest("InputValueCalc_UnsignedShort_Range_Max_Pass") }

  @Test def test_InputValueCalc_Long_Range_Min_Pass() { runner.runOneTest("InputValueCalc_Long_Range_Min_Pass") }
  @Test def test_InputValueCalc_Long_Range_Max_Pass() { runner.runOneTest("InputValueCalc_Long_Range_Max_Pass") }

  @Test def test_InputValueCalc_Byte_Range_Min_Pass() { runner.runOneTest("InputValueCalc_Byte_Range_Min_Pass") }
  @Test def test_InputValueCalc_Byte_Range_Max_Pass() { runner.runOneTest("InputValueCalc_Byte_Range_Max_Pass") }

  @Test def test_InputValueCalc_UnsignedByte_Range_Max_Pass() { runner.runOneTest("InputValueCalc_UnsignedByte_Range_Max_Pass") }

  @Test def test_InputValueCalc_UnsignedLong_Range_Max_Pass() { runner.runOneTest("InputValueCalc_UnsignedLong_Range_Max_Pass") }

  // NOTE: These tests were modified to test both the CompileTime and RunTime side of expressions
  // when dealing with InputValueCalc.  Essentially, constant expressions are evaluated at compile time.
  // Non constant expressions are evaluated at run time.
  // Constant: inputValueCalc="{ 3 }"
  // Run Time: inputValueCalc="{ ../ex:num }"
  //
  @Test def test_InputValueCalc_Short_Range_Min_Fail_CompileTime() { runner.runOneTest("InputValueCalc_Short_Range_Min_Fail_CompileTime") }
  @Test def test_InputValueCalc_Short_Range_Min_Fail_RunTime() { runner.runOneTest("InputValueCalc_Short_Range_Min_Fail_RunTime") }
  @Test def test_InputValueCalc_Short_Range_Max_Fail_CompileTime() { runner.runOneTest("InputValueCalc_Short_Range_Max_Fail_CompileTime") }
  @Test def test_InputValueCalc_Short_Range_Max_Fail_RunTime() { runner.runOneTest("InputValueCalc_Short_Range_Max_Fail_RunTime") }
  @Test def test_InputValueCalc_UnsignedInt_Range_Min_Fail_CompileTime() { runner.runOneTest("InputValueCalc_UnsignedInt_Range_Min_Fail_CompileTime") }
  @Test def test_InputValueCalc_UnsignedInt_Range_Min_Fail_RunTime() { runner.runOneTest("InputValueCalc_UnsignedInt_Range_Min_Fail_RunTime") }
  @Test def test_InputValueCalc_UnsignedInt_Range_Max_Fail_CompileTime() { runner.runOneTest("InputValueCalc_UnsignedInt_Range_Max_Fail_CompileTime") }
  @Test def test_InputValueCalc_UnsignedInt_Range_Max_Fail_RunTime() { runner.runOneTest("InputValueCalc_UnsignedInt_Range_Max_Fail_RunTime") }
  @Test def test_InputValueCalc_UnsignedShort_Range_Min_Fail_CompileTime() { runner.runOneTest("InputValueCalc_UnsignedShort_Range_Min_Fail_CompileTime") }
  @Test def test_InputValueCalc_UnsignedShort_Range_Min_Fail_RunTime() { runner.runOneTest("InputValueCalc_UnsignedShort_Range_Min_Fail_RunTime") }
  @Test def test_InputValueCalc_UnsignedShort_Range_Max_Fail_CompileTime() { runner.runOneTest("InputValueCalc_UnsignedShort_Range_Max_Fail_CompileTime") }
  @Test def test_InputValueCalc_UnsignedShort_Range_Max_Fail_RunTime() { runner.runOneTest("InputValueCalc_UnsignedShort_Range_Max_Fail_RunTime") }
  @Test def test_InputValueCalc_Long_Range_Min_Fail_CompileTime() { runner.runOneTest("InputValueCalc_Long_Range_Min_Fail_CompileTime") }
  @Test def test_InputValueCalc_Long_Range_Min_Fail_RunTime() { runner.runOneTest("InputValueCalc_Long_Range_Min_Fail_RunTime") }
  @Test def test_InputValueCalc_Long_Range_Max_Fail_CompileTime() { runner.runOneTest("InputValueCalc_Long_Range_Max_Fail_CompileTime") }
  @Test def test_InputValueCalc_Long_Range_Max_Fail_RunTime() { runner.runOneTest("InputValueCalc_Long_Range_Max_Fail_RunTime") }
  @Test def test_InputValueCalc_UnsignedLong_Range_Min_Fail_CompileTime() { runner.runOneTest("InputValueCalc_UnsignedLong_Range_Min_Fail_CompileTime") }
  @Test def test_InputValueCalc_UnsignedLong_Range_Min_Fail_RunTime() { runner.runOneTest("InputValueCalc_UnsignedLong_Range_Min_Fail_RunTime") }
  @Test def test_InputValueCalc_UnsignedLong_Range_Max_Fail_CompileTime() { runner.runOneTest("InputValueCalc_UnsignedLong_Range_Max_Fail_CompileTime") }
  @Test def test_InputValueCalc_UnsignedLong_Range_Max_Fail_RunTime() { runner.runOneTest("InputValueCalc_UnsignedLong_Range_Max_Fail_RunTime") }
  @Test def test_InputValueCalc_Byte_Range_Min_Fail_CompileTime() { runner.runOneTest("InputValueCalc_Byte_Range_Min_Fail_CompileTime") }
  @Test def test_InputValueCalc_Byte_Range_Min_Fail_RunTime() { runner.runOneTest("InputValueCalc_Byte_Range_Min_Fail_RunTime") }
  @Test def test_InputValueCalc_Byte_Range_Max_Fail_CompileTime() { runner.runOneTest("InputValueCalc_Byte_Range_Max_Fail_CompileTime") }
  @Test def test_InputValueCalc_Byte_Range_Max_Fail_RunTime() { runner.runOneTest("InputValueCalc_Byte_Range_Max_Fail_RunTime") }
  @Test def test_InputValueCalc_UnsignedByte_Range_Min_Fail_CompileTime() { runner.runOneTest("InputValueCalc_UnsignedByte_Range_Min_Fail_CompileTime") }
  @Test def test_InputValueCalc_UnsignedByte_Range_Min_Fail_RunTime() { runner.runOneTest("InputValueCalc_UnsignedByte_Range_Min_Fail_RunTime") }
  @Test def test_InputValueCalc_UnsignedByte_Range_Max_Fail_CompileTime() { runner.runOneTest("InputValueCalc_UnsignedByte_Range_Max_Fail_CompileTime") }
  @Test def test_InputValueCalc_UnsignedByte_Range_Max_Fail_RunTime() { runner.runOneTest("InputValueCalc_UnsignedByte_Range_Max_Fail_RunTime") }
  @Test def test_InputValueCalc_Int_Range_Min_Fail_CompileTime() { runner.runOneTest("InputValueCalc_Int_Range_Min_Fail_CompileTime") }
  @Test def test_InputValueCalc_Int_Range_Min_Fail_RunTime() { runner.runOneTest("InputValueCalc_Int_Range_Min_Fail_RunTime") }
  @Test def test_InputValueCalc_Int_Range_Max_Fail_CompileTime() { runner.runOneTest("InputValueCalc_Int_Range_Max_Fail_CompileTime") }
  @Test def test_InputValueCalc_Int_Range_Max_Fail_RunTime() { runner.runOneTest("InputValueCalc_Int_Range_Max_Fail_RunTime") }
  @Test def test_InputValueCalc_nonmatching_types() { runner.runOneTest("InputValueCalc_nonmatching_types") }
  @Test def test_InputValueCalc_no_representation() { runner.runOneTest("InputValueCalc_no_representation") }

}
