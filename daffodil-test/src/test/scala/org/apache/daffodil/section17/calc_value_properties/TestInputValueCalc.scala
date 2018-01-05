/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.section17.calc_value_properties

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestInputValueCalc {
  val testDir = "/org/apache/daffodil/section17/calc_value_properties/"

  val runner = Runner(testDir, "inputValueCalc.tdml", validateTDMLFile = false)
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
  @Test def test_InputValueCalc_in_format() { runner.runOneTest("InputValueCalc_in_format") }
  @Test def test_InputValueCalc_with_outputValueCalc() { runner.runOneTest("InputValueCalc_with_outputValueCalc") }
}
