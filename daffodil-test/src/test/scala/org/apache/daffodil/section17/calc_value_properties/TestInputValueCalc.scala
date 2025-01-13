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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Ignore
import org.junit.Test

object TestInputValueCalc extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section17/calc_value_properties/inputValueCalc.tdml"
  override def createRunner() = Runner(tdmlDir, tdmlFile, validateTDMLFile = false)
}

object TestAR extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section17/calc_value_properties/AR.tdml"
}

object TestAQ extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section17/calc_value_properties/AQ.tdml"
}

object TestAA extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section17/calc_value_properties/AA.tdml"
}

class TestInputValueCalc extends TdmlTests {
  val tdmlSuite = TestInputValueCalc

  @Test def InputValueCalc_01 = test
  @Test def InputValueCalc_02 = test
  @Test def InputValueCalc_03 = test
  @Test def InputValueCalc_04 = test
  @Test def InputValueCalc_05 = test
  @Test def InputValueCalc_06 = test
  @Test def InputValueCalc_08 = test
  @Test def InputValueCalc_09 = test
  @Test def InputValueCalc_10 = test
  @Test def InputValueCalc_11 = test

  @Test def InputValueCalc_Int_Range_Min_Pass = test
  @Test def InputValueCalc_Int_Range_Max_Pass = test

  @Test def InputValueCalc_Short_Range_Min_Pass = test
  @Test def InputValueCalc_Short_Range_Max_Pass = test

  @Test def InputValueCalc_UnsignedInt_Range_Max_Pass = test

  @Test def InputValueCalc_UnsignedShort_Range_Max_Pass = test

  @Test def InputValueCalc_Long_Range_Min_Pass = test
  @Test def InputValueCalc_Long_Range_Max_Pass = test

  @Test def InputValueCalc_Byte_Range_Min_Pass = test
  @Test def InputValueCalc_Byte_Range_Max_Pass = test

  @Test def InputValueCalc_UnsignedByte_Range_Max_Pass = test

  @Test def InputValueCalc_UnsignedLong_Range_Max_Pass = test

  @Test def InputValueCalc_UnsignedLong_From_Double = test

  // NOTE: These tests were modified to test both the CompileTime and RunTime side of expressions
  // when dealing with InputValueCalc.  Essentially, constant expressions are evaluated at compile time.
  // Non constant expressions are evaluated at run time.
  // Constant: inputValueCalc="{ 3 }"
  // Run Time: inputValueCalc="{ ../ex:num }"
  @Test def InputValueCalc_Short_Range_Min_Fail_CompileTime = test
  @Test def InputValueCalc_Short_Range_Min_Fail_RunTime = test
  @Test def InputValueCalc_Short_Range_Max_Fail_CompileTime = test
  @Test def InputValueCalc_Short_Range_Max_Fail_RunTime = test
  @Test def InputValueCalc_UnsignedInt_Range_Min_Fail_CompileTime = test
  @Test def InputValueCalc_UnsignedInt_Range_Min_Fail_RunTime = test
  @Test def InputValueCalc_UnsignedInt_Range_Max_Fail_CompileTime = test
  @Test def InputValueCalc_UnsignedInt_Range_Max_Fail_RunTime = test
  @Test def InputValueCalc_UnsignedShort_Range_Min_Fail_CompileTime = test
  @Test def InputValueCalc_UnsignedShort_Range_Min_Fail_RunTime = test
  @Test def InputValueCalc_UnsignedShort_Range_Max_Fail_CompileTime = test
  @Test def InputValueCalc_UnsignedShort_Range_Max_Fail_RunTime = test
  @Test def InputValueCalc_Long_Range_Min_Fail_CompileTime = test
  @Test def InputValueCalc_Long_Range_Min_Fail_RunTime = test
  @Test def InputValueCalc_Long_Range_Max_Fail_CompileTime = test
  @Test def InputValueCalc_Long_Range_Max_Fail_RunTime = test
  @Test def InputValueCalc_UnsignedLong_Range_Min_Fail_CompileTime = test
  @Test def InputValueCalc_UnsignedLong_Range_Min_Fail_RunTime = test
  @Test def InputValueCalc_UnsignedLong_Range_Max_Fail_CompileTime = test
  @Test def InputValueCalc_UnsignedLong_Range_Max_Fail_RunTime = test
  @Test def InputValueCalc_Byte_Range_Min_Fail_CompileTime = test
  @Test def InputValueCalc_Byte_Range_Min_Fail_RunTime = test
  @Test def InputValueCalc_Byte_Range_Max_Fail_CompileTime = test
  @Test def InputValueCalc_Byte_Range_Max_Fail_RunTime = test
  @Test def InputValueCalc_UnsignedByte_Range_Min_Fail_CompileTime = test
  @Test def InputValueCalc_UnsignedByte_Range_Min_Fail_RunTime = test
  @Test def InputValueCalc_UnsignedByte_Range_Max_Fail_CompileTime = test
  @Test def InputValueCalc_UnsignedByte_Range_Max_Fail_RunTime = test
  @Test def InputValueCalc_Int_Range_Min_Fail_CompileTime = test
  @Test def InputValueCalc_Int_Range_Min_Fail_RunTime = test
  @Test def InputValueCalc_Int_Range_Max_Fail_CompileTime = test
  @Test def InputValueCalc_Int_Range_Max_Fail_RunTime = test
  @Test def InputValueCalc_nonmatching_types = test
  @Test def InputValueCalc_no_representation = test
  @Test def InputValueCalc_in_format = test
  @Test def InputValueCalc_with_outputValueCalc = test

  // DFDL-1025
  @Ignore @Test def InputValueCalc_refers_self = test
  @Ignore @Test def InputValueCalc_circular_ref = test

  // DFDL-1024
  @Test def InputValueCalc_optional_elem = test
  @Test def InputValueCalc_array_elem = test
  @Ignore @Test def InputValueCalc_global_elem = test

  // DFDL-2806
  @Test def IVC_ignored_length_checks = test
}

class TestAA extends TdmlTests {
  val tdmlSuite = TestAA

  @Test def AA000 = test
  @Test def inputValueCalcErrorDiagnostic1 = test
  @Test def inputValueCalcErrorDiagnostic2 = test
  @Test def inputValueCalcAbsolutePath = test
}

class TestAQ extends TdmlTests {
  val tdmlSuite = TestAQ

  @Test def AQ000 = test
}

class TestAR extends TdmlTests {
  val tdmlSuite = TestAR

  @Test def AR000 = test
}
