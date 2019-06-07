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

package org.apache.daffodil.extensions

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestInputTypeValueCalc {
  val testDir = "/org/apache/daffodil/extensions/type_calc/"

  val runner = Runner(testDir, "inputTypeCalc.tdml", validateTDMLFile = false)
  val exprRunner = Runner(testDir, "inputTypeCalcExpression.tdml", validateTDMLFile = false)
  val fnRunner = Runner(testDir, "typeCalcFunctions.tdml", validateTDMLFile = false)
  val fnErrRunner = Runner(testDir, "typeCalcFunctionErrors.tdml", validateTDMLFile = false)

  @AfterClass def shutDown {
    runner.reset
  }

}

class TestInputTypeValueCalc {
  import TestInputTypeValueCalc._
  @Test def test_InputTypeCalc_keysetValue_00() { runner.runOneTest("InputTypeCalc_keysetValue_00") }
  @Test def test_InputTypeCalc_keysetValue_01() { runner.runOneTest("InputTypeCalc_keysetValue_01") }
  @Test def test_InputTypeCalc_keysetValue_02() { runner.runOneTest("InputTypeCalc_keysetValue_02") }

  @Test def test_InputTypeCalc_unparse_keysetValue_00() { runner.runOneTest("InputTypeCalc_unparse_keysetValue_00") }
  @Test def test_InputTypeCalc_unparse_keysetValue_01() { runner.runOneTest("InputTypeCalc_unparse_keysetValue_01") }
  @Test def test_InputTypeCalc_unparse_keysetValue_02() { runner.runOneTest("InputTypeCalc_unparse_keysetValue_02") }

  @Test def test_InputTypeCalc_choiceDispatchByType_01() { runner.runOneTest("InputTypeCalc_choiceDispatchByType_01") }
  @Test def test_InputTypeCalc_choiceDispatchByType_02() { runner.runOneTest("InputTypeCalc_choiceDispatchByType_02") }
  @Test def test_InputTypeCalc_choiceDispatchByType_03() { runner.runOneTest("InputTypeCalc_choiceDispatchByType_03") }

  @Test def test_InputTypeCalc_unparse_choiceDispatchByType_01() { runner.runOneTest("InputTypeCalc_unparse_choiceDispatchByType_01") }
  @Test def test_InputTypeCalc_unparse_choiceDispatchByType_02() { runner.runOneTest("InputTypeCalc_unparse_choiceDispatchByType_02") }

  @Test def test_InputTypeCalc_choiceBranchKeyByType_01() { runner.runOneTest("InputTypeCalc_choiceBranchKeyByType_01") }

  @Test def test_InputTypeCalc_unionOfKeysetValueCalcs_01() { runner.runOneTest("InputTypeCalc_unionOfKeysetValueCalcs_01") }
  @Test def test_InputTypeCalc_unparse_unionOfKeysetValueCalcs_01() { runner.runOneTest("InputTypeCalc_unparse_unionOfKeysetValueCalcs_01") }

  @Test def test_InputTypeCalc_unionOfKeysetValueCalcs_02() { runner.runOneTest("InputTypeCalc_unionOfKeysetValueCalcs_02") }
  @Test def test_InputTypeCalc_unparse_unionOfKeysetValueCalcs_02() { runner.runOneTest("InputTypeCalc_unparse_unionOfKeysetValueCalcs_02") }

  @Test def test_InputTypeCalc_expression_01() { exprRunner.runOneTest("InputTypeCalc_expression_01") }
  @Test def test_OutputTypeCalc_expression_01() { exprRunner.runOneTest("OutputTypeCalc_expression_01") }
  @Test def test_InputTypeCalc_expression_02() { exprRunner.runOneTest("InputTypeCalc_expression_02") }
  @Test def test_OutputTypeCalc_expression_02() { exprRunner.runOneTest("OutputTypeCalc_expression_02") }

  @Test def test_inputTypeCalcInt_01() { fnRunner.runOneTest("inputTypeCalcInt_01") }
  @Test def test_inputTypeCalcString_01() { fnRunner.runOneTest("inputTypeCalcString_01") }
  @Test def test_outputTypeCalcInt_01() { fnRunner.runOneTest("outputTypeCalcInt_01") }
  @Test def test_outputTypeCalcString_01() { fnRunner.runOneTest("outputTypeCalcString_01") }
  @Test def test_repTypeValueInt_01() { fnRunner.runOneTest("repTypeValueInt_01") }
  @Test def test_logicalTypeValueInt_01() { fnRunner.runOneTest("logicalTypeValueInt_01") }
  @Test def test_repTypeValueString_01() { fnRunner.runOneTest("repTypeValueString_01") }
  @Test def test_logicalTypeValueString_01() { fnRunner.runOneTest("logicalTypeValueString_01") }
  @Test def test_outputTypeCalcNextSiblingInt_01() { fnRunner.runOneTest("outputTypeCalcNextSiblingInt_01") }
  @Test def test_outputTypeCalcNextSiblingString_01() { fnRunner.runOneTest("outputTypeCalcNextSiblingString_01") }
  @Test def test_abstractIntToStringByKeyset_01() { fnRunner.runOneTest("abstractIntToStringByKeyset_01") }

  @Test def test_typeCalcDispatch_typeError_01() { fnErrRunner.runOneTest("typeCalcDispatch_typeError_01") }
  @Test def test_typeCalcDispatch_typeError_02() { fnErrRunner.runOneTest("typeCalcDispatch_typeError_02") }
  @Test def test_typeCalcDispatch_typeError_03() { fnErrRunner.runOneTest("typeCalcDispatch_typeError_03") }
  @Test def test_typeCalcDispatch_typeError_04() { fnErrRunner.runOneTest("typeCalcDispatch_typeError_04") }
  @Test def test_typeCalcDispatch_typeError_05() { fnErrRunner.runOneTest("typeCalcDispatch_typeError_05") }
  @Test def test_typeCalcDispatch_typeError_06() { fnErrRunner.runOneTest("typeCalcDispatch_typeError_06") }
  @Test def test_typeCalcDispatch_typeError_07() { fnErrRunner.runOneTest("typeCalcDispatch_typeError_07") }
  @Test def test_typeCalcDispatch_typeError_08() { fnErrRunner.runOneTest("typeCalcDispatch_typeError_08") }
  @Test def test_typeCalcDispatch_typeError_09() { fnErrRunner.runOneTest("typeCalcDispatch_typeError_09") }
  @Test def test_typeCalcDispatch_typeError_10() { fnErrRunner.runOneTest("typeCalcDispatch_typeError_10") }
  @Test def test_typeCalcDispatch_typeError_11() { fnErrRunner.runOneTest("typeCalcDispatch_typeError_11") }
  @Test def test_typeCalcDispatch_typeError_12() { fnErrRunner.runOneTest("typeCalcDispatch_typeError_12") }
  @Test def test_repTypeValue_bad_context_01() { fnErrRunner.runOneTest("repTypeValue_bad_context_01") }
  @Test def test_repTypeValue_bad_context_02() { fnErrRunner.runOneTest("repTypeValue_bad_context_02") }
  @Test def test_logicalTypeValue_bad_context_01() { fnErrRunner.runOneTest("logicalTypeValue_bad_context_01") }
  @Test def test_logicalTypeValue_bad_context_02() { fnErrRunner.runOneTest("logicalTypeValue_bad_context_02") }
  @Test def test_nonexistant_reptype_01() { fnErrRunner.runOneTest("nonexistant_reptype_01") }
  @Test def test_nonexistantOutputTypeCalc_01() { fnErrRunner.runOneTest("nonexistantOutputTypeCalc_01") }
  @Test def test_nonexistantInputTypeCalc_01() { fnErrRunner.runOneTest("nonexistantInputTypeCalc_01") }
  @Test def test_nonexistantTypeCalcType_01() { fnErrRunner.runOneTest("nonexistantTypeCalcType_01") }
  @Test def test_nonexistantTypeCalcType_02() { fnErrRunner.runOneTest("nonexistantTypeCalcType_02") }

}
