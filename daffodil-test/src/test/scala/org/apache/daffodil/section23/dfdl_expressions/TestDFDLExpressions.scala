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

package org.apache.daffodil.section23.dfdl_expressions

import org.junit._
import org.apache.daffodil.tdml.Runner

object TestDFDLExpressions {
  val testDir = "/org/apache/daffodil/section23/dfdl_expressions/"
  val testDir2 = "/org/apache/daffodil/section23/dfdl_functions/"

  // I'm not sure these belong in section23, but there is no section of the spec that
  // is about all these properties together, yet, since there is common mechanism here
  // I really think their tests should not be scattered all over to the sections where each
  // property is defined.

  val testDir4 = "/org/apache/daffodil/section23/runtime_properties/"

  val runner = Runner(testDir, "expressions.tdml")
  val runnerNV = Runner(testDir, "expressions.tdml", validateDFDLSchemas = false)
  val runner2 = Runner(testDir2, "Functions.tdml")
  val runner2_utf8 = Runner(testDir2, "Functions_UTF8.tdml")
  val runner2b = Runner(testDir2, "Functions-neg.tdml")
  val runner3 = Runner(testDir, "expression_fail.tdml", validateTDMLFile = false)
  val runner4 = Runner(testDir4, "runtime-properties.tdml", validateTDMLFile = true, validateDFDLSchemas = false)
  val runner_fun = Runner(testDir, "functions.tdml")
  val runner5 = Runner(testDir, "valueLength.tdml")

  val testDir6 = "/org/apache/daffodil/section23/dfdl_expressions/"
  val runner6 = Runner(testDir6, "expressions.tdml")

  val runner7 = Runner(testDir, "expressions2.tdml", compileAllTopLevel = true)

  @AfterClass def shutDown(): Unit = {
    runner.reset
    runnerNV.reset
    runner2.reset
    runner2_utf8.reset
    runner2b.reset
    runner3.reset
    runner4.reset
    runner_fun.reset
    runner5.reset
    runner6.reset
    runner7.reset
  }
}

class TestDFDLExpressions {
  import TestDFDLExpressions._

  @Test def test_variableRefError(): Unit = { runner4.runOneTest("variableRefError") }

  @Test def test_byteOrderExpr1(): Unit = { runner4.runOneTest("byteOrderExpr1") }
  @Test def test_byteOrderExpr1b(): Unit = { runner4.runOneTest("byteOrderExpr1b") }
  @Test def test_byteOrderExpr2(): Unit = { runner4.runOneTest("byteOrderExpr2") }
  @Test def test_byteOrderExpr2b(): Unit = { runner4.runOneTest("byteOrderExpr2b") }
  @Test def test_byteOrderExpr3(): Unit = { runner4.runOneTest("byteOrderExpr3") }
  @Test def test_byteOrderExpr4(): Unit = { runner4.runOneTest("byteOrderExpr4") }
  @Test def test_byteOrderExpr5(): Unit = { runner4.runOneTest("byteOrderExpr5") }
  @Test def test_byteOrderExpr6(): Unit = { runner4.runOneTest("byteOrderExpr6") }
  @Test def test_byteOrderExpr7(): Unit = { runner4.runOneTest("byteOrderExpr7") }
  @Test def test_byteOrderExpr7b(): Unit = { runner4.runOneTest("byteOrderExpr7b") }
  @Test def test_byteOrderExpr7c(): Unit = { runner4.runOneTest("byteOrderExpr7c") }
  @Test def test_byteOrderExpr8(): Unit = { runner4.runOneTest("byteOrderExpr8") }
  @Test def test_byteOrderExpr9(): Unit = { runner4.runOneTest("byteOrderExpr9") }

  //DFDL-1111
  //@Test def test_diagnostics_01() { runner.runOneTest("diagnostics_01") }
  //@Test def test_diagnostics_02() { runner.runOneTest("diagnostics_02") }
  //@Test def test_diagnostics_03() { runner.runOneTest("diagnostics_03") }

  @Test def test_sequenceReturned_01(): Unit = { runner.runOneTest("sequenceReturned_01") }
  @Test def test_sequenceReturned_02(): Unit = { runner.runOneTest("sequenceReturned_02") }
  @Test def test_sequenceReturned_03(): Unit = { runner.runOneTest("sequenceReturned_03") }
  @Test def test_longPath_01(): Unit = { runner.runOneTest("longPath_01") }
  @Test def test_longPath_02(): Unit = { runner.runOneTest("longPath_02") }

  @Test def test_hiddenDataExpression(): Unit = { runner.runOneTest("hiddenDataExpression") }
  @Test def test_hiddenDataExpression2(): Unit = { runner.runOneTest("hiddenDataExpression2") }

  @Test def test_arrayIndexOutOfBounds_01(): Unit = { runner.runOneTest("arrayIndexOutOfBounds_01") }
  @Test def test_arrayIndexOutOfBounds_02(): Unit = { runner.runOneTest("arrayIndexOutOfBounds_02") }
  @Test def test_arrayIndexOutOfBounds_03(): Unit = { runner.runOneTest("arrayIndexOutOfBounds_03") }

  @Test def test_arrayIndexOutOfBounds_05(): Unit = { runner.runOneTest("arrayIndexOutOfBounds_05") }

  @Test def test_asterisk_01(): Unit = { runner.runOneTest("asterisk_01") }
  @Test def test_asterisk_02(): Unit = { runner.runOneTest("asterisk_02") }
  @Test def test_asterisk_03(): Unit = { runner.runOneTest("asterisk_03") }

  //DFDL-1146
  //@Test def test_attribute_axis_01() { runner.runOneTest("attribute_axis_01") }
  //@Test def test_attribute_axis_02() { runner.runOneTest("attribute_axis_02") }
  //@Test def test_attribute_axis_03() { runner.runOneTest("attribute_axis_03") }

  @Test def test_comparison_operators_01(): Unit = { runner.runOneTest("comparison_operators_01") }
  @Test def test_comparison_operators_02(): Unit = { runner.runOneTest("comparison_operators_02") }
  @Test def test_comparison_operators_03(): Unit = { runner.runOneTest("comparison_operators_03") }
  @Test def test_comparison_operators_04(): Unit = { runner.runOneTest("comparison_operators_04") }
  @Test def test_comparison_operators_05(): Unit = { runner.runOneTest("comparison_operators_05") }
  @Test def test_comparison_operators_06(): Unit = { runner.runOneTest("comparison_operators_06") }
  @Test def test_comparison_operators_07(): Unit = { runner.runOneTest("comparison_operators_07") }
  @Test def test_comparison_operators_08(): Unit = { runner.runOneTest("comparison_operators_08") }
  @Test def test_comparison_operators_09(): Unit = { runner.runOneTest("comparison_operators_09") }
  @Test def test_comparison_operators_10(): Unit = { runner.runOneTest("comparison_operators_10") }
  @Test def test_comparison_operators_11(): Unit = { runner.runOneTest("comparison_operators_11") }
  @Test def test_comparison_operators_12(): Unit = { runner.runOneTest("comparison_operators_12") }
  @Test def test_comparison_operators_13(): Unit = { runner.runOneTest("comparison_operators_13") }
  @Test def test_comparison_operators_14(): Unit = { runner.runOneTest("comparison_operators_14") }
  @Test def test_comparison_operators_15(): Unit = { runner.runOneTest("comparison_operators_15") }
  @Test def test_comparison_operators_16(): Unit = { runner.runOneTest("comparison_operators_16") }
  @Test def test_comparison_operators_17(): Unit = { runner.runOneTest("comparison_operators_17") }
  @Test def test_comparison_operators_18(): Unit = { runner.runOneTest("comparison_operators_18") }
  @Test def test_comparison_operators_19(): Unit = { runner.runOneTest("comparison_operators_19") }
  @Test def test_comparison_operators_20(): Unit = { runner.runOneTest("comparison_operators_20") }
  @Test def test_comparison_operators_21(): Unit = { runner.runOneTest("comparison_operators_21") }
  @Test def test_comparison_operators_22(): Unit = { runner.runOneTest("comparison_operators_22") }
  @Test def test_comparison_operators_23(): Unit = { runner.runOneTest("comparison_operators_23") }
  @Test def test_comparison_operators_24(): Unit = { runner.runOneTest("comparison_operators_24") }
  @Test def test_comparison_operators_25(): Unit = { runner.runOneTest("comparison_operators_25") }
  @Test def test_comparison_operators_26(): Unit = { runner.runOneTest("comparison_operators_26") }
  @Test def test_comparison_operators_27(): Unit = { runner.runOneTest("comparison_operators_27") }
  @Test def test_comparison_operators_28(): Unit = { runner.runOneTest("comparison_operators_28") }

  @Test def test_comparison_operators_29(): Unit = { runner.runOneTest("comparison_operators_29") }
  @Test def test_comparison_operators_30(): Unit = { runner.runOneTest("comparison_operators_30") }
  @Test def test_comparison_operators_31(): Unit = { runner.runOneTest("comparison_operators_31") }
  @Test def test_comparison_operators_32(): Unit = { runner.runOneTest("comparison_operators_32") }
  @Test def test_comparison_operators_33(): Unit = { runner.runOneTest("comparison_operators_33") }
  @Test def test_comparison_operators_34(): Unit = { runner.runOneTest("comparison_operators_34") }
  @Test def test_comparison_operators_35(): Unit = { runner.runOneTest("comparison_operators_35") }
  @Test def test_comparison_operators_36(): Unit = { runner.runOneTest("comparison_operators_36") }
  @Test def test_comparison_operators_37(): Unit = { runner.runOneTest("comparison_operators_37") }
  @Test def test_comparison_operators_38(): Unit = { runner.runOneTest("comparison_operators_38") }
  @Test def test_comparison_operators_39(): Unit = { runner.runOneTest("comparison_operators_39") }
  @Test def test_comparison_operators_40(): Unit = { runner.runOneTest("comparison_operators_40") }
  @Test def test_comparison_operators_41(): Unit = { runner.runOneTest("comparison_operators_41") }
  @Test def test_comparison_operators_42(): Unit = { runner.runOneTest("comparison_operators_42") }
  @Test def test_comparison_operators_43(): Unit = { runner.runOneTest("comparison_operators_43") }
  @Test def test_comparison_operators_44(): Unit = { runner.runOneTest("comparison_operators_44") }
  @Test def test_comparison_operators_45(): Unit = { runner.runOneTest("comparison_operators_45") }
  @Test def test_comparison_operators_46(): Unit = { runner.runOneTest("comparison_operators_46") }
  @Test def test_comparison_operators_46a(): Unit = { runner.runOneTest("comparison_operators_46a") }

  // from XPath Spec Sec 10.4.6.1 Examples
  @Test def test_comparison_operators_47(): Unit = { runner.runOneTest("comparison_operators_47") }
  @Test def test_comparison_operators_48(): Unit = { runner.runOneTest("comparison_operators_48") }
  @Test def test_comparison_operators_49(): Unit = { runner.runOneTest("comparison_operators_49") }
  @Test def test_comparison_operators_50(): Unit = { runner.runOneTest("comparison_operators_50") }
  @Test def test_comparison_operators_51(): Unit = { runner.runOneTest("comparison_operators_51") }
  @Test def test_comparison_operators_52(): Unit = { runner.runOneTest("comparison_operators_52") }
  @Test def test_comparison_operators_53(): Unit = { runner.runOneTest("comparison_operators_53") }

  @Test def test_comparison_operators_54(): Unit = { runner.runOneTest("comparison_operators_54") }
  @Test def test_comparison_operators_55(): Unit = { runner.runOneTest("comparison_operators_55") }
  @Test def test_comparison_operators_56(): Unit = { runner.runOneTest("comparison_operators_56") }
  @Test def test_comparison_operators_57(): Unit = { runner.runOneTest("comparison_operators_57") }
  @Test def test_comparison_operators_58(): Unit = { runner.runOneTest("comparison_operators_58") }
  @Test def test_comparison_operators_59(): Unit = { runner.runOneTest("comparison_operators_59") }
  @Test def test_comparison_operators_60(): Unit = { runner.runOneTest("comparison_operators_60") }
  @Test def test_comparison_operators_61(): Unit = { runner.runOneTest("comparison_operators_61") }
  @Test def test_comparison_operators_62(): Unit = { runner.runOneTest("comparison_operators_62") }
  @Test def test_comparison_operators_63(): Unit = { runner.runOneTest("comparison_operators_63") }
  @Test def test_comparison_operators_64(): Unit = { runner.runOneTest("comparison_operators_64") }
  @Test def test_comparison_operators_65(): Unit = { runner.runOneTest("comparison_operators_65") }
  @Test def test_comparison_operators_66(): Unit = { runner.runOneTest("comparison_operators_66") }
  @Test def test_comparison_operators_67(): Unit = { runner.runOneTest("comparison_operators_67") }
  @Test def test_comparison_operators_68(): Unit = { runner.runOneTest("comparison_operators_68") }
  @Test def test_comparison_operators_69(): Unit = { runner.runOneTest("comparison_operators_69") }
  @Test def test_comparison_operators_70(): Unit = { runner.runOneTest("comparison_operators_70") }
  @Test def test_comparison_operators_71(): Unit = { runner.runOneTest("comparison_operators_71") }
  @Test def test_comparison_operators_72(): Unit = { runner.runOneTest("comparison_operators_72") }
  @Test def test_comparison_operators_73(): Unit = { runner.runOneTest("comparison_operators_73") }
  @Test def test_comparison_operators_74(): Unit = { runner.runOneTest("comparison_operators_74") }
  @Test def test_comparison_operators_75(): Unit = { runner.runOneTest("comparison_operators_75") }
  @Test def test_comparison_operators_76(): Unit = { runner.runOneTest("comparison_operators_76") }
  @Test def test_comparison_operators_77(): Unit = { runner.runOneTest("comparison_operators_77") }
  @Test def test_comparison_operators_78(): Unit = { runner.runOneTest("comparison_operators_78") }
  @Test def test_comparison_operators_79(): Unit = { runner.runOneTest("comparison_operators_79") }
  @Test def test_comparison_operators_80(): Unit = { runner.runOneTest("comparison_operators_80") }
  @Test def test_comparison_operators_81(): Unit = { runner.runOneTest("comparison_operators_81") }
  @Test def test_comparison_operators_82(): Unit = { runner.runOneTest("comparison_operators_82") }
  @Test def test_comparison_operators_83(): Unit = { runner.runOneTest("comparison_operators_83") }

  @Test def test_regexLookahead(): Unit = { runnerNV.runOneTest("regexLookahead") }
  @Test def test_regexLookaheadFail(): Unit = { runnerNV.runOneTest("regexLookaheadFail") }
  @Test def test_regexLookaheadFail2(): Unit = { runnerNV.runOneTest("regexLookaheadFail2") }
  @Test def test_regexCompatFail(): Unit = { runner.runOneTest("regexCompatFail") }

  @Test def test_expressionRules01(): Unit = { runner.runOneTest("expressionRules01") }
  @Test def test_expressionRules02(): Unit = { runner.runOneTest("expressionRules02") }
  @Test def test_expressionRules03(): Unit = { runner.runOneTest("expressionRules03") }
  @Test def test_expressionRules04(): Unit = { runner.runOneTest("expressionRules04") }
  @Test def test_expressionRules05(): Unit = { runner.runOneTest("expressionRules05") }
  @Test def test_expressionRules06(): Unit = { runner.runOneTest("expressionRules06") }

  // uses lengthUnits bytes with utf-8 and lengthKind Explicit
  @Test def test_lke3_rel(): Unit = { runner.runOneTest("lke3_rel") }

  @Test def test_lke1_rel(): Unit = { runner.runOneTest("lke1_rel") }
  @Test def test_lke1_abs(): Unit = { runner.runOneTest("lke1_abs") }
  @Test def test_ocke_from_string_01(): Unit = { runner.runOneTest("ocke_from_string_01") }
  @Test def test_ocke_from_string_02(): Unit = { runner.runOneTest("ocke_from_string_02") }
  @Test def test_ocke1(): Unit = { runner.runOneTest("ocke1") }
  @Test def test_ocke2(): Unit = { runner.runOneTest("ocke2") }
  @Test def test_ArrayOptElem_01(): Unit = { runner.runOneTest("ArrayOptElem_01") }
  @Test def test_lke2_rel(): Unit = { runner.runOneTest("lke2_rel") }
  @Test def test_expression_type_error1(): Unit = { runner.runOneTest("expression_type_error1") }
  @Test def test_expression_type_error2(): Unit = { runner.runOneTest("expression_type_error2") }
  @Test def test_expression_type_error3(): Unit = { runner.runOneTest("expression_type_error3") }
  @Test def test_expression_type_error4(): Unit = { runner.runOneTest("expression_type_error4") }
  @Test def test_expression_type_error5(): Unit = { runner.runOneTest("expression_type_error5") }
  @Test def test_expression_type_error6(): Unit = { runner.runOneTest("expression_type_error6") }
  @Test def test_expression_type_error7(): Unit = { runner.runOneTest("expression_type_error7") }
  @Test def test_expression_unknown_prefix(): Unit = { runner.runOneTest("expression_unknown_prefix") }
  @Test def test_ocke_rel(): Unit = { runner.runOneTest("ocke_rel") }
  @Test def test_ocke_rel2(): Unit = { runner.runOneTest("ocke_rel2") }
  @Test def test_ocke_rel3(): Unit = { runner.runOneTest("ocke_rel3") }
  @Test def test_ocke_rel4(): Unit = { runner.runOneTest("ocke_rel4") }
  @Test def test_ocke_step_dne(): Unit = { runner.runOneTest("ocke_step_dne") }
  @Test def test_ocke_array_index_step_dne(): Unit = { runner.runOneTest("ocke_array_index_step_dne") }
  @Test def test_ocke_non_upward(): Unit = { runner.runOneTest("ocke_non_upward") }
  @Test def test_ocke_single_upward(): Unit = { runner.runOneTest("ocke_single_upward") }
  @Test def test_internal_space_preserved(): Unit = { runner.runOneTest("internal_space_preserved") }
  @Test def test_internal_space_preserved2(): Unit = { runner.runOneTest("internal_space_preserved2") }
  @Test def test_internal_space_preserved3a(): Unit = { runner.runOneTest("internal_space_preserved3a") }
  @Test def test_internal_space_preserved3b(): Unit = { runner.runOneTest("internal_space_preserved3b") }
  @Test def test_internal_space_preserved4(): Unit = { runner.runOneTest("internal_space_preserved4") }
  @Test def test_internal_space_not_preserved1(): Unit = { runner.runOneTest("internal_space_not_preserved1") }
  @Test def test_internal_space_not_preserved2(): Unit = { runner.runOneTest("internal_space_not_preserved2") }

  @Test def test_whitespace_expression(): Unit = { runner.runOneTest("whitespace_expression") }
  @Test def test_whitespace_expression2(): Unit = { runner.runOneTest("whitespace_expression2") }

  @Test def test_expresion_bad_path_to_element(): Unit = { runner.runOneTest("expresion_bad_path_to_element") }
  @Test def test_ArrayOptElem_02(): Unit = { runner.runOneTest("ArrayOptElem_02") }

  @Test def test_dfdlSelfReferencingExpression1(): Unit = { runner.runOneTest("dfdlSelfReferencingExpression1") }
  @Test def test_dfdlSelfReferencingExpression2(): Unit = { runner.runOneTest("dfdlSelfReferencingExpression2") }
  @Test def test_dfdlSelfReferencingExpression3(): Unit = { runner.runOneTest("dfdlSelfReferencingExpression3") }

  @Test def test_nonFunctionIsDetected() = { runnerNV.runOneTest("nonFunctionIsDetected") }
  @Test def test_constantFunction1(): Unit = { runnerNV.runOneTest("constantFunction1") }
  @Test def test_dfdlPosition1(): Unit = { runnerNV.runOneTest("dfdlPosition1") }
  @Test def test_dfdlPosition2(): Unit = { runnerNV.runOneTest("dfdlPosition2") }
  @Test def test_dfdlPosition3(): Unit = { runnerNV.runOneTest("dfdlPosition3") }
  @Test def test_dfdlPosition4(): Unit = { runnerNV.runOneTest("dfdlPosition4") }
  @Test def test_dfdlPosition5(): Unit = { runnerNV.runOneTest("dfdlPosition5") }

  @Test def test_repeatFlags1(): Unit = { runner.runOneTest("repeatFlags1") }
  @Test def test_repeatFlags2(): Unit = { runner.runOneTest("repeatFlags2") }
  @Test def test_repeatFlags3(): Unit = { runner.runOneTest("repeatFlags3") }
  @Test def test_repeatFlags4(): Unit = { runner.runOneTest("repeatFlags4") }
  @Test def test_repeatFlags5(): Unit = { runner.runOneTest("repeatFlags5") }

  @Test def test_repeatBitFlags1(): Unit = { runner.runOneTest("repeatBitFlags1") }
  @Test def test_repeatBitFlags2(): Unit = { runner.runOneTest("repeatBitFlags2") }
  @Test def test_repeatBitFlags3(): Unit = { runner.runOneTest("repeatBitFlags3") }
  @Test def test_repeatBitFlags4(): Unit = { runner.runOneTest("repeatBitFlags4") }
  @Test def test_repeatBitFlags5(): Unit = { runner.runOneTest("repeatBitFlags5") }
  @Test def test_repeatBitFlags6(): Unit = { runner.runOneTest("repeatBitFlags6") }

  @Test def test_invalid_enum_1(): Unit = { runner.runOneTest("invalid_enum_1") }
  @Test def test_invalid_enum_2(): Unit = { runner.runOneTest("invalid_enum_2") }
  @Test def test_invalid_enum_3(): Unit = { runner.runOneTest("invalid_enum_3") }

  // Test removed including TDML for it. DPath no longer will even execution expressions that have
  // type errors, and type errors at runtime cause SDE so you can't if-then-else them into
  // some usable thing.
  // @Test def test_trueFalseTypeError() { runner.runOneTest("trueFalseTypeError") }
  @Test def test_trueFalseTypeCorrect(): Unit = { runner.runOneTest("trueFalseTypeCorrect") }

  @Test def test_predicate_01(): Unit = { runner.runOneTest("predicate_01") }
  //DFDL-1164
  //@Test def test_predicate_02() { runner.runOneTest("predicate_02") }
  //@Test def test_predicate_03() { runner.runOneTest("predicate_03") }
  @Test def test_predicate_04(): Unit = { runner.runOneTest("predicate_04") }
  @Test def test_predicate_05(): Unit = { runner.runOneTest("predicate_05") }

  @Test def test_sequential_and_01(): Unit = { runner.runOneTest("sequential_and_01") }
  @Test def test_sequential_and_02(): Unit = { runner.runOneTest("sequential_and_02") }
  @Test def test_sequential_and_03(): Unit = { runner.runOneTest("sequential_and_03") }
  @Test def test_sequential_and_04(): Unit = { runner.runOneTest("sequential_and_04") }
  @Test def test_sequential_and_05(): Unit = { runner.runOneTest("sequential_and_05") }
  @Test def test_sequential_or_01(): Unit = { runner.runOneTest("sequential_or_01") }
  @Test def test_sequential_or_02(): Unit = { runner.runOneTest("sequential_or_02") }
  @Test def test_sequential_or_03(): Unit = { runner.runOneTest("sequential_or_03") }
  @Test def test_sequential_or_04(): Unit = { runner.runOneTest("sequential_or_04") }
  @Test def test_sequential_or_05(): Unit = { runner.runOneTest("sequential_or_05") }
  @Test def test_sequential_and_or_01(): Unit = { runner.runOneTest("sequential_and_or_01") }
  @Test def test_sequential_and_or_02(): Unit = { runner.runOneTest("sequential_and_or_02") }
  @Test def test_sequential_and_or_03(): Unit = { runner.runOneTest("sequential_and_or_03") }
  @Test def test_sequential_and_or_04(): Unit = { runner.runOneTest("sequential_and_or_04") }
  @Test def test_sequential_and_or_05(): Unit = { runner.runOneTest("sequential_and_or_05") }
  @Test def test_sequential_and_or_06(): Unit = { runner.runOneTest("sequential_and_or_06") }
  @Test def test_sequential_and_or_07(): Unit = { runner.runOneTest("sequential_and_or_07") }

  //DFDL-1059
  @Test def test_parent_axis_01(): Unit = { runner.runOneTest("parent_axis_01") }
  @Test def test_child_axis_01(): Unit = { runner.runOneTest("child_axis_01") }
  @Test def test_self_axis_01(): Unit = { runner.runOneTest("self_axis_01") }
  //@Test def test_multiple_axis_01() { runner.runOneTest("multiple_axis_01") }

  @Test def test_attribute_axis_04(): Unit = { runner.runOneTest("attribute_axis_04") }

  @Test def test_ancestor_axis_01(): Unit = { runner.runOneTest("ancestor_axis_01") }
  @Test def test_ancestor_or_self_axis_01(): Unit = { runner.runOneTest("ancestor_or_self_axis_01") }
  @Test def test_descendant_axis_01(): Unit = { runner.runOneTest("descendant_axis_01") }
  @Test def test_descendant_or_self_axis_01(): Unit = { runner.runOneTest("descendant_or_self_axis_01") }
  @Test def test_following_axis_01(): Unit = { runner.runOneTest("following_axis_01") }
  @Test def test_following_sibling_axis_01(): Unit = { runner.runOneTest("following_sibling_axis_01") }
  @Test def test_namespace_axis_01(): Unit = { runner.runOneTest("namespace_axis_01") }
  @Test def test_preceding_axis_01(): Unit = { runner.runOneTest("preceding_axis_01") }
  @Test def test_preceding_sibling_axis_01(): Unit = { runner.runOneTest("preceding_sibling_axis_01") }

  /////////////////////// FUNCTIONS ///////////////////////////

  @Test def test_dateTimeFunctions01(): Unit = { runner_fun.runOneTest("dateTimeFunctions01") }
  @Test def test_dateTimeFunctions02(): Unit = { runner_fun.runOneTest("dateTimeFunctions02") }
  @Test def test_dateFunctions01(): Unit = { runner_fun.runOneTest("dateFunctions01") }
  @Test def test_dateFunctions02(): Unit = { runner_fun.runOneTest("dateFunctions02") }
  @Test def test_timeFunctions01(): Unit = { runner_fun.runOneTest("timeFunctions01") }
  @Test def test_timeFunctions02(): Unit = { runner_fun.runOneTest("timeFunctions02") }
  @Test def test_functionFail01(): Unit = { runner_fun.runOneTest("functionFail01") }
  @Test def test_functionFail02(): Unit = { runner_fun.runOneTest("functionFail02") }
  @Test def test_functionFail03(): Unit = { runner_fun.runOneTest("functionFail03") }

  @Test def test_substringFunction01(): Unit = { runner_fun.runOneTest("substringFunction01") }
  @Test def test_substringFunction02(): Unit = { runner_fun.runOneTest("substringFunction02") }
  @Test def test_substringFunction03(): Unit = { runner_fun.runOneTest("substringFunction03") }
  @Test def test_substringFunction04(): Unit = { runner_fun.runOneTest("substringFunction04") }
  @Test def test_substringFunction05(): Unit = { runner_fun.runOneTest("substringFunction05") }
  @Test def test_substringFunction06(): Unit = { runner_fun.runOneTest("substringFunction06") }
  @Test def test_substringFunction07(): Unit = { runner_fun.runOneTest("substringFunction07") }
  @Test def test_substringFunction08(): Unit = { runner_fun.runOneTest("substringFunction08") }
  @Test def test_substringFunction12(): Unit = { runner_fun.runOneTest("substringFunction12") }
  @Test def test_substringFunction13(): Unit = { runner_fun.runOneTest("substringFunction13") }
  @Test def test_substringFunction14(): Unit = { runner_fun.runOneTest("substringFunction14") }
  @Test def test_substringFunction15(): Unit = { runner_fun.runOneTest("substringFunction15") }

  @Test def test_boolFunctionChoice_01(): Unit = { runner2.runOneTest("boolFunctionChoice_01") }
  @Test def test_boolFunctionChoice_02(): Unit = { runner2.runOneTest("boolFunctionChoice_02") }
  @Test def test_boolFunctionChoice_03(): Unit = { runner2.runOneTest("boolFunctionChoice_03") }

  @Test def test_boolFlags_01(): Unit = { runner2.runOneTest("boolFlags_01") }
  @Test def test_boolFlags_02(): Unit = { runner2.runOneTest("boolFlags_02") }
  @Test def test_boolFlags_03(): Unit = { runner2.runOneTest("boolFlags_03") }
  @Test def test_boolFlags_04(): Unit = { runner2.runOneTest("boolFlags_04") }
  @Test def test_boolFlags_05(): Unit = { runner2.runOneTest("boolFlags_05") }

  @Test def test_not_01(): Unit = { runner2.runOneTest("not_01") }
  @Test def test_not_02(): Unit = { runner2.runOneTest("not_02") }
  @Test def test_not_03(): Unit = { runner2.runOneTest("not_03") }
  //DFDL-1076
  //@Test def test_not_04() { runner2.runOneTest("not_04") }
  //DFDL-1075
  //@Test def test_not_05() { runner2.runOneTest("not_05") }
  //@Test def test_not_07() { runner2.runOneTest("not_07") }
  @Test def test_not_06(): Unit = { runner2.runOneTest("not_06") }
  @Test def test_not_08(): Unit = { runner2.runOneTest("not_08") }
  @Test def test_not_09(): Unit = { runner2.runOneTest("not_09") }
  @Test def test_not_10(): Unit = { runner2.runOneTest("not_10") }
  @Test def test_not_11(): Unit = { runner2.runOneTest("not_11") }
  @Test def test_not_12(): Unit = { runner2.runOneTest("not_12") }
  @Test def test_not_13(): Unit = { runner2.runOneTest("not_13") }
  @Test def test_not_14(): Unit = { runner2.runOneTest("not_14") }
  @Test def test_not_15(): Unit = { runner2.runOneTest("not_15") }
  @Test def test_not_16(): Unit = { runner2.runOneTest("not_16") }

  @Test def test_xPathFunc_abs_01(): Unit = { runner2.runOneTest("xPathFunc_abs_01") }
  @Test def test_xPathFunc_abs_02(): Unit = { runner2.runOneTest("xPathFunc_abs_02") }
  @Test def test_xPathFunc_abs_03(): Unit = { runner2.runOneTest("xPathFunc_abs_03") }
  @Test def test_xPathFunc_abs_04(): Unit = { runner2.runOneTest("xPathFunc_abs_04") }
  @Test def test_abs_05(): Unit = { runner2.runOneTest("abs_05") }
  @Test def test_abs_06(): Unit = { runner2.runOneTest("abs_06") }
  @Test def test_abs_07(): Unit = { runner2.runOneTest("abs_07") }
  @Test def test_abs_08(): Unit = { runner2.runOneTest("abs_08") }
  @Test def test_abs_09(): Unit = { runner2.runOneTest("abs_09") }

  @Test def test_xPathFunc_ceil_01(): Unit = { runner2.runOneTest("xPathFunc_ceil_01") }
  @Test def test_xPathFunc_ceil_02(): Unit = { runner2.runOneTest("xPathFunc_ceil_02") }
  @Test def test_xPathFunc_ceil_03(): Unit = { runner2.runOneTest("xPathFunc_ceil_03") }
  @Test def test_xPathFunc_ceil_04(): Unit = { runner2.runOneTest("xPathFunc_ceil_04") }
  @Test def test_xPathFunc_ceil_05(): Unit = { runner2.runOneTest("xPathFunc_ceil_05") }
  @Test def test_ceil_06(): Unit = { runner2.runOneTest("ceil_06") }
  @Test def test_ceil_07(): Unit = { runner2.runOneTest("ceil_07") }
  @Test def test_ceil_08(): Unit = { runner2.runOneTest("ceil_08") }
  @Test def test_ceil_09(): Unit = { runner2.runOneTest("ceil_09") }

  @Test def test_xPathFunc_floor_01(): Unit = { runner2.runOneTest("xPathFunc_floor_01") }
  @Test def test_xPathFunc_floor_02(): Unit = { runner2.runOneTest("xPathFunc_floor_02") }
  @Test def test_xPathFunc_floor_03(): Unit = { runner2.runOneTest("xPathFunc_floor_03") }
  @Test def test_xPathFunc_floor_04(): Unit = { runner2.runOneTest("xPathFunc_floor_04") }
  @Test def test_xPathFunc_floor_05(): Unit = { runner2.runOneTest("xPathFunc_floor_05") }
  @Test def test_floor_06(): Unit = { runner2.runOneTest("floor_06") }
  @Test def test_floor_07(): Unit = { runner2.runOneTest("floor_07") }
  @Test def test_floor_08(): Unit = { runner2.runOneTest("floor_08") }
  @Test def test_floor_09(): Unit = { runner2.runOneTest("floor_09") }

  @Test def test_xPathFunc_round_01(): Unit = { runner2.runOneTest("xPathFunc_round_01") }
  @Test def test_xPathFunc_round_02(): Unit = { runner2.runOneTest("xPathFunc_round_02") }
  @Test def test_xPathFunc_round_03(): Unit = { runner2.runOneTest("xPathFunc_round_03") }
  @Test def test_xPathFunc_round_04(): Unit = { runner2.runOneTest("xPathFunc_round_04") }
  @Test def test_xPathFunc_round_05(): Unit = { runner2.runOneTest("xPathFunc_round_05") }
  @Test def test_xPathFunc_round_06(): Unit = { runner2.runOneTest("xPathFunc_round_06") }
  @Test def test_round_07(): Unit = { runner2.runOneTest("round_07") }
  @Test def test_round_08(): Unit = { runner2.runOneTest("round_08") }
  @Test def test_round_09(): Unit = { runner2.runOneTest("round_09") }
  @Test def test_round_10(): Unit = { runner2.runOneTest("round_10") }
  @Test def test_round_11(): Unit = { runner2.runOneTest("round_11") }
  @Test def test_round_12(): Unit = { runner2.runOneTest("round_12") }
  @Test def test_round_13(): Unit = { runner2.runOneTest("round_13") }
  @Test def test_xPathFunc_round_14(): Unit = { runner2.runOneTest("xPathFunc_round_14") }
  @Test def test_xPathFunc_round_15(): Unit = { runner2.runOneTest("xPathFunc_round_15") }
  @Test def test_xPathFunc_round_16(): Unit = { runner2.runOneTest("xPathFunc_round_16") }
  @Test def test_round_17(): Unit = { runner2.runOneTest("round_17") }
  @Test def test_round_18(): Unit = { runner2.runOneTest("round_18") }
  @Test def test_round_19(): Unit = { runner2.runOneTest("round_19") }

  @Test def test_xPathFunc_round_hte_01(): Unit = { runner2.runOneTest("xPathFunc_round_hte_01") }
  @Test def test_xPathFunc_round_hte_02(): Unit = { runner2.runOneTest("xPathFunc_round_hte_02") }
  @Test def test_xPathFunc_round_hte_03(): Unit = { runner2.runOneTest("xPathFunc_round_hte_03") }
  @Test def test_xPathFunc_round_hte_04(): Unit = { runner2.runOneTest("xPathFunc_round_hte_04") }
  @Test def test_xPathFunc_round_hte_05(): Unit = { runner2.runOneTest("xPathFunc_round_hte_05") }
  @Test def test_xPathFunc_round_hte_06(): Unit = { runner2.runOneTest("xPathFunc_round_hte_06") }
  @Test def test_xPathFunc_round_hte_07(): Unit = { runner2.runOneTest("xPathFunc_round_hte_07") }
  @Test def test_xPathFunc_round_hte_08(): Unit = { runner2.runOneTest("xPathFunc_round_hte_08") }
  @Test def test_round_hte_09(): Unit = { runner2.runOneTest("round_hte_09") }
  @Test def test_round_hte_10(): Unit = { runner2.runOneTest("round_hte_10") }
  @Test def test_round_hte_11(): Unit = { runner2.runOneTest("round_hte_11") }
  @Test def test_round_hte_12(): Unit = { runner2.runOneTest("round_hte_12") }
  @Test def test_round_hte_13(): Unit = { runner2.runOneTest("round_hte_13") }
  @Test def test_round_hte_14(): Unit = { runner2.runOneTest("round_hte_14") }
  @Test def test_round_hte_15(): Unit = { runner2.runOneTest("round_hte_15") }
  @Test def test_round_hte_16(): Unit = { runner2.runOneTest("round_hte_16") }
  @Test def test_round_hte_17(): Unit = { runner2.runOneTest("round_hte_17") }
  @Test def test_round_hte_18(): Unit = { runner2.runOneTest("round_hte_18") }

  //DAFFODIL-1080
  @Test def test_empty_02(): Unit = { runner2.runOneTest("empty_02") }
  @Test def test_exists_02(): Unit = { runner2.runOneTest("exists_02") }

  @Test def test_count_05b(): Unit = { runner2.runOneTest("count_05b") }

  //DFDL-1097
  //@Test def test_local_name_06() { runner2.runOneTest("local_name_06") }

  @Test def test_empty_01(): Unit = { runner2.runOneTest("empty_01") }
  @Test def test_empty_03(): Unit = { runner2.runOneTest("empty_03") }
  @Test def test_empty_04(): Unit = { runner2.runOneTest("empty_04") }
  @Test def test_empty_05(): Unit = { runner2.runOneTest("empty_05") }
  @Test def test_empty_06(): Unit = { runner2.runOneTest("empty_06") }
  @Test def test_empty_07(): Unit = { runner2.runOneTest("empty_07") }
  @Test def test_empty_08(): Unit = { runner2.runOneTest("empty_08") }
  @Test def test_empty_09(): Unit = { runner2.runOneTest("empty_09") }

  @Test def test_exists_01(): Unit = { runner2.runOneTest("exists_01") }
  @Test def test_exists_03(): Unit = { runner2.runOneTest("exists_03") }
  @Test def test_exists_04(): Unit = { runner2.runOneTest("exists_04") }
  @Test def test_exists_06(): Unit = { runner2.runOneTest("exists_06") }
  @Test def test_exists_07(): Unit = { runner2.runOneTest("exists_07") }
  @Test def test_exists_05(): Unit = { runner2.runOneTest("exists_05") }
  @Test def test_exists_08(): Unit = { runner2.runOneTest("exists_08") }
  @Test def test_exists_09(): Unit = { runner2.runOneTest("exists_09") }
  @Test def test_exists_10(): Unit = { runner2.runOneTest("exists_10") }
  @Test def test_exists_11(): Unit = { runner2.runOneTest("exists_11") }
  @Test def test_exists_12(): Unit = { runner2.runOneTest("exists_12") }
  @Test def test_exists_13(): Unit = { runner2.runOneTest("exists_13") }
  @Test def test_exists_14(): Unit = { runner2.runOneTest("exists_14") }

  //DFDL-1189
  //@Test def test_exactly_one_01() { runner2.runOneTest("exactly_one_01") }
  //@Test def test_exactly_one_02() { runner2.runOneTest("exactly_one_02") }
  //@Test def test_exactly_one_03() { runner2.runOneTest("exactly_one_03") }
  @Test def test_exactly_one_04(): Unit = { runner2.runOneTest("exactly_one_04") }
  //@Test def test_exactly_one_05() { runner2.runOneTest("exactly_one_05") }
  //@Test def test_exactly_one_06() { runner2.runOneTest("exactly_one_06") }

  @Test def test_count_01(): Unit = { runner2.runOneTest("count_01") }
  @Test def test_count_02(): Unit = { runner2.runOneTest("count_02") }
  @Test def test_count_03(): Unit = { runner2.runOneTest("count_03") }
  @Test def test_count_03b(): Unit = { runner2.runOneTest("count_03b") }
  @Test def test_count_04(): Unit = { runner2.runOneTest("count_04") }
  @Test def test_count_05(): Unit = { runner2.runOneTest("count_05") }
  @Test def test_count_05c(): Unit = { runner2.runOneTest("count_05c") }
  @Test def test_count_06(): Unit = { runner2.runOneTest("count_06") }
  @Test def test_count_06b(): Unit = { runner2.runOneTest("count_06b") }
  @Test def test_count_07(): Unit = { runner2.runOneTest("count_07") }
  @Test def test_count_08(): Unit = { runner2.runOneTest("count_08") }
  @Test def test_count_08b(): Unit = { runner2.runOneTest("count_08b") }
  @Test def test_count_09(): Unit = { runner2.runOneTest("count_09") }
  @Test def test_count_10(): Unit = { runner2.runOneTest("count_10") }
  @Test def test_count_11(): Unit = { runner2.runOneTest("count_11") }

  @Test def test_local_name_01(): Unit = { runner2.runOneTest("local_name_01") }
  @Test def test_local_name_02(): Unit = { runner2.runOneTest("local_name_02") }
  @Test def test_local_name_03(): Unit = { runner2.runOneTest("local_name_03") }
  @Test def test_local_name_04(): Unit = { runner2.runOneTest("local_name_04") }
  @Test def test_local_name_05(): Unit = { runner2.runOneTest("local_name_05") }
  //DFDL-1151
  //@Test def test_local_name_07() { runner2.runOneTest("local_name_07") }

  //DFDL-1101
  @Test def test_namespace_uri_01(): Unit = { runner2.runOneTest("namespace_uri_01") }
  @Test def test_namespace_uri_02(): Unit = { runner2.runOneTest("namespace_uri_02") }
  //DFDL-1114
  @Test def test_namespace_uri_03(): Unit = { runner2.runOneTest("namespace_uri_03") }
  @Test def test_namespace_uri_04(): Unit = { runner2.runOneTest("namespace_uri_04") }
  @Test def test_namespace_uri_05(): Unit = { runner2.runOneTest("namespace_uri_05") }
  @Test def test_namespace_uri_06(): Unit = { runner2.runOneTest("namespace_uri_06") }
  @Test def test_namespace_uri_07(): Unit = { runner2.runOneTest("namespace_uri_07") }
  @Test def test_namespace_uri_08(): Unit = { runner2.runOneTest("namespace_uri_08") }

  //DFDL-1233
  @Test def test_nilled_02(): Unit = { runner2.runOneTest("nilled_02") }
  @Test def test_nilled_03(): Unit = { runner2.runOneTest("nilled_03") }

  @Test def test_concat_01(): Unit = { runner2.runOneTest("concat_01") }
  @Test def test_concat_02(): Unit = { runner2.runOneTest("concat_02") }
  @Test def test_concat_03(): Unit = { runner2.runOneTest("concat_03") }
  @Test def test_concat_04(): Unit = { runner2.runOneTest("concat_04") }
  @Test def test_concat_05(): Unit = { runner2.runOneTest("concat_05") }

  @Test def test_substring_01(): Unit = { runner2.runOneTest("substring_01") }
  @Test def test_substring_02(): Unit = { runner2.runOneTest("substring_02") }
  @Test def test_substring_03(): Unit = { runner2.runOneTest("substring_03") }
  @Test def test_substring_04(): Unit = { runner2.runOneTest("substring_04") }
  @Test def test_substring_05(): Unit = { runner2.runOneTest("substring_05") }
  @Test def test_substring_06(): Unit = { runner2.runOneTest("substring_06") }
  @Test def test_substring_07(): Unit = { runner2.runOneTest("substring_07") }
  @Test def test_substring_08(): Unit = { runner2.runOneTest("substring_08") }
  @Test def test_substring_09(): Unit = { runner2.runOneTest("substring_09") }
  @Test def test_substring_10(): Unit = { runner2.runOneTest("substring_10") }
  @Test def test_substring_11(): Unit = { runner2.runOneTest("substring_11") }

  @Test def test_stringlength_01(): Unit = { runner2.runOneTest("stringlength_01") }
  @Test def test_stringlength_02(): Unit = { runner2.runOneTest("stringlength_02") }
  @Test def test_stringlength_03(): Unit = { runner2.runOneTest("stringlength_03") }
  @Test def test_stringlength_04(): Unit = { runner2.runOneTest("stringlength_04") }

  @Test def test_uppercase_01(): Unit = { runner2.runOneTest("uppercase_01") }
  @Test def test_uppercase_02(): Unit = { runner2.runOneTest("uppercase_02") }
  @Test def test_uppercase_03(): Unit = { runner2.runOneTest("uppercase_03") }

  @Test def test_lowercase_01(): Unit = { runner2.runOneTest("lowercase_01") }
  @Test def test_lowercase_02(): Unit = { runner2.runOneTest("lowercase_02") }
  @Test def test_lowercase_03(): Unit = { runner2.runOneTest("lowercase_03") }

  @Test def test_lowercase_04(): Unit = { runner2_utf8.runOneTest("lowercase_04") }
  @Test def test_uppercase_04(): Unit = { runner2_utf8.runOneTest("uppercase_04") }
  @Test def test_uppercase_05(): Unit = { runner2_utf8.runOneTest("uppercase_05") }
  @Test def test_lowercase_05(): Unit = { runner2_utf8.runOneTest("lowercase_05") }

  @Test def test_contains_01(): Unit = { runner2.runOneTest("contains_01") }
  @Test def test_contains_02(): Unit = { runner2.runOneTest("contains_02") }
  @Test def test_contains_03(): Unit = { runner2.runOneTest("contains_03") }
  @Test def test_contains_04(): Unit = { runner2.runOneTest("contains_04") }
  @Test def test_contains_05(): Unit = { runner2.runOneTest("contains_05") }
  @Test def test_contains_06(): Unit = { runner2.runOneTest("contains_06") }
  @Test def test_contains_07(): Unit = { runner2.runOneTest("contains_07") }

  @Test def test_startswith_01(): Unit = { runner2.runOneTest("startswith_01") }
  @Test def test_startswith_02(): Unit = { runner2.runOneTest("startswith_02") }
  @Test def test_startswith_03(): Unit = { runner2.runOneTest("startswith_03") }
  @Test def test_startswith_04(): Unit = { runner2.runOneTest("startswith_04") }
  @Test def test_startswith_05(): Unit = { runner2.runOneTest("startswith_05") }
  @Test def test_startswith_06(): Unit = { runner2.runOneTest("startswith_06") }
  @Test def test_startswith_07(): Unit = { runner2.runOneTest("startswith_07") }

  @Test def test_endswith_01(): Unit = { runner2.runOneTest("endswith_01") }
  @Test def test_endswith_02(): Unit = { runner2.runOneTest("endswith_02") }
  @Test def test_endswith_03(): Unit = { runner2.runOneTest("endswith_03") }
  @Test def test_endswith_04(): Unit = { runner2.runOneTest("endswith_04") }
  @Test def test_endswith_05(): Unit = { runner2.runOneTest("endswith_05") }
  @Test def test_endswith_06(): Unit = { runner2.runOneTest("endswith_06") }
  @Test def test_endswith_07(): Unit = { runner2.runOneTest("endswith_07") }

  @Test def test_substringbefore_01(): Unit = { runner2.runOneTest("substringbefore_01") }
  @Test def test_substringbefore_02(): Unit = { runner2.runOneTest("substringbefore_02") }
  @Test def test_substringbefore_03(): Unit = { runner2.runOneTest("substringbefore_03") }
  @Test def test_substringbefore_04(): Unit = { runner2.runOneTest("substringbefore_04") }
  @Test def test_substringbefore_05(): Unit = { runner2.runOneTest("substringbefore_05") }
  @Test def test_substringbefore_06(): Unit = { runner2.runOneTest("substringbefore_06") }
  @Test def test_substringbefore_07(): Unit = { runner2.runOneTest("substringbefore_07") }

  @Test def test_substringafter_01(): Unit = { runner2.runOneTest("substringafter_01") }
  @Test def test_substringafter_02(): Unit = { runner2.runOneTest("substringafter_02") }
  @Test def test_substringafter_03(): Unit = { runner2.runOneTest("substringafter_03") }
  @Test def test_substringafter_04(): Unit = { runner2.runOneTest("substringafter_04") }
  @Test def test_substringafter_05(): Unit = { runner2.runOneTest("substringafter_05") }
  @Test def test_substringafter_06(): Unit = { runner2.runOneTest("substringafter_06") }
  @Test def test_substringafter_07(): Unit = { runner2.runOneTest("substringafter_07") }

  @Test def test_yearfromdatetime_01(): Unit = { runner2.runOneTest("yearfromdatetime_01") }
  @Test def test_yearfromdatetime_02(): Unit = { runner2.runOneTest("yearfromdatetime_02") }
  @Test def test_yearfromdatetime_03(): Unit = { runner2.runOneTest("yearfromdatetime_03") }
  @Test def test_monthfromdatetime_01(): Unit = { runner2.runOneTest("monthfromdatetime_01") }
  @Test def test_monthfromdatetime_02(): Unit = { runner2.runOneTest("monthfromdatetime_02") }
  @Test def test_dayfromdatetime_01(): Unit = { runner2.runOneTest("dayfromdatetime_01") }
  @Test def test_dayfromdatetime_02(): Unit = { runner2.runOneTest("dayfromdatetime_02") }
  @Test def test_hoursfromdatetime_01(): Unit = { runner2.runOneTest("hoursfromdatetime_01") }
  @Test def test_hoursfromdatetime_02(): Unit = { runner2.runOneTest("hoursfromdatetime_02") }
  @Test def test_minutesfromdatetime_01(): Unit = { runner2.runOneTest("minutesfromdatetime_01") }
  @Test def test_minutesfromdatetime_02(): Unit = { runner2.runOneTest("minutesfromdatetime_02") }
  @Test def test_secondsfromdatetime_01(): Unit = { runner2.runOneTest("secondsfromdatetime_01") }
  @Test def test_secondsfromdatetime_02(): Unit = { runner2.runOneTest("secondsfromdatetime_02") }
  @Test def test_secondsfromdatetime_03(): Unit = { runner2.runOneTest("secondsfromdatetime_03") }
  @Test def test_timezonefromdatetime_01(): Unit = { runner2.runOneTest("timezonefromdatetime_01") }
  @Test def test_timezonefromdatetime_02(): Unit = { runner2.runOneTest("timezonefromdatetime_02") }

  @Test def test_xfromdatetime_01(): Unit = { runner2.runOneTest("xfromdatetime_01") }
  @Test def test_xfromdatetime_02(): Unit = { runner2.runOneTest("xfromdatetime_02") }
  @Test def test_xfromdatetime_03(): Unit = { runner2.runOneTest("xfromdatetime_03") }
  @Test def test_xfromdatetime_04(): Unit = { runner2.runOneTest("xfromdatetime_04") }

  @Test def test_yearfromdate_01(): Unit = { runner2.runOneTest("yearfromdate_01") }
  @Test def test_yearfromdate_02(): Unit = { runner2.runOneTest("yearfromdate_02") }
  @Test def test_yearfromdate_03(): Unit = { runner2.runOneTest("yearfromdate_03") }
  @Test def test_monthfromdate_01(): Unit = { runner2.runOneTest("monthfromdate_01") }
  @Test def test_monthfromdate_02(): Unit = { runner2.runOneTest("monthfromdate_02") }
  @Test def test_dayfromdate_01(): Unit = { runner2.runOneTest("dayfromdate_01") }
  @Test def test_dayfromdate_02(): Unit = { runner2.runOneTest("dayfromdate_02") }
  @Test def test_timezonefromdate_01(): Unit = { runner2.runOneTest("timezonefromdate_01") }
  @Test def test_timezonefromdate_02(): Unit = { runner2.runOneTest("timezonefromdate_02") }

  @Test def test_xfromdate_01(): Unit = { runner2.runOneTest("xfromdate_01") }
  @Test def test_xfromdate_02(): Unit = { runner2.runOneTest("xfromdate_02") }
  @Test def test_xfromdate_03(): Unit = { runner2.runOneTest("xfromdate_03") }

  @Test def test_hoursfromtime_01(): Unit = { runner2.runOneTest("hoursfromtime_01") }
  @Test def test_hoursfromtime_02(): Unit = { runner2.runOneTest("hoursfromtime_02") }
  @Test def test_minutesfromtime_01(): Unit = { runner2.runOneTest("minutesfromtime_01") }
  @Test def test_minutesfromtime_02(): Unit = { runner2.runOneTest("minutesfromtime_02") }
  @Test def test_secondsfromtime_01(): Unit = { runner2.runOneTest("secondsfromtime_01") }
  @Test def test_secondsfromtime_02(): Unit = { runner2.runOneTest("secondsfromtime_02") }
  @Test def test_timezonefromtime_01(): Unit = { runner2.runOneTest("timezonefromtime_01") }
  @Test def test_timezonefromtime_02(): Unit = { runner2.runOneTest("timezonefromtime_02") }
  @Test def test_timezonefromtime_03(): Unit = { runner2.runOneTest("timezonefromtime_03") }

  @Test def test_xfromtime_01(): Unit = { runner2.runOneTest("xfromtime_01") }
  @Test def test_xfromtime_02(): Unit = { runner2.runOneTest("xfromtime_02") }
  @Test def test_xfromtime_03(): Unit = { runner2.runOneTest("xfromtime_03") }

  @Test def test_fn_text_01(): Unit = { runner2.runOneTest("fn_text_01") }
  @Test def test_fn_text_02(): Unit = { runner2.runOneTest("fn_text_02") }

  @Test def test_ubyte_constructor_01(): Unit = { runner2.runOneTest("ubyte_constructor_01") }
  @Test def test_ubyte_constructor_02(): Unit = { runner2.runOneTest("ubyte_constructor_02") }
  @Test def test_ubyte_constructor_03(): Unit = { runner2.runOneTest("ubyte_constructor_03") }
  @Test def test_ubyte_constructor_04(): Unit = { runner2.runOneTest("ubyte_constructor_04") }

  @Test def test_uint_constructor_01(): Unit = { runner2.runOneTest("uint_constructor_01") }
  @Test def test_uint_constructor_02(): Unit = { runner2.runOneTest("uint_constructor_02") }
  @Test def test_uint_constructor_03(): Unit = { runner2.runOneTest("uint_constructor_03") }
  @Test def test_uint_constructor_04(): Unit = { runner2.runOneTest("uint_constructor_04") }

  @Test def test_nonNeg_constructor_01(): Unit = { runner2.runOneTest("nonNeg_constructor_01") }
  @Test def test_nonNeg_constructor_02(): Unit = { runner2.runOneTest("nonNeg_constructor_02") }
  @Test def test_nonNeg_constructor_03(): Unit = { runner2.runOneTest("nonNeg_constructor_03") }
  @Test def test_nonNeg_constructor_04(): Unit = { runner2.runOneTest("nonNeg_constructor_04") }

  @Test def test_byte_constructor_01(): Unit = { runner2.runOneTest("byte_constructor_01") }
  @Test def test_byte_constructor_02(): Unit = { runner2.runOneTest("byte_constructor_02") }
  @Test def test_byte_constructor_03(): Unit = { runner2.runOneTest("byte_constructor_03") }
  @Test def test_byte_constructor_04(): Unit = { runner2.runOneTest("byte_constructor_04") }

  @Test def test_hexBinary_constructor_01(): Unit = { runner2.runOneTest("hexBinary_constructor_01") }
  @Test def test_hexBinary_constructor_02(): Unit = { runner2.runOneTest("hexBinary_constructor_02") }
  @Test def test_hexBinary_constructor_03(): Unit = { runner2.runOneTest("hexBinary_constructor_03") }
  @Test def test_hexBinary_constructor_04(): Unit = { runner2.runOneTest("hexBinary_constructor_04") }

  @Test def test_dfdlHexBinary_constructor_01(): Unit = { runner2.runOneTest("dfdlHexBinary_constructor_01") }
  @Test def test_dfdlHexBinary_constructor_02(): Unit = { runner2.runOneTest("dfdlHexBinary_constructor_02") }
  @Test def test_dfdlHexBinary_constructor_03(): Unit = { runner2.runOneTest("dfdlHexBinary_constructor_03") }
  @Test def test_dfdlHexBinary_constructor_04(): Unit = { runner2.runOneTest("dfdlHexBinary_constructor_04") }
  @Test def test_dfdlHexBinary_constructor_05(): Unit = { runner2.runOneTest("dfdlHexBinary_constructor_05") }

  @Test def test_dfdlByte_constructor_01(): Unit = { runner2.runOneTest("dfdlByte_constructor_01") }
  @Test def test_dfdlByte_constructor_02(): Unit = { runner2.runOneTest("dfdlByte_constructor_02") }
  @Test def test_dfdlByte_constructor_03(): Unit = { runner2.runOneTest("dfdlByte_constructor_03") }
  @Test def test_dfdlByte_constructor_04(): Unit = { runner2.runOneTest("dfdlByte_constructor_04") }
  @Test def test_dfdlByte_constructor_05(): Unit = { runner2.runOneTest("dfdlByte_constructor_05") }
  @Test def test_dfdlByte_constructor_06(): Unit = { runner2.runOneTest("dfdlByte_constructor_06") }
  @Test def test_dfdlByte_constructor_07(): Unit = { runner2.runOneTest("dfdlByte_constructor_07") }
  @Test def test_dfdlByte_constructor_08(): Unit = { runner2.runOneTest("dfdlByte_constructor_08") }
  @Test def test_dfdlByte_constructor_09(): Unit = { runner2.runOneTest("dfdlByte_constructor_09") }
  @Test def test_dfdlByte_constructor_10(): Unit = { runner2.runOneTest("dfdlByte_constructor_10") }

  @Test def test_dfdlUByte_constructor_01(): Unit = { runner2.runOneTest("dfdlUByte_constructor_01") }
  @Test def test_dfdlUByte_constructor_02(): Unit = { runner2.runOneTest("dfdlUByte_constructor_02") }
  @Test def test_dfdlUByte_constructor_03(): Unit = { runner2.runOneTest("dfdlUByte_constructor_03") }
  @Test def test_dfdlUByte_constructor_04(): Unit = { runner2.runOneTest("dfdlUByte_constructor_04") }
  @Test def test_dfdlUByte_constructor_05(): Unit = { runner2.runOneTest("dfdlUByte_constructor_05") }
  @Test def test_dfdlUByte_constructor_06(): Unit = { runner2.runOneTest("dfdlUByte_constructor_06") }
  @Test def test_dfdlUByte_constructor_07(): Unit = { runner2.runOneTest("dfdlUByte_constructor_07") }
  @Test def test_dfdlUByte_constructor_08(): Unit = { runner2.runOneTest("dfdlUByte_constructor_08") }
  @Test def test_dfdlUByte_constructor_09(): Unit = { runner2.runOneTest("dfdlUByte_constructor_09") }
  @Test def test_dfdlUByte_constructor_11(): Unit = { runner2.runOneTest("dfdlUByte_constructor_11") }

  @Test def test_dfdlShort_constructor_01(): Unit = { runner2.runOneTest("dfdlShort_constructor_01") }
  @Test def test_dfdlShort_constructor_02(): Unit = { runner2.runOneTest("dfdlShort_constructor_02") }
  @Test def test_dfdlShort_constructor_03(): Unit = { runner2.runOneTest("dfdlShort_constructor_03") }
  @Test def test_dfdlShort_constructor_04(): Unit = { runner2.runOneTest("dfdlShort_constructor_04") }
  @Test def test_dfdlShort_constructor_05(): Unit = { runner2.runOneTest("dfdlShort_constructor_05") }
  @Test def test_dfdlShort_constructor_06(): Unit = { runner2.runOneTest("dfdlShort_constructor_06") }
  @Test def test_dfdlShort_constructor_07(): Unit = { runner2.runOneTest("dfdlShort_constructor_07") }
  @Test def test_dfdlShort_constructor_08(): Unit = { runner2.runOneTest("dfdlShort_constructor_08") }
  @Test def test_dfdlShort_constructor_09(): Unit = { runner2.runOneTest("dfdlShort_constructor_09") }
  @Test def test_dfdlShort_constructor_11(): Unit = { runner2.runOneTest("dfdlShort_constructor_11") }

  @Test def test_dfdlUShort_constructor_01(): Unit = { runner2.runOneTest("dfdlUShort_constructor_01") }
  @Test def test_dfdlUShort_constructor_02(): Unit = { runner2.runOneTest("dfdlUShort_constructor_02") }
  @Test def test_dfdlUShort_constructor_03(): Unit = { runner2.runOneTest("dfdlUShort_constructor_03") }
  @Test def test_dfdlUShort_constructor_04(): Unit = { runner2.runOneTest("dfdlUShort_constructor_04") }
  @Test def test_dfdlUShort_constructor_05(): Unit = { runner2.runOneTest("dfdlUShort_constructor_05") }
  @Test def test_dfdlUShort_constructor_06(): Unit = { runner2.runOneTest("dfdlUShort_constructor_06") }
  @Test def test_dfdlUShort_constructor_07(): Unit = { runner2.runOneTest("dfdlUShort_constructor_07") }
  @Test def test_dfdlUShort_constructor_08(): Unit = { runner2.runOneTest("dfdlUShort_constructor_08") }
  @Test def test_dfdlUShort_constructor_09(): Unit = { runner2.runOneTest("dfdlUShort_constructor_09") }
  @Test def test_dfdlUShort_constructor_11(): Unit = { runner2.runOneTest("dfdlUShort_constructor_11") }

  @Test def test_dfdlInt_constructor_01(): Unit = { runner2.runOneTest("dfdlInt_constructor_01") }
  @Test def test_dfdlInt_constructor_02(): Unit = { runner2.runOneTest("dfdlInt_constructor_02") }
  @Test def test_dfdlInt_constructor_03(): Unit = { runner2.runOneTest("dfdlInt_constructor_03") }
  @Test def test_dfdlInt_constructor_04(): Unit = { runner2.runOneTest("dfdlInt_constructor_04") }
  @Test def test_dfdlInt_constructor_05(): Unit = { runner2.runOneTest("dfdlInt_constructor_05") }
  @Test def test_dfdlInt_constructor_06(): Unit = { runner2.runOneTest("dfdlInt_constructor_06") }
  @Test def test_dfdlInt_constructor_07(): Unit = { runner2.runOneTest("dfdlInt_constructor_07") }
  @Test def test_dfdlInt_constructor_08(): Unit = { runner2.runOneTest("dfdlInt_constructor_08") }
  @Test def test_dfdlInt_constructor_09(): Unit = { runner2.runOneTest("dfdlInt_constructor_09") }
  @Test def test_dfdlInt_constructor_11(): Unit = { runner2.runOneTest("dfdlInt_constructor_11") }
  @Test def test_dfdlInt_constructor_12(): Unit = { runner2.runOneTest("dfdlInt_constructor_12") }
  @Test def test_dfdlInt_constructor_13(): Unit = { runner2.runOneTest("dfdlInt_constructor_13") }
  @Test def test_dfdlInt_constructor_14(): Unit = { runner2.runOneTest("dfdlInt_constructor_14") }

  @Test def test_dfdlUInt_constructor_01(): Unit = { runner2.runOneTest("dfdlUInt_constructor_01") }
  @Test def test_dfdlUInt_constructor_02(): Unit = { runner2.runOneTest("dfdlUInt_constructor_02") }
  @Test def test_dfdlUInt_constructor_03(): Unit = { runner2.runOneTest("dfdlUInt_constructor_03") }
  @Test def test_dfdlUInt_constructor_04(): Unit = { runner2.runOneTest("dfdlUInt_constructor_04") }
  @Test def test_dfdlUInt_constructor_05(): Unit = { runner2.runOneTest("dfdlUInt_constructor_05") }
  @Test def test_dfdlUInt_constructor_06(): Unit = { runner2.runOneTest("dfdlUInt_constructor_06") }
  @Test def test_dfdlUInt_constructor_07(): Unit = { runner2.runOneTest("dfdlUInt_constructor_07") }
  @Test def test_dfdlUInt_constructor_08(): Unit = { runner2.runOneTest("dfdlUInt_constructor_08") }
  @Test def test_dfdlUInt_constructor_09(): Unit = { runner2.runOneTest("dfdlUInt_constructor_09") }
  @Test def test_dfdlUInt_constructor_11(): Unit = { runner2.runOneTest("dfdlUInt_constructor_11") }
  @Test def test_dfdlUInt_constructor_12(): Unit = { runner2.runOneTest("dfdlUInt_constructor_12") }

  @Test def test_dfdlLong_constructor_01(): Unit = { runner2.runOneTest("dfdlLong_constructor_01") }
  @Test def test_dfdlLong_constructor_02(): Unit = { runner2.runOneTest("dfdlLong_constructor_02") }
  @Test def test_dfdlLong_constructor_03(): Unit = { runner2.runOneTest("dfdlLong_constructor_03") }
  @Test def test_dfdlLong_constructor_04(): Unit = { runner2.runOneTest("dfdlLong_constructor_04") }
  @Test def test_dfdlLong_constructor_05(): Unit = { runner2.runOneTest("dfdlLong_constructor_05") }
  @Test def test_dfdlLong_constructor_06(): Unit = { runner2.runOneTest("dfdlLong_constructor_06") }
  @Test def test_dfdlLong_constructor_07(): Unit = { runner2.runOneTest("dfdlLong_constructor_07") }
  @Test def test_dfdlLong_constructor_08(): Unit = { runner2.runOneTest("dfdlLong_constructor_08") }
  @Test def test_dfdlLong_constructor_09(): Unit = { runner2.runOneTest("dfdlLong_constructor_09") }
  @Test def test_dfdlLong_constructor_11(): Unit = { runner2.runOneTest("dfdlLong_constructor_11") }
  @Test def test_dfdlLong_constructor_12(): Unit = { runner2.runOneTest("dfdlLong_constructor_12") }

  @Test def test_dfdlULong_constructor_01(): Unit = { runner2.runOneTest("dfdlULong_constructor_01") }
  @Test def test_dfdlULong_constructor_02(): Unit = { runner2.runOneTest("dfdlULong_constructor_02") }
  @Test def test_dfdlULong_constructor_03(): Unit = { runner2.runOneTest("dfdlULong_constructor_03") }
  @Test def test_dfdlULong_constructor_04(): Unit = { runner2.runOneTest("dfdlULong_constructor_04") }
  @Test def test_dfdlULong_constructor_05(): Unit = { runner2.runOneTest("dfdlULong_constructor_05") }
  @Test def test_dfdlULong_constructor_06(): Unit = { runner2.runOneTest("dfdlULong_constructor_06") }
  @Test def test_dfdlULong_constructor_07(): Unit = { runner2.runOneTest("dfdlULong_constructor_07") }
  @Test def test_dfdlULong_constructor_08(): Unit = { runner2.runOneTest("dfdlULong_constructor_08") }
  @Test def test_dfdlULong_constructor_09(): Unit = { runner2.runOneTest("dfdlULong_constructor_09") }
  @Test def test_dfdlULong_constructor_11(): Unit = { runner2.runOneTest("dfdlULong_constructor_11") }
  @Test def test_dfdlULong_constructor_12(): Unit = { runner2.runOneTest("dfdlULong_constructor_12") }

  @Test def test_xsDateTime_constructor_06(): Unit = { runner2.runOneTest("xsDateTime_constructor_06") }
  @Test def test_xsDateTime_constructor_07(): Unit = { runner2.runOneTest("xsDateTime_constructor_07") }
  @Test def test_xsDateTime_constructor_08(): Unit = { runner2.runOneTest("xsDateTime_constructor_08") }
  @Test def test_xsDateTime_constructor_09(): Unit = { runner2.runOneTest("xsDateTime_constructor_09") }
  @Test def test_xsDateTime_constructor_10(): Unit = { runner2.runOneTest("xsDateTime_constructor_10") }
  @Test def test_date_constructor_05(): Unit = { runner2.runOneTest("date_constructor_05") }
  @Test def test_date_constructor_06(): Unit = { runner2.runOneTest("date_constructor_06") }
  @Test def test_date_constructor_07(): Unit = { runner2.runOneTest("date_constructor_07") }
  @Test def test_date_constructor_08(): Unit = { runner2.runOneTest("date_constructor_08") }
  @Test def test_time_constructor_05(): Unit = { runner2.runOneTest("time_constructor_05") }
  @Test def test_time_constructor_06(): Unit = { runner2.runOneTest("time_constructor_06") }
  @Test def test_time_constructor_07(): Unit = { runner2.runOneTest("time_constructor_07") }
  @Test def test_time_constructor_08(): Unit = { runner2.runOneTest("time_constructor_08") }
  @Test def test_time_constructor_09(): Unit = { runner2.runOneTest("time_constructor_09") }

  @Test def test_time_constructor_01(): Unit = { runner2.runOneTest("time_constructor_01") }
  @Test def test_time_constructor_02(): Unit = { runner2.runOneTest("time_constructor_02") }
  @Test def test_time_constructor_03(): Unit = { runner2.runOneTest("time_constructor_03") }
  @Test def test_time_constructor_04(): Unit = { runner2.runOneTest("time_constructor_04") }

  //DFDL-1124
  //@Test def test_date_constructor_01() { runner2.runOneTest("date_constructor_01") }
  @Test def test_date_constructor_02(): Unit = { runner2.runOneTest("date_constructor_02") }
  //@Test def test_date_constructor_02a() { runner2.runOneTest("date_constructor_02a") }
  @Test def test_date_constructor_03(): Unit = { runner2.runOneTest("date_constructor_03") }
  //@Test def test_date_constructor_03a() { runner2.runOneTest("date_constructor_03a") }
  @Test def test_date_constructor_04(): Unit = { runner2.runOneTest("date_constructor_04") }
  //@Test def test_nonNeg_constructor_02a() { runner2.runOneTest("nonNeg_constructor_02a") }

  @Test def test_xsDateTime_constructor_01(): Unit = { runner2.runOneTest("xsDateTime_constructor_01") }
  @Test def test_xsDateTime_constructor_02(): Unit = { runner2.runOneTest("xsDateTime_constructor_02") }
  //DFDL-1115
  //@Test def test_xsDateTime_constructor_03() { runner2.runOneTest("xsDateTime_constructor_03") }
  @Test def test_xsDateTime_constructor_04(): Unit = { runner2.runOneTest("xsDateTime_constructor_04") }
  @Test def test_xsDateTime_constructor_05(): Unit = { runner2.runOneTest("xsDateTime_constructor_05") }

  @Test def test_double_constructor_01(): Unit = { runner2.runOneTest("double_constructor_01") }
  @Test def test_double_constructor_02(): Unit = { runner2.runOneTest("double_constructor_02") }
  @Test def test_double_constructor_03(): Unit = { runner2.runOneTest("double_constructor_03") }
  @Test def test_double_constructor_04(): Unit = { runner2.runOneTest("double_constructor_04") }
  @Test def test_double_constructor_05(): Unit = { runner2.runOneTest("double_constructor_05") }
  @Test def test_double_constructor_06(): Unit = { runner2.runOneTest("double_constructor_06") }
  @Test def test_double_constructor_07(): Unit = { runner2.runOneTest("double_constructor_07") }

  @Test def test_float_constructor_01(): Unit = { runner2.runOneTest("float_constructor_01") }
  @Test def test_float_constructor_02(): Unit = { runner2.runOneTest("float_constructor_02") }
  @Test def test_float_constructor_03(): Unit = { runner2.runOneTest("float_constructor_03") }
  @Test def test_float_constructor_04(): Unit = { runner2.runOneTest("float_constructor_04") }

  @Test def test_decimal_constructor_01(): Unit = { runner2.runOneTest("decimal_constructor_01") }
  @Test def test_decimal_constructor_02(): Unit = { runner2.runOneTest("decimal_constructor_02") }
  @Test def test_decimal_constructor_03(): Unit = { runner2.runOneTest("decimal_constructor_03") }
  @Test def test_decimal_constructor_04(): Unit = { runner2.runOneTest("decimal_constructor_04") }
  @Test def test_decimal_constructor_05(): Unit = { runner2.runOneTest("decimal_constructor_05") }
  @Test def test_decimal_constructor_06(): Unit = { runner2.runOneTest("decimal_constructor_06") }

  @Test def test_short_constructor_01(): Unit = { runner2.runOneTest("short_constructor_01") }
  @Test def test_short_constructor_02(): Unit = { runner2.runOneTest("short_constructor_02") }
  @Test def test_short_constructor_03(): Unit = { runner2.runOneTest("short_constructor_03") }
  @Test def test_short_constructor_04(): Unit = { runner2.runOneTest("short_constructor_04") }
  @Test def test_short_constructor_05(): Unit = { runner2.runOneTest("short_constructor_05") }
  @Test def test_short_constructor_06(): Unit = { runner2.runOneTest("short_constructor_06") }

  @Test def test_ushort_constructor_01(): Unit = { runner2.runOneTest("ushort_constructor_01") }
  @Test def test_ushort_constructor_02(): Unit = { runner2.runOneTest("ushort_constructor_02") }
  @Test def test_ushort_constructor_03(): Unit = { runner2.runOneTest("ushort_constructor_03") }
  @Test def test_ushort_constructor_04(): Unit = { runner2.runOneTest("ushort_constructor_04") }
  @Test def test_ushort_constructor_05(): Unit = { runner2.runOneTest("ushort_constructor_05") }

  @Test def test_ulong_constructor_01(): Unit = { runner2.runOneTest("ulong_constructor_01") }
  @Test def test_ulong_constructor_02(): Unit = { runner2.runOneTest("ulong_constructor_02") }
  @Test def test_ulong_constructor_03(): Unit = { runner2.runOneTest("ulong_constructor_03") }
  @Test def test_ulong_constructor_04(): Unit = { runner2.runOneTest("ulong_constructor_04") }
  @Test def test_ulong_constructor_05(): Unit = { runner2.runOneTest("ulong_constructor_05") }
  @Test def test_ulong_constructor_06(): Unit = { runner2.runOneTest("ulong_constructor_06") }
  @Test def test_ulong_constructor_07(): Unit = { runner2.runOneTest("ulong_constructor_07") }

  @Test def test_long_constructor_01(): Unit = { runner2.runOneTest("long_constructor_01") }
  @Test def test_long_constructor_02(): Unit = { runner2.runOneTest("long_constructor_02") }
  @Test def test_long_constructor_03(): Unit = { runner2.runOneTest("long_constructor_03") }
  @Test def test_long_constructor_04(): Unit = { runner2.runOneTest("long_constructor_04") }

  @Test def test_int_constructor_01(): Unit = { runner2.runOneTest("int_constructor_01") }
  @Test def test_int_constructor_02(): Unit = { runner2.runOneTest("int_constructor_02") }
  @Test def test_int_constructor_03(): Unit = { runner2.runOneTest("int_constructor_03") }
  @Test def test_int_constructor_04(): Unit = { runner2.runOneTest("int_constructor_04") }

  @Test def test_fnDateTime_constructor_01(): Unit = { runner2.runOneTest("fnDateTime_constructor_01") }
  @Test def test_fnDateTime_constructor_02(): Unit = { runner2.runOneTest("fnDateTime_constructor_02") }
  @Test def test_fnDateTime_constructor_03(): Unit = { runner2.runOneTest("fnDateTime_constructor_03") }
  @Test def test_fnDateTime_constructor_04(): Unit = { runner2.runOneTest("fnDateTime_constructor_04") }
  @Test def test_fnDateTime_constructor_05(): Unit = { runner2.runOneTest("fnDateTime_constructor_05") }
  @Test def test_fnDateTime_constructor_06(): Unit = { runner2.runOneTest("fnDateTime_constructor_06") }
  @Test def test_fnDateTime_constructor_07(): Unit = { runner2.runOneTest("fnDateTime_constructor_07") }
  @Test def test_fnDateTime_constructor_08(): Unit = { runner2.runOneTest("fnDateTime_constructor_08") }
  @Test def test_fnDateTime_constructor_09(): Unit = { runner2.runOneTest("fnDateTime_constructor_09") }
  @Test def test_fnDateTime_constructor_10(): Unit = { runner2.runOneTest("fnDateTime_constructor_10") }

  @Test def test_integer_constructor_01(): Unit = { runner2.runOneTest("integer_constructor_01") }
  @Test def test_integer_constructor_02(): Unit = { runner2.runOneTest("integer_constructor_02") }
  @Test def test_integer_constructor_03(): Unit = { runner2.runOneTest("integer_constructor_03") }
  @Test def test_integer_constructor_04(): Unit = { runner2.runOneTest("integer_constructor_04") }
  @Test def test_integer_constructor_05(): Unit = { runner2.runOneTest("integer_constructor_05") }
  @Test def test_integer_constructor_06(): Unit = { runner2.runOneTest("integer_constructor_06") }
  @Test def test_integer_constructor_07(): Unit = { runner2.runOneTest("integer_constructor_07") }

  @Test def test_testBit_0(): Unit = { runner2.runOneTest("testBit_0") }
  @Test def test_testBit_1(): Unit = { runner2.runOneTest("testBit_1") }
  @Test def test_testBit_2(): Unit = { runner2.runOneTest("testBit_2") }
  @Test def test_testBit_3(): Unit = { runner2.runOneTest("testBit_3") }
  @Test def test_testBit_4(): Unit = { runner2.runOneTest("testBit_4") }
  @Test def test_testBit_5(): Unit = { runner2.runOneTest("testBit_5") }

  @Test def test_stringLiteralFromString_obsolete(): Unit = { runner2.runOneTest("stringLiteralFromString_obsolete") }
  @Test def test_containsEntity_obsolete(): Unit = { runner2.runOneTest("containsEntity_obsolete") }

  @Test def test_encodeDFDLEntities_0(): Unit = { runner2.runOneTest("encodeDFDLEntities_0") }
  @Test def test_encodeDFDLEntities_1(): Unit = { runner2.runOneTest("encodeDFDLEntities_1") }
  @Test def test_encodeDFDLEntities_2(): Unit = { runner2.runOneTest("encodeDFDLEntities_2") }
  @Test def test_encodeDFDLEntities_3(): Unit = { runner2.runOneTest("encodeDFDLEntities_3") }
  @Test def test_encodeDFDLEntities_4(): Unit = { runner2.runOneTest("encodeDFDLEntities_4") }

  @Test def test_setBits_0(): Unit = { runner2.runOneTest("setBits_0") }
  @Test def test_setBits_1(): Unit = { runner2.runOneTest("setBits_1") }
  @Test def test_setBits_2(): Unit = { runner2.runOneTest("setBits_2") }

  @Test def test_containsDFDLEntities_0(): Unit = { runner2.runOneTest("containsDFDLEntities_0") }
  @Test def test_containsDFDLEntities_1(): Unit = { runner2.runOneTest("containsDFDLEntities_1") }
  @Test def test_containsDFDLEntities_2(): Unit = { runner2.runOneTest("containsDFDLEntities_2") }
  @Test def test_containsDFDLEntities_3(): Unit = { runner2.runOneTest("containsDFDLEntities_3") }
  @Test def test_containsDFDLEntities_4(): Unit = { runner2.runOneTest("containsDFDLEntities_4") }

  //DFDL-1118
  //@Test def test_more_count_0() { runner2.runOneTest("more_count_0") }
  //@Test def test_more_count_1() { runner2.runOneTest("more_count_1") }
  @Test def test_more_count_1b(): Unit = { runner2.runOneTest("more_count_1b") }
  //@Test def test_more_count_1b_2() { runner2.runOneTest("more_count_1b_2") }
  //@Test def test_more_count_2() { runner2.runOneTest("more_count_2") }
  @Test def test_more_count_3(): Unit = { runner2.runOneTest("more_count_3") }
  @Test def test_more_count_3b(): Unit = { runner2.runOneTest("more_count_3b") }

  @Test def test_more_count_4(): Unit = { runner2.runOneTest("more_count_4") }

  @Test def test_valueLength_0(): Unit = { runner2.runOneTest("valueLength_0") }
  @Test def test_valueLength_1(): Unit = { runner2.runOneTest("valueLength_1") }
  // DFDL-1516:dfdl:contentLength & dfdl:valueLength specifying lengthUnits 'characters' and variable-width encodings
  @Test def test_valueLength_2(): Unit = { runner2.runOneTest("valueLength_2") }
  @Test def test_valueLength_3(): Unit = { runner2.runOneTest("valueLength_3") }
  @Test def test_valueLength_4(): Unit = { runner2.runOneTest("valueLength_4") }
  @Test def test_valueLength_5(): Unit = { runner2.runOneTest("valueLength_5") }
  @Test def test_valueLength_6(): Unit = { runner2.runOneTest("valueLength_6") }
  @Test def test_valueLength_sde(): Unit = { runner2.runOneTest("valueLength_sde") }
  @Test def test_valueLength_unparse_0(): Unit = { runner2.runOneTest("valueLength_unparse_0") }
  // DFDL-1516:dfdl:contentLength & dfdl:valueLength specifying lengthUnits 'characters' and variable-width encodings
  @Test def test_valueLength_unparse_1(): Unit = { runner2.runOneTest("valueLength_unparse_1") }
  @Test def test_valueLength_unparse_2(): Unit = { runner2.runOneTest("valueLength_unparse_2") }
  @Test def test_valueLength_unparse_3(): Unit = { runner2.runOneTest("valueLength_unparse_3") }
  @Test def test_valueLength_unparse_4(): Unit = { runner2.runOneTest("valueLength_unparse_4") }

  @Test def test_contentLength_0(): Unit = { runner2.runOneTest("contentLength_0") }
  @Test def test_contentLength_1(): Unit = { runner2.runOneTest("contentLength_1") }
  @Test def test_contentLength_2(): Unit = { runner2.runOneTest("contentLength_2") }

  @Test def test_valueContentLength1(): Unit = { runner2.runOneTest("valueContentLength1") }
  @Test def test_valueContentLength2(): Unit = { runner2.runOneTest("valueContentLength2") }

  @Test def test_fn_not_declared(): Unit = { runner2b.runOneTest("fn_not_declared") }
  @Test def test_fn_not_declared_2(): Unit = { runner2b.runOneTest("fn_not_declared_2") }

  // DFDL-313
  // Verified that we do get an error regarding an improperly formatted
  // DFDL expression.  This test needs its own file since it fails at the
  // schema loading (SAXParse) level and would cause other tests within
  // the same file to fail.
  //
  @Test def test_no_closing_brace(): Unit = { runner3.runOneTest("no_closing_brace") } // no closing } for expression

  @Test def test_valueLengthPair1(): Unit = { runner5.runOneTest("valueLengthPair1") }
  @Test def test_valueLengthPair2(): Unit = { runner5.runOneTest("valueLengthPair2") }
  @Test def test_valueLengthPair3(): Unit = { runner5.runOneTest("valueLengthPair3") }
  @Test def test_valueLengthAndOccurs1(): Unit = { runner5.runOneTest("valueLengthAndOccurs1") }

  // Added from scala-new for DFDL-1198
  @Test def test_array_index_oob_01(): Unit = { runner.runOneTest("array_index_oob_01") }
  @Test def test_array_index_oob_02(): Unit = { runner.runOneTest("array_index_oob_02") }
  @Test def test_array_index_oob_03(): Unit = { runner.runOneTest("array_index_oob_03") }
  @Test def test_array_index_oob_04(): Unit = { runner.runOneTest("array_index_oob_04") }
  @Test def test_array_index_oob_05(): Unit = { runner.runOneTest("array_index_oob_05") }

  // Added from scala-new for DFDL-1660
  @Test def test_array_index_relative_path_subexpression_01(): Unit = { runner.runOneTest("array_index_relative_path_subexpression_01") }

  //DFDL-1702
  @Test def test_mathPow01(): Unit = { runner2.runOneTest("mathPow01") }
  @Test def test_mathPow02(): Unit = { runner2.runOneTest("mathPow02") }
  @Test def test_mathPow03(): Unit = { runner2.runOneTest("mathPow03") }
  @Test def test_mathPow04(): Unit = { runner2.runOneTest("mathPow04") }
  @Test def test_mathPow05(): Unit = { runner2.runOneTest("mathPow05") }
  @Test def test_mathPow06(): Unit = { runner2.runOneTest("mathPow06") }
  @Test def test_mathPow07(): Unit = { runner2.runOneTest("mathPow07") }
  @Test def test_mathPow08(): Unit = { runner2.runOneTest("mathPow08") }
  @Test def test_mathPow09(): Unit = { runner2.runOneTest("mathPow09") }
  @Test def test_mathPow10(): Unit = { runner2.runOneTest("mathPow10") }
  @Test def test_mathPow11(): Unit = { runner2.runOneTest("mathPow11") }
  @Test def test_mathPow12(): Unit = { runner2.runOneTest("mathPow12") }
  @Test def test_mathPow13(): Unit = { runner2.runOneTest("mathPow13") }
  @Test def test_mathPow14(): Unit = { runner2.runOneTest("mathPow14") }
  @Test def test_mathPow15(): Unit = { runner2.runOneTest("mathPow15") }
  @Test def test_mathPow16(): Unit = { runner2.runOneTest("mathPow16") }
  @Test def test_mathPow17(): Unit = { runner2.runOneTest("mathPow17") }
  @Test def test_mathPow18(): Unit = { runner2.runOneTest("mathPow18") }
  @Test def test_mathPow19(): Unit = { runner2.runOneTest("mathPow19") }
  @Test def test_mathPow20(): Unit = { runner2.runOneTest("mathPow20") }
  @Test def test_mathPow21(): Unit = { runner2.runOneTest("mathPow21") }
  @Test def test_mathPow22(): Unit = { runner2.runOneTest("mathPow22") }
  @Test def test_mathPow23(): Unit = { runner2.runOneTest("mathPow23") }
  @Test def test_mathPow24(): Unit = { runner2.runOneTest("mathPow24") }
  @Test def test_mathPow25(): Unit = { runner2.runOneTest("mathPow25") }
  @Test def test_mathPow26(): Unit = { runner2.runOneTest("mathPow26") }
  @Test def test_mathPow27(): Unit = { runner2.runOneTest("mathPow27") }
  @Test def test_mathPow28(): Unit = { runner2.runOneTest("mathPow28") }
  @Test def test_mathPow29(): Unit = { runner2.runOneTest("mathPow29") }
  @Test def test_mathPow30(): Unit = { runner2.runOneTest("mathPow30") }
  @Test def test_mathPow31(): Unit = { runner2.runOneTest("mathPow31") }
  @Test def test_mathPow32(): Unit = { runner2.runOneTest("mathPow32") }
  @Test def test_mathPow33(): Unit = { runner2.runOneTest("mathPow33") }
  @Test def test_mathPow34(): Unit = { runner2.runOneTest("mathPow34") }

  // DFDL-1771
  @Test def test_expr_path_past_root1(): Unit = { runner7.runOneTest("test_expr_path_past_root1") }

  // DFDL-1804
  @Test def test_traceComplex(): Unit = { runner7.runOneTest("traceComplex") }

  //DFDL-1076
  @Test def test_nilled_01(): Unit = { runner2.runOneTest("nilled_01") }

  // DFDL-1617 - should detect errors due to query-style expressions
  @Test def test_query_style_01(): Unit = { runner6.runOneTest("query_style_01") }
  @Test def test_query_style_02(): Unit = { runner6.runOneTest("query_style_02") }

  //DFDL-1221
  @Test def test_beyondRoot_01(): Unit = { runner.runOneTest("beyondRoot_01") }

  //DFDL-711
  @Test def test_short_parent_axis_01(): Unit = { runner.runOneTest("short_parent_axis_01") }

  @Test def test_element_long_form_whitespace(): Unit = { runner.runOneTest("element_long_form_whitespace") }

  @Test def test_DoubleFromRawLong(): Unit = { runner2.runOneTest("DoubleFromRawLong") }
  @Test def test_DoubleToRawLong(): Unit = { runner2.runOneTest("DoubleToRawLong") }

  @Test def test_unused_path_no_context_01(): Unit = { runner7.runOneTest("unused_path_no_context_01") }
}
