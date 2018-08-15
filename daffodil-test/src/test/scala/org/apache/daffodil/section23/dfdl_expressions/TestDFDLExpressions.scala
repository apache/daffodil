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

  val runner7 = Runner(testDir, "expressions2.tdml", compileAllTopLevel = true)

  @AfterClass def shutDown() {
    runner4.reset
    runner.reset
    runnerNV.reset
    runner_fun.reset
    runner2.reset
    runner2_utf8.reset
    runner2b.reset
    runner3.reset
    runner5.reset
    runner7.reset
  }
}

class TestDFDLExpressions {
  import TestDFDLExpressions._

  @Test def test_variableRefError { runner4.runOneTest("variableRefError") }

  @Test def test_byteOrderExpr1 { runner4.runOneTest("byteOrderExpr1") }
  @Test def test_byteOrderExpr1b { runner4.runOneTest("byteOrderExpr1b") }
  @Test def test_byteOrderExpr2 { runner4.runOneTest("byteOrderExpr2") }
  @Test def test_byteOrderExpr2b { runner4.runOneTest("byteOrderExpr2b") }
  @Test def test_byteOrderExpr3 { runner4.runOneTest("byteOrderExpr3") }
  @Test def test_byteOrderExpr4 { runner4.runOneTest("byteOrderExpr4") }
  @Test def test_byteOrderExpr5 { runner4.runOneTest("byteOrderExpr5") }
  @Test def test_byteOrderExpr6 { runner4.runOneTest("byteOrderExpr6") }
  @Test def test_byteOrderExpr7 { runner4.runOneTest("byteOrderExpr7") }
  @Test def test_byteOrderExpr7b { runner4.runOneTest("byteOrderExpr7b") }
  @Test def test_byteOrderExpr7c { runner4.runOneTest("byteOrderExpr7c") }
  @Test def test_byteOrderExpr8 { runner4.runOneTest("byteOrderExpr8") }
  @Test def test_byteOrderExpr9 { runner4.runOneTest("byteOrderExpr9") }

  //DFDL-1111
  //@Test def test_diagnostics_01() { runner.runOneTest("diagnostics_01") }
  //@Test def test_diagnostics_02() { runner.runOneTest("diagnostics_02") }
  //@Test def test_diagnostics_03() { runner.runOneTest("diagnostics_03") }

  //DFDL-1221
  //@Test def test_beyondRoot_01() { runner.runOneTest("beyondRoot_01") }

  @Test def test_sequenceReturned_01() { runner.runOneTest("sequenceReturned_01") }
  @Test def test_sequenceReturned_02() { runner.runOneTest("sequenceReturned_02") }
  @Test def test_sequenceReturned_03() { runner.runOneTest("sequenceReturned_03") }
  @Test def test_longPath_01() { runner.runOneTest("longPath_01") }
  @Test def test_longPath_02() { runner.runOneTest("longPath_02") }

  @Test def test_hiddenDataExpression() { runner.runOneTest("hiddenDataExpression") }
  @Test def test_hiddenDataExpression2() { runner.runOneTest("hiddenDataExpression2") }

  @Test def test_arrayIndexOutOfBounds_01() { runner.runOneTest("arrayIndexOutOfBounds_01") }
  @Test def test_arrayIndexOutOfBounds_02() { runner.runOneTest("arrayIndexOutOfBounds_02") }
  @Test def test_arrayIndexOutOfBounds_03() { runner.runOneTest("arrayIndexOutOfBounds_03") }

  // TODO: TBD DFDL-TICKET, should statically tell this is an invalid index (-1)
  //@Test def test_arrayIndexOutOfBounds_04() { runner.runOneTest("arrayIndexOutOfBounds_04") }
  @Test def test_arrayIndexOutOfBounds_05() { runner.runOneTest("arrayIndexOutOfBounds_05") }

  @Test def test_asterisk_01() { runner.runOneTest("asterisk_01") }
  @Test def test_asterisk_02() { runner.runOneTest("asterisk_02") }
  @Test def test_asterisk_03() { runner.runOneTest("asterisk_03") }

  //DFDL-1146
  //@Test def test_attribute_axis_01() { runner.runOneTest("attribute_axis_01") }
  //@Test def test_attribute_axis_02() { runner.runOneTest("attribute_axis_02") }
  //@Test def test_attribute_axis_03() { runner.runOneTest("attribute_axis_03") }

  @Test def test_comparison_operators_01() { runner.runOneTest("comparison_operators_01") }
  @Test def test_comparison_operators_02() { runner.runOneTest("comparison_operators_02") }
  @Test def test_comparison_operators_03() { runner.runOneTest("comparison_operators_03") }
  @Test def test_comparison_operators_04() { runner.runOneTest("comparison_operators_04") }
  @Test def test_comparison_operators_05() { runner.runOneTest("comparison_operators_05") }
  @Test def test_comparison_operators_06() { runner.runOneTest("comparison_operators_06") }
  @Test def test_comparison_operators_07() { runner.runOneTest("comparison_operators_07") }
  @Test def test_comparison_operators_08() { runner.runOneTest("comparison_operators_08") }
  @Test def test_comparison_operators_09() { runner.runOneTest("comparison_operators_09") }
  @Test def test_comparison_operators_10() { runner.runOneTest("comparison_operators_10") }
  @Test def test_comparison_operators_11() { runner.runOneTest("comparison_operators_11") }
  @Test def test_comparison_operators_12() { runner.runOneTest("comparison_operators_12") }
  @Test def test_comparison_operators_13() { runner.runOneTest("comparison_operators_13") }
  @Test def test_comparison_operators_14() { runner.runOneTest("comparison_operators_14") }
  @Test def test_comparison_operators_15() { runner.runOneTest("comparison_operators_15") }
  @Test def test_comparison_operators_16() { runner.runOneTest("comparison_operators_16") }
  @Test def test_comparison_operators_17() { runner.runOneTest("comparison_operators_17") }
  @Test def test_comparison_operators_18() { runner.runOneTest("comparison_operators_18") }
  @Test def test_comparison_operators_19() { runner.runOneTest("comparison_operators_19") }
  @Test def test_comparison_operators_20() { runner.runOneTest("comparison_operators_20") }
  @Test def test_comparison_operators_21() { runner.runOneTest("comparison_operators_21") }
  @Test def test_comparison_operators_22() { runner.runOneTest("comparison_operators_22") }
  @Test def test_comparison_operators_23() { runner.runOneTest("comparison_operators_23") }
  @Test def test_comparison_operators_24() { runner.runOneTest("comparison_operators_24") }
  @Test def test_comparison_operators_25() { runner.runOneTest("comparison_operators_25") }
  @Test def test_comparison_operators_26() { runner.runOneTest("comparison_operators_26") }
  @Test def test_comparison_operators_27() { runner.runOneTest("comparison_operators_27") }
  @Test def test_comparison_operators_28() { runner.runOneTest("comparison_operators_28") }

  @Test def test_comparison_operators_29() { runner.runOneTest("comparison_operators_29") }
  @Test def test_comparison_operators_30() { runner.runOneTest("comparison_operators_30") }
  @Test def test_comparison_operators_31() { runner.runOneTest("comparison_operators_31") }
  @Test def test_comparison_operators_32() { runner.runOneTest("comparison_operators_32") }
  @Test def test_comparison_operators_33() { runner.runOneTest("comparison_operators_33") }
  @Test def test_comparison_operators_34() { runner.runOneTest("comparison_operators_34") }
  @Test def test_comparison_operators_35() { runner.runOneTest("comparison_operators_35") }
  @Test def test_comparison_operators_36() { runner.runOneTest("comparison_operators_36") }
  @Test def test_comparison_operators_37() { runner.runOneTest("comparison_operators_37") }
  @Test def test_comparison_operators_38() { runner.runOneTest("comparison_operators_38") }
  @Test def test_comparison_operators_39() { runner.runOneTest("comparison_operators_39") }
  @Test def test_comparison_operators_40() { runner.runOneTest("comparison_operators_40") }
  @Test def test_comparison_operators_41() { runner.runOneTest("comparison_operators_41") }
  @Test def test_comparison_operators_42() { runner.runOneTest("comparison_operators_42") }
  @Test def test_comparison_operators_43() { runner.runOneTest("comparison_operators_43") }
  @Test def test_comparison_operators_44() { runner.runOneTest("comparison_operators_44") }
  @Test def test_comparison_operators_45() { runner.runOneTest("comparison_operators_45") }
  @Test def test_comparison_operators_46() { runner.runOneTest("comparison_operators_46") }

  // DAFFODIL-1986
  // @Test def test_comparison_operators_46a() { runner.runOneTest("comparison_operators_46a") }

  // from XPath Spec Sec 10.4.6.1 Examples
  @Test def test_comparison_operators_47() { runner.runOneTest("comparison_operators_47") }
  @Test def test_comparison_operators_48() { runner.runOneTest("comparison_operators_48") }
  @Test def test_comparison_operators_49() { runner.runOneTest("comparison_operators_49") }
  @Test def test_comparison_operators_50() { runner.runOneTest("comparison_operators_50") }
  @Test def test_comparison_operators_51() { runner.runOneTest("comparison_operators_51") }
  @Test def test_comparison_operators_52() { runner.runOneTest("comparison_operators_52") }
  @Test def test_comparison_operators_53() { runner.runOneTest("comparison_operators_53") }

  @Test def test_comparison_operators_54() { runner.runOneTest("comparison_operators_54") }
  @Test def test_comparison_operators_55() { runner.runOneTest("comparison_operators_55") }
  @Test def test_comparison_operators_56() { runner.runOneTest("comparison_operators_56") }
  @Test def test_comparison_operators_57() { runner.runOneTest("comparison_operators_57") }
  @Test def test_comparison_operators_58() { runner.runOneTest("comparison_operators_58") }
  @Test def test_comparison_operators_59() { runner.runOneTest("comparison_operators_59") }
  @Test def test_comparison_operators_60() { runner.runOneTest("comparison_operators_60") }
  @Test def test_comparison_operators_61() { runner.runOneTest("comparison_operators_61") }
  @Test def test_comparison_operators_62() { runner.runOneTest("comparison_operators_62") }
  @Test def test_comparison_operators_63() { runner.runOneTest("comparison_operators_63") }
  @Test def test_comparison_operators_64() { runner.runOneTest("comparison_operators_64") }
  @Test def test_comparison_operators_65() { runner.runOneTest("comparison_operators_65") }
  @Test def test_comparison_operators_66() { runner.runOneTest("comparison_operators_66") }
  @Test def test_comparison_operators_67() { runner.runOneTest("comparison_operators_67") }
  @Test def test_comparison_operators_68() { runner.runOneTest("comparison_operators_68") }
  @Test def test_comparison_operators_69() { runner.runOneTest("comparison_operators_69") }
  @Test def test_comparison_operators_70() { runner.runOneTest("comparison_operators_70") }
  @Test def test_comparison_operators_71() { runner.runOneTest("comparison_operators_71") }
  @Test def test_comparison_operators_72() { runner.runOneTest("comparison_operators_72") }
  @Test def test_comparison_operators_73() { runner.runOneTest("comparison_operators_73") }
  @Test def test_comparison_operators_74() { runner.runOneTest("comparison_operators_74") }
  @Test def test_comparison_operators_75() { runner.runOneTest("comparison_operators_75") }
  @Test def test_comparison_operators_76() { runner.runOneTest("comparison_operators_76") }
  @Test def test_comparison_operators_77() { runner.runOneTest("comparison_operators_77") }
  @Test def test_comparison_operators_78() { runner.runOneTest("comparison_operators_78") }
  @Test def test_comparison_operators_79() { runner.runOneTest("comparison_operators_79") }
  @Test def test_comparison_operators_80() { runner.runOneTest("comparison_operators_80") }
  @Test def test_comparison_operators_81() { runner.runOneTest("comparison_operators_81") }
  @Test def test_comparison_operators_82() { runner.runOneTest("comparison_operators_82") }
  @Test def test_comparison_operators_83() { runner.runOneTest("comparison_operators_83") }

  @Test def test_regexLookahead() { runnerNV.runOneTest("regexLookahead") }
  @Test def test_regexLookaheadFail() { runnerNV.runOneTest("regexLookaheadFail") }
  @Test def test_regexLookaheadFail2() { runnerNV.runOneTest("regexLookaheadFail2") }
  //  @Test def test_regexCompatFail() { runner.runOneTest("regexCompatFail") }

  @Test def test_expressionRules01() { runner.runOneTest("expressionRules01") }
  @Test def test_expressionRules02() { runner.runOneTest("expressionRules02") }
  @Test def test_expressionRules03() { runner.runOneTest("expressionRules03") }
  @Test def test_expressionRules04() { runner.runOneTest("expressionRules04") }
  @Test def test_expressionRules05() { runner.runOneTest("expressionRules05") }
  @Test def test_expressionRules06() { runner.runOneTest("expressionRules06") }

  // uses lengthUnits bytes with utf-8 and lengthKind Explicit
  // @Test def test_lke3_rel() { runner.runOneTest("lke3_rel") }

  @Test def test_lke1_rel() { runner.runOneTest("lke1_rel") }
  @Test def test_lke1_abs() { runner.runOneTest("lke1_abs") }
  @Test def test_ocke1() { runner.runOneTest("ocke1") }
  @Test def test_ocke2() { runner.runOneTest("ocke2") }
  @Test def test_ArrayOptElem_01() { runner.runOneTest("ArrayOptElem_01") }
  @Test def test_lke2_rel() { runner.runOneTest("lke2_rel") }
  @Test def test_expression_type_error1() { runner.runOneTest("expression_type_error1") }
  // DFDL-1044
  // @Test def test_expression_type_error2() { runner.runOneTest("expression_type_error2") }
  @Test def test_expression_type_error3() { runner.runOneTest("expression_type_error3") }
  @Test def test_expression_type_error4() { runner.runOneTest("expression_type_error4") }
  @Test def test_expression_unknown_prefix() { runner.runOneTest("expression_unknown_prefix") }
  @Test def test_ocke_rel() { runner.runOneTest("ocke_rel") }
  @Test def test_ocke_rel2() { runner.runOneTest("ocke_rel2") }
  @Test def test_ocke_rel3() { runner.runOneTest("ocke_rel3") }
  @Test def test_ocke_rel4() { runner.runOneTest("ocke_rel4") }
  @Test def test_internal_space_preserved() { runner.runOneTest("internal_space_preserved") }
  @Test def test_internal_space_preserved2() { runner.runOneTest("internal_space_preserved2") }
  @Test def test_internal_space_preserved3a() { runner.runOneTest("internal_space_preserved3a") }
  @Test def test_internal_space_preserved3b() { runner.runOneTest("internal_space_preserved3b") }
  @Test def test_internal_space_not_preserved1() { runner.runOneTest("internal_space_not_preserved1") }

  //DFDL-1287
  //@Test def test_internal_space_preserved4() { runner.runOneTest("internal_space_preserved4") }
  //@Test def test_internal_space_not_preserved2() { runner.runOneTest("internal_space_not_preserved2") }

  @Test def test_whitespace_expression() { runner.runOneTest("whitespace_expression") }
  @Test def test_whitespace_expression2() { runner.runOneTest("whitespace_expression2") }

  @Test def test_expresion_bad_path_to_element() { runner.runOneTest("expresion_bad_path_to_element") }
  @Test def test_ArrayOptElem_02() { runner.runOneTest("ArrayOptElem_02") }

  //DFDL-1035 - tests need better diagnostic
  //@Test def test_dfdlCheckConstraints() { runner.runOneTest("dfdlCheckConstraints") }
  //@Test def test_dfdlCheckConstraints2() { runner.runOneTest("dfdlCheckConstraints2") }

  // DFDL-1043
  // @Test def test_checkConstraintsComplexTypeFails() { runner.runOneTest("checkConstraintsComplexTypeFails") }

  @Test def test_nonFunctionIsDetected() = { runnerNV.runOneTest("nonFunctionIsDetected") }
  @Test def test_constantFunction1() { runnerNV.runOneTest("constantFunction1") }
  @Test def test_dfdlPosition1() { runnerNV.runOneTest("dfdlPosition1") }
  @Test def test_dfdlPosition2() { runnerNV.runOneTest("dfdlPosition2") }
  @Test def test_dfdlPosition3() { runnerNV.runOneTest("dfdlPosition3") }
  @Test def test_dfdlPosition4() { runnerNV.runOneTest("dfdlPosition4") }
  @Test def test_dfdlPosition5() { runnerNV.runOneTest("dfdlPosition5") }

  @Test def test_repeatFlags1() { runner.runOneTest("repeatFlags1") }
  @Test def test_repeatFlags2() { runner.runOneTest("repeatFlags2") }
  @Test def test_repeatFlags3() { runner.runOneTest("repeatFlags3") }
  @Test def test_repeatFlags4() { runner.runOneTest("repeatFlags4") }
  @Test def test_repeatFlags5() { runner.runOneTest("repeatFlags5") }

  @Test def test_repeatBitFlags1() { runner.runOneTest("repeatBitFlags1") }
  @Test def test_repeatBitFlags2() { runner.runOneTest("repeatBitFlags2") }
  @Test def test_repeatBitFlags3() { runner.runOneTest("repeatBitFlags3") }
  @Test def test_repeatBitFlags4() { runner.runOneTest("repeatBitFlags4") }
  @Test def test_repeatBitFlags5() { runner.runOneTest("repeatBitFlags5") }
  @Test def test_repeatBitFlags6() { runner.runOneTest("repeatBitFlags6") }

  @Test def test_invalid_enum_1() { runner.runOneTest("invalid_enum_1") }
  @Test def test_invalid_enum_2() { runner.runOneTest("invalid_enum_2") }
  @Test def test_invalid_enum_3() { runner.runOneTest("invalid_enum_3") }

  // Test removed including TDML for it. DPath no longer will even execution expressions that have
  // type errors, and type errors at runtime cause SDE so you can't if-then-else them into
  // some usable thing.
  // @Test def test_trueFalseTypeError() { runner.runOneTest("trueFalseTypeError") }
  @Test def test_trueFalseTypeCorrect() { runner.runOneTest("trueFalseTypeCorrect") }

  @Test def test_predicate_01() { runner.runOneTest("predicate_01") }
  //DFDL-1164
  //@Test def test_predicate_02() { runner.runOneTest("predicate_02") }
  //@Test def test_predicate_03() { runner.runOneTest("predicate_03") }
  @Test def test_predicate_04() { runner.runOneTest("predicate_04") }
  @Test def test_predicate_05() { runner.runOneTest("predicate_05") }

  @Test def test_sequential_and_01() { runner.runOneTest("sequential_and_01") }
  @Test def test_sequential_and_02() { runner.runOneTest("sequential_and_02") }
  @Test def test_sequential_and_03() { runner.runOneTest("sequential_and_03") }
  @Test def test_sequential_and_04() { runner.runOneTest("sequential_and_04") }
  @Test def test_sequential_and_05() { runner.runOneTest("sequential_and_05") }
  @Test def test_sequential_or_01() { runner.runOneTest("sequential_or_01") }
  @Test def test_sequential_or_02() { runner.runOneTest("sequential_or_02") }
  @Test def test_sequential_or_03() { runner.runOneTest("sequential_or_03") }
  @Test def test_sequential_or_04() { runner.runOneTest("sequential_or_04") }
  @Test def test_sequential_or_05() { runner.runOneTest("sequential_or_05") }
  @Test def test_sequential_and_or_01() { runner.runOneTest("sequential_and_or_01") }
  @Test def test_sequential_and_or_02() { runner.runOneTest("sequential_and_or_02") }
  @Test def test_sequential_and_or_03() { runner.runOneTest("sequential_and_or_03") }
  @Test def test_sequential_and_or_04() { runner.runOneTest("sequential_and_or_04") }
  @Test def test_sequential_and_or_05() { runner.runOneTest("sequential_and_or_05") }
  @Test def test_sequential_and_or_06() { runner.runOneTest("sequential_and_or_06") }
  @Test def test_sequential_and_or_07() { runner.runOneTest("sequential_and_or_07") }

  //DFDL-1059
  @Test def test_parent_axis_01() { runner.runOneTest("parent_axis_01") }
  @Test def test_child_axis_01() { runner.runOneTest("child_axis_01") }
  //@Test def test_self_axis_01() { runner.runOneTest("self_axis_01") }
  //@Test def test_multiple_axis_01() { runner.runOneTest("multiple_axis_01") }

  @Test def test_attribute_axis_04() { runner.runOneTest("attribute_axis_04") }

  @Test def test_ancestor_axis_01() { runner.runOneTest("ancestor_axis_01") }
  @Test def test_ancestor_or_self_axis_01() { runner.runOneTest("ancestor_or_self_axis_01") }
  @Test def test_descendant_axis_01() { runner.runOneTest("descendant_axis_01") }
  @Test def test_descendant_or_self_axis_01() { runner.runOneTest("descendant_or_self_axis_01") }
  @Test def test_following_axis_01() { runner.runOneTest("following_axis_01") }
  @Test def test_following_sibling_axis_01() { runner.runOneTest("following_sibling_axis_01") }
  @Test def test_namespace_axis_01() { runner.runOneTest("namespace_axis_01") }
  @Test def test_preceding_axis_01() { runner.runOneTest("preceding_axis_01") }
  @Test def test_preceding_sibling_axis_01() { runner.runOneTest("preceding_sibling_axis_01") }

  //DFDL-711
  //@Test def test_short_parent_axis_01() { runner.runOneTest("short_parent_axis_01") }

  /////////////////////// FUNCTIONS ///////////////////////////

  @Test def test_dateTimeFunctions01() { runner_fun.runOneTest("dateTimeFunctions01") }
  @Test def test_dateTimeFunctions02() { runner_fun.runOneTest("dateTimeFunctions02") }
  @Test def test_dateFunctions01() { runner_fun.runOneTest("dateFunctions01") }
  @Test def test_dateFunctions02() { runner_fun.runOneTest("dateFunctions02") }
  @Test def test_timeFunctions01() { runner_fun.runOneTest("timeFunctions01") }
  @Test def test_timeFunctions02() { runner_fun.runOneTest("timeFunctions02") }
  @Test def test_functionFail01() { runner_fun.runOneTest("functionFail01") }
  @Test def test_functionFail02() { runner_fun.runOneTest("functionFail02") }
  @Test def test_functionFail03() { runner_fun.runOneTest("functionFail03") }

  @Test def test_substringFunction01() { runner_fun.runOneTest("substringFunction01") }
  @Test def test_substringFunction02() { runner_fun.runOneTest("substringFunction02") }
  @Test def test_substringFunction03() { runner_fun.runOneTest("substringFunction03") }
  @Test def test_substringFunction04() { runner_fun.runOneTest("substringFunction04") }
  @Test def test_substringFunction05() { runner_fun.runOneTest("substringFunction05") }
  @Test def test_substringFunction06() { runner_fun.runOneTest("substringFunction06") }
  @Test def test_substringFunction07() { runner_fun.runOneTest("substringFunction07") }
  @Test def test_substringFunction08() { runner_fun.runOneTest("substringFunction08") }
  @Test def test_substringFunction12() { runner_fun.runOneTest("substringFunction12") }
  @Test def test_substringFunction13() { runner_fun.runOneTest("substringFunction13") }
  @Test def test_substringFunction14() { runner_fun.runOneTest("substringFunction14") }
  @Test def test_substringFunction15() { runner_fun.runOneTest("substringFunction15") }

  @Test def test_boolFunctionChoice_01() { runner2.runOneTest("boolFunctionChoice_01") }
  @Test def test_boolFunctionChoice_02() { runner2.runOneTest("boolFunctionChoice_02") }
  @Test def test_boolFunctionChoice_03() { runner2.runOneTest("boolFunctionChoice_03") }

  @Test def test_boolFlags_01() { runner2.runOneTest("boolFlags_01") }
  @Test def test_boolFlags_02() { runner2.runOneTest("boolFlags_02") }
  @Test def test_boolFlags_03() { runner2.runOneTest("boolFlags_03") }
  @Test def test_boolFlags_04() { runner2.runOneTest("boolFlags_04") }
  @Test def test_boolFlags_05() { runner2.runOneTest("boolFlags_05") }

  @Test def test_not_01() { runner2.runOneTest("not_01") }
  @Test def test_not_02() { runner2.runOneTest("not_02") }
  @Test def test_not_03() { runner2.runOneTest("not_03") }
  //DFDL-1076
  //@Test def test_not_04() { runner2.runOneTest("not_04") }
  //DFDL-1075
  //@Test def test_not_05() { runner2.runOneTest("not_05") }
  //@Test def test_not_07() { runner2.runOneTest("not_07") }
  @Test def test_not_06() { runner2.runOneTest("not_06") }
  @Test def test_not_08() { runner2.runOneTest("not_08") }
  @Test def test_not_09() { runner2.runOneTest("not_09") }
  @Test def test_not_10() { runner2.runOneTest("not_10") }
  @Test def test_not_11() { runner2.runOneTest("not_11") }
  @Test def test_not_12() { runner2.runOneTest("not_12") }
  @Test def test_not_13() { runner2.runOneTest("not_13") }
  @Test def test_not_14() { runner2.runOneTest("not_14") }
  @Test def test_not_15() { runner2.runOneTest("not_15") }
  @Test def test_not_16() { runner2.runOneTest("not_16") }

  @Test def test_xPathFunc_abs_01() { runner2.runOneTest("xPathFunc_abs_01") }
  @Test def test_xPathFunc_abs_02() { runner2.runOneTest("xPathFunc_abs_02") }
  @Test def test_xPathFunc_abs_03() { runner2.runOneTest("xPathFunc_abs_03") }
  @Test def test_xPathFunc_abs_04() { runner2.runOneTest("xPathFunc_abs_04") }
  @Test def test_abs_05() { runner2.runOneTest("abs_05") }
  @Test def test_abs_06() { runner2.runOneTest("abs_06") }
  @Test def test_abs_07() { runner2.runOneTest("abs_07") }
  @Test def test_abs_08() { runner2.runOneTest("abs_08") }
  @Test def test_abs_09() { runner2.runOneTest("abs_09") }

  @Test def test_xPathFunc_ceil_01() { runner2.runOneTest("xPathFunc_ceil_01") }
  @Test def test_xPathFunc_ceil_02() { runner2.runOneTest("xPathFunc_ceil_02") }
  @Test def test_xPathFunc_ceil_03() { runner2.runOneTest("xPathFunc_ceil_03") }
  @Test def test_xPathFunc_ceil_04() { runner2.runOneTest("xPathFunc_ceil_04") }
  @Test def test_xPathFunc_ceil_05() { runner2.runOneTest("xPathFunc_ceil_05") }
  @Test def test_ceil_06() { runner2.runOneTest("ceil_06") }
  @Test def test_ceil_07() { runner2.runOneTest("ceil_07") }
  @Test def test_ceil_08() { runner2.runOneTest("ceil_08") }

  @Test def test_xPathFunc_floor_01() { runner2.runOneTest("xPathFunc_floor_01") }
  @Test def test_xPathFunc_floor_02() { runner2.runOneTest("xPathFunc_floor_02") }
  @Test def test_xPathFunc_floor_03() { runner2.runOneTest("xPathFunc_floor_03") }
  @Test def test_xPathFunc_floor_04() { runner2.runOneTest("xPathFunc_floor_04") }
  @Test def test_xPathFunc_floor_05() { runner2.runOneTest("xPathFunc_floor_05") }
  @Test def test_floor_06() { runner2.runOneTest("floor_06") }
  @Test def test_floor_07() { runner2.runOneTest("floor_07") }
  @Test def test_floor_08() { runner2.runOneTest("floor_08") }

  @Test def test_xPathFunc_round_01() { runner2.runOneTest("xPathFunc_round_01") }
  @Test def test_xPathFunc_round_02() { runner2.runOneTest("xPathFunc_round_02") }
  @Test def test_xPathFunc_round_03() { runner2.runOneTest("xPathFunc_round_03") }
  @Test def test_xPathFunc_round_04() { runner2.runOneTest("xPathFunc_round_04") }
  @Test def test_xPathFunc_round_05() { runner2.runOneTest("xPathFunc_round_05") }
  @Test def test_xPathFunc_round_06() { runner2.runOneTest("xPathFunc_round_06") }
  @Test def test_round_07() { runner2.runOneTest("round_07") }
  @Test def test_round_08() { runner2.runOneTest("round_08") }
  @Test def test_round_09() { runner2.runOneTest("round_09") }
  @Test def test_round_10() { runner2.runOneTest("round_10") }
  @Test def test_round_11() { runner2.runOneTest("round_11") }
  @Test def test_round_12() { runner2.runOneTest("round_12") }
  @Test def test_round_13() { runner2.runOneTest("round_13") }
  @Test def test_xPathFunc_round_14() { runner2.runOneTest("xPathFunc_round_14") }
  @Test def test_xPathFunc_round_15() { runner2.runOneTest("xPathFunc_round_15") }
  @Test def test_xPathFunc_round_16() { runner2.runOneTest("xPathFunc_round_16") }

  @Test def test_xPathFunc_round_hte_01() { runner2.runOneTest("xPathFunc_round_hte_01") }
  @Test def test_xPathFunc_round_hte_02() { runner2.runOneTest("xPathFunc_round_hte_02") }
  @Test def test_xPathFunc_round_hte_03() { runner2.runOneTest("xPathFunc_round_hte_03") }
  @Test def test_xPathFunc_round_hte_04() { runner2.runOneTest("xPathFunc_round_hte_04") }
  @Test def test_xPathFunc_round_hte_05() { runner2.runOneTest("xPathFunc_round_hte_05") }
  @Test def test_xPathFunc_round_hte_06() { runner2.runOneTest("xPathFunc_round_hte_06") }
  @Test def test_xPathFunc_round_hte_07() { runner2.runOneTest("xPathFunc_round_hte_07") }
  @Test def test_xPathFunc_round_hte_08() { runner2.runOneTest("xPathFunc_round_hte_08") }
  @Test def test_round_hte_09() { runner2.runOneTest("round_hte_09") }
  @Test def test_round_hte_10() { runner2.runOneTest("round_hte_10") }
  @Test def test_round_hte_11() { runner2.runOneTest("round_hte_11") }
  @Test def test_round_hte_12() { runner2.runOneTest("round_hte_12") }
  @Test def test_round_hte_13() { runner2.runOneTest("round_hte_13") }
  @Test def test_round_hte_14() { runner2.runOneTest("round_hte_14") }
  @Test def test_round_hte_15() { runner2.runOneTest("round_hte_15") }
  @Test def test_round_hte_16() { runner2.runOneTest("round_hte_16") }

  //DFDL-1080
  //@Test def test_empty_02() { runner2.runOneTest("empty_02") }
  //@Test def test_exists_02() { runner2.runOneTest("exists_02") }

  //DFDL-1091
  //@Test def test_count_05b() { runner2.runOneTest("count_05b") }

  //DFDL-1097
  //@Test def test_local_name_06() { runner2.runOneTest("local_name_06") }

  @Test def test_empty_01() { runner2.runOneTest("empty_01") }
  @Test def test_empty_03() { runner2.runOneTest("empty_03") }
  @Test def test_empty_04() { runner2.runOneTest("empty_04") }
  @Test def test_empty_05() { runner2.runOneTest("empty_05") }
  @Test def test_empty_06() { runner2.runOneTest("empty_06") }
  @Test def test_empty_07() { runner2.runOneTest("empty_07") }
  @Test def test_empty_08() { runner2.runOneTest("empty_08") }
  @Test def test_empty_09() { runner2.runOneTest("empty_09") }

  @Test def test_exists_01() { runner2.runOneTest("exists_01") }
  @Test def test_exists_03() { runner2.runOneTest("exists_03") }
  @Test def test_exists_04() { runner2.runOneTest("exists_04") }
  @Test def test_exists_06() { runner2.runOneTest("exists_06") }
  @Test def test_exists_07() { runner2.runOneTest("exists_07") }
  @Test def test_exists_05() { runner2.runOneTest("exists_05") }
  @Test def test_exists_08() { runner2.runOneTest("exists_08") }
  @Test def test_exists_09() { runner2.runOneTest("exists_09") }

  //DFDL-1120
  //@Test def test_exists_10() { runner2.runOneTest("exists_10") }

  @Test def test_exists_11() { runner2.runOneTest("exists_11") }
  @Test def test_exists_12() { runner2.runOneTest("exists_12") }

  //DFDL-1189
  //@Test def test_exactly_one_01() { runner2.runOneTest("exactly_one_01") }
  //@Test def test_exactly_one_02() { runner2.runOneTest("exactly_one_02") }
  //@Test def test_exactly_one_03() { runner2.runOneTest("exactly_one_03") }
  //@Test def test_exactly_one_04() { runner2.runOneTest("exactly_one_04") }
  //@Test def test_exactly_one_05() { runner2.runOneTest("exactly_one_05") }
  //@Test def test_exactly_one_06() { runner2.runOneTest("exactly_one_06") }

  @Test def test_count_01() { runner2.runOneTest("count_01") }
  @Test def test_count_02() { runner2.runOneTest("count_02") }
  @Test def test_count_03() { runner2.runOneTest("count_03") }
  @Test def test_count_03b() { runner2.runOneTest("count_03b") }
  @Test def test_count_04() { runner2.runOneTest("count_04") }
  @Test def test_count_05c() { runner2.runOneTest("count_05c") }
  @Test def test_count_06b() { runner2.runOneTest("count_06b") }
  @Test def test_count_07() { runner2.runOneTest("count_07") }
  @Test def test_count_08b() { runner2.runOneTest("count_08b") }

  //DFDL-1159 (unordered sequences)
  //@Test def test_count_05() { runner2.runOneTest("count_05") }
  //@Test def test_count_06() { runner2.runOneTest("count_06") }
  //@Test def test_count_08() { runner2.runOneTest("count_08") }

  @Test def test_local_name_01() { runner2.runOneTest("local_name_01") }
  @Test def test_local_name_02() { runner2.runOneTest("local_name_02") }
  @Test def test_local_name_03() { runner2.runOneTest("local_name_03") }
  @Test def test_local_name_04() { runner2.runOneTest("local_name_04") }
  @Test def test_local_name_05() { runner2.runOneTest("local_name_05") }
  //DFDL-1151
  //@Test def test_local_name_07() { runner2.runOneTest("local_name_07") }

  //DFDL-1101
  //@Test def test_namespace_uri_01() { runner2.runOneTest("namespace_uri_01") }
  //@Test def test_namespace_uri_02() { runner2.runOneTest("namespace_uri_02") }
  //DFDL-1114
  //@Test def test_namespace_uri_03() { runner2.runOneTest("namespace_uri_03") }
  //@Test def test_namespace_uri_04() { runner2.runOneTest("namespace_uri_04") }
  //@Test def test_namespace_uri_05() { runner2.runOneTest("namespace_uri_05") }
  //@Test def test_namespace_uri_06() { runner2.runOneTest("namespace_uri_06") }

  //DFDL-1076
  //@Test def test_nilled_01() { runner2.runOneTest("nilled_01") }
  //DFDL-1233
  //@Test def test_nilled_02() { runner2.runOneTest("nilled_02") }
  //@Test def test_nilled_03() { runner2.runOneTest("nilled_03") }
  //@Test def test_nilled_04() { runner2.runOneTest("nilled_04") }

  @Test def test_concat_01() { runner2.runOneTest("concat_01") }
  @Test def test_concat_02() { runner2.runOneTest("concat_02") }
  @Test def test_concat_03() { runner2.runOneTest("concat_03") }
  @Test def test_concat_04() { runner2.runOneTest("concat_04") }
  @Test def test_concat_05() { runner2.runOneTest("concat_05") }

  @Test def test_substring_01() { runner2.runOneTest("substring_01") }
  @Test def test_substring_02() { runner2.runOneTest("substring_02") }
  @Test def test_substring_03() { runner2.runOneTest("substring_03") }
  @Test def test_substring_04() { runner2.runOneTest("substring_04") }
  @Test def test_substring_05() { runner2.runOneTest("substring_05") }
  @Test def test_substring_06() { runner2.runOneTest("substring_06") }
  @Test def test_substring_07() { runner2.runOneTest("substring_07") }
  @Test def test_substring_08() { runner2.runOneTest("substring_08") }
  @Test def test_substring_09() { runner2.runOneTest("substring_09") }
  @Test def test_substring_10() { runner2.runOneTest("substring_10") }
  @Test def test_substring_11() { runner2.runOneTest("substring_11") }

  @Test def test_stringlength_01() { runner2.runOneTest("stringlength_01") }
  @Test def test_stringlength_02() { runner2.runOneTest("stringlength_02") }
  @Test def test_stringlength_03() { runner2.runOneTest("stringlength_03") }
  @Test def test_stringlength_04() { runner2.runOneTest("stringlength_04") }

  @Test def test_uppercase_01() { runner2.runOneTest("uppercase_01") }
  @Test def test_uppercase_02() { runner2.runOneTest("uppercase_02") }
  @Test def test_uppercase_03() { runner2.runOneTest("uppercase_03") }

  @Test def test_lowercase_01() { runner2.runOneTest("lowercase_01") }
  @Test def test_lowercase_02() { runner2.runOneTest("lowercase_02") }
  @Test def test_lowercase_03() { runner2.runOneTest("lowercase_03") }

  @Test def test_lowercase_04() { runner2_utf8.runOneTest("lowercase_04") }
  @Test def test_uppercase_04() { runner2_utf8.runOneTest("uppercase_04") }
  @Test def test_uppercase_05() { runner2_utf8.runOneTest("uppercase_05") }
  @Test def test_lowercase_05() { runner2_utf8.runOneTest("lowercase_05") }

  @Test def test_contains_01() { runner2.runOneTest("contains_01") }
  @Test def test_contains_02() { runner2.runOneTest("contains_02") }
  @Test def test_contains_03() { runner2.runOneTest("contains_03") }
  @Test def test_contains_04() { runner2.runOneTest("contains_04") }
  @Test def test_contains_05() { runner2.runOneTest("contains_05") }
  @Test def test_contains_06() { runner2.runOneTest("contains_06") }
  @Test def test_contains_07() { runner2.runOneTest("contains_07") }

  @Test def test_startswith_01() { runner2.runOneTest("startswith_01") }
  @Test def test_startswith_02() { runner2.runOneTest("startswith_02") }
  @Test def test_startswith_03() { runner2.runOneTest("startswith_03") }
  @Test def test_startswith_04() { runner2.runOneTest("startswith_04") }
  @Test def test_startswith_05() { runner2.runOneTest("startswith_05") }
  @Test def test_startswith_06() { runner2.runOneTest("startswith_06") }
  @Test def test_startswith_07() { runner2.runOneTest("startswith_07") }

  @Test def test_endswith_01() { runner2.runOneTest("endswith_01") }
  @Test def test_endswith_02() { runner2.runOneTest("endswith_02") }
  @Test def test_endswith_03() { runner2.runOneTest("endswith_03") }
  @Test def test_endswith_04() { runner2.runOneTest("endswith_04") }
  @Test def test_endswith_05() { runner2.runOneTest("endswith_05") }
  @Test def test_endswith_06() { runner2.runOneTest("endswith_06") }
  @Test def test_endswith_07() { runner2.runOneTest("endswith_07") }

  @Test def test_substringbefore_01() { runner2.runOneTest("substringbefore_01") }
  @Test def test_substringbefore_02() { runner2.runOneTest("substringbefore_02") }
  @Test def test_substringbefore_03() { runner2.runOneTest("substringbefore_03") }
  @Test def test_substringbefore_04() { runner2.runOneTest("substringbefore_04") }
  @Test def test_substringbefore_05() { runner2.runOneTest("substringbefore_05") }
  @Test def test_substringbefore_06() { runner2.runOneTest("substringbefore_06") }
  @Test def test_substringbefore_07() { runner2.runOneTest("substringbefore_07") }

  @Test def test_substringafter_01() { runner2.runOneTest("substringafter_01") }
  @Test def test_substringafter_02() { runner2.runOneTest("substringafter_02") }
  @Test def test_substringafter_03() { runner2.runOneTest("substringafter_03") }
  @Test def test_substringafter_04() { runner2.runOneTest("substringafter_04") }
  @Test def test_substringafter_05() { runner2.runOneTest("substringafter_05") }
  @Test def test_substringafter_06() { runner2.runOneTest("substringafter_06") }
  @Test def test_substringafter_07() { runner2.runOneTest("substringafter_07") }

  @Test def test_yearfromdatetime_01() { runner2.runOneTest("yearfromdatetime_01") }
  @Test def test_yearfromdatetime_02() { runner2.runOneTest("yearfromdatetime_02") }
  @Test def test_yearfromdatetime_03() { runner2.runOneTest("yearfromdatetime_03") }
  @Test def test_monthfromdatetime_01() { runner2.runOneTest("monthfromdatetime_01") }
  @Test def test_monthfromdatetime_02() { runner2.runOneTest("monthfromdatetime_02") }
  @Test def test_dayfromdatetime_01() { runner2.runOneTest("dayfromdatetime_01") }
  @Test def test_dayfromdatetime_02() { runner2.runOneTest("dayfromdatetime_02") }
  @Test def test_hoursfromdatetime_01() { runner2.runOneTest("hoursfromdatetime_01") }
  @Test def test_hoursfromdatetime_02() { runner2.runOneTest("hoursfromdatetime_02") }
  @Test def test_minutesfromdatetime_01() { runner2.runOneTest("minutesfromdatetime_01") }
  @Test def test_minutesfromdatetime_02() { runner2.runOneTest("minutesfromdatetime_02") }
  @Test def test_secondsfromdatetime_01() { runner2.runOneTest("secondsfromdatetime_01") }
  @Test def test_secondsfromdatetime_02() { runner2.runOneTest("secondsfromdatetime_02") }
  @Test def test_secondsfromdatetime_03() { runner2.runOneTest("secondsfromdatetime_03") }
  @Test def test_timezonefromdatetime_01() { runner2.runOneTest("timezonefromdatetime_01") }
  @Test def test_timezonefromdatetime_02() { runner2.runOneTest("timezonefromdatetime_02") }

  @Test def test_xfromdatetime_01() { runner2.runOneTest("xfromdatetime_01") }
  @Test def test_xfromdatetime_02() { runner2.runOneTest("xfromdatetime_02") }
  @Test def test_xfromdatetime_03() { runner2.runOneTest("xfromdatetime_03") }
  @Test def test_xfromdatetime_04() { runner2.runOneTest("xfromdatetime_04") }

  @Test def test_yearfromdate_01() { runner2.runOneTest("yearfromdate_01") }
  @Test def test_yearfromdate_02() { runner2.runOneTest("yearfromdate_02") }
  @Test def test_yearfromdate_03() { runner2.runOneTest("yearfromdate_03") }
  @Test def test_monthfromdate_01() { runner2.runOneTest("monthfromdate_01") }
  @Test def test_monthfromdate_02() { runner2.runOneTest("monthfromdate_02") }
  @Test def test_dayfromdate_01() { runner2.runOneTest("dayfromdate_01") }
  @Test def test_dayfromdate_02() { runner2.runOneTest("dayfromdate_02") }
  @Test def test_timezonefromdate_01() { runner2.runOneTest("timezonefromdate_01") }
  @Test def test_timezonefromdate_02() { runner2.runOneTest("timezonefromdate_02") }

  @Test def test_xfromdate_01() { runner2.runOneTest("xfromdate_01") }
  @Test def test_xfromdate_02() { runner2.runOneTest("xfromdate_02") }
  @Test def test_xfromdate_03() { runner2.runOneTest("xfromdate_03") }

  @Test def test_hoursfromtime_01() { runner2.runOneTest("hoursfromtime_01") }
  @Test def test_hoursfromtime_02() { runner2.runOneTest("hoursfromtime_02") }
  @Test def test_minutesfromtime_01() { runner2.runOneTest("minutesfromtime_01") }
  @Test def test_minutesfromtime_02() { runner2.runOneTest("minutesfromtime_02") }
  @Test def test_secondsfromtime_01() { runner2.runOneTest("secondsfromtime_01") }
  @Test def test_secondsfromtime_02() { runner2.runOneTest("secondsfromtime_02") }
  @Test def test_timezonefromtime_01() { runner2.runOneTest("timezonefromtime_01") }
  @Test def test_timezonefromtime_02() { runner2.runOneTest("timezonefromtime_02") }
  @Test def test_timezonefromtime_03() { runner2.runOneTest("timezonefromtime_03") }

  @Test def test_xfromtime_01() { runner2.runOneTest("xfromtime_01") }
  @Test def test_xfromtime_02() { runner2.runOneTest("xfromtime_02") }
  @Test def test_xfromtime_03() { runner2.runOneTest("xfromtime_03") }

  @Test def test_fn_text_01() { runner2.runOneTest("fn_text_01") }
  @Test def test_fn_text_02() { runner2.runOneTest("fn_text_02") }

  @Test def test_ubyte_constructor_01() { runner2.runOneTest("ubyte_constructor_01") }
  @Test def test_ubyte_constructor_02() { runner2.runOneTest("ubyte_constructor_02") }
  @Test def test_ubyte_constructor_03() { runner2.runOneTest("ubyte_constructor_03") }
  @Test def test_ubyte_constructor_04() { runner2.runOneTest("ubyte_constructor_04") }

  @Test def test_uint_constructor_01() { runner2.runOneTest("uint_constructor_01") }
  @Test def test_uint_constructor_02() { runner2.runOneTest("uint_constructor_02") }
  @Test def test_uint_constructor_03() { runner2.runOneTest("uint_constructor_03") }
  @Test def test_uint_constructor_04() { runner2.runOneTest("uint_constructor_04") }

  @Test def test_nonNeg_constructor_01() { runner2.runOneTest("nonNeg_constructor_01") }
  @Test def test_nonNeg_constructor_02() { runner2.runOneTest("nonNeg_constructor_02") }
  @Test def test_nonNeg_constructor_03() { runner2.runOneTest("nonNeg_constructor_03") }
  @Test def test_nonNeg_constructor_04() { runner2.runOneTest("nonNeg_constructor_04") }

  @Test def test_byte_constructor_01() { runner2.runOneTest("byte_constructor_01") }
  @Test def test_byte_constructor_02() { runner2.runOneTest("byte_constructor_02") }
  @Test def test_byte_constructor_03() { runner2.runOneTest("byte_constructor_03") }
  @Test def test_byte_constructor_04() { runner2.runOneTest("byte_constructor_04") }

  @Test def test_hexBinary_constructor_01() { runner2.runOneTest("hexBinary_constructor_01") }
  @Test def test_hexBinary_constructor_02() { runner2.runOneTest("hexBinary_constructor_02") }
  @Test def test_hexBinary_constructor_03() { runner2.runOneTest("hexBinary_constructor_03") }
  @Test def test_hexBinary_constructor_04() { runner2.runOneTest("hexBinary_constructor_04") }

  @Test def test_dfdlHexBinary_constructor_01() { runner2.runOneTest("dfdlHexBinary_constructor_01") }
  @Test def test_dfdlHexBinary_constructor_02() { runner2.runOneTest("dfdlHexBinary_constructor_02") }
  @Test def test_dfdlHexBinary_constructor_03() { runner2.runOneTest("dfdlHexBinary_constructor_03") }
  @Test def test_dfdlHexBinary_constructor_04() { runner2.runOneTest("dfdlHexBinary_constructor_04") }

  @Test def test_dfdlByte_constructor_01() { runner2.runOneTest("dfdlByte_constructor_01") }
  @Test def test_dfdlByte_constructor_02() { runner2.runOneTest("dfdlByte_constructor_02") }
  @Test def test_dfdlByte_constructor_03() { runner2.runOneTest("dfdlByte_constructor_03") }
  @Test def test_dfdlByte_constructor_04() { runner2.runOneTest("dfdlByte_constructor_04") }
  @Test def test_dfdlByte_constructor_05() { runner2.runOneTest("dfdlByte_constructor_05") }
  @Test def test_dfdlByte_constructor_06() { runner2.runOneTest("dfdlByte_constructor_06") }
  @Test def test_dfdlByte_constructor_07() { runner2.runOneTest("dfdlByte_constructor_07") }
  @Test def test_dfdlByte_constructor_08() { runner2.runOneTest("dfdlByte_constructor_08") }
  @Test def test_dfdlByte_constructor_09() { runner2.runOneTest("dfdlByte_constructor_09") }
  @Test def test_dfdlByte_constructor_10() { runner2.runOneTest("dfdlByte_constructor_10") }

  @Test def test_dfdlUByte_constructor_01() { runner2.runOneTest("dfdlUByte_constructor_01") }
  @Test def test_dfdlUByte_constructor_02() { runner2.runOneTest("dfdlUByte_constructor_02") }
  @Test def test_dfdlUByte_constructor_03() { runner2.runOneTest("dfdlUByte_constructor_03") }
  @Test def test_dfdlUByte_constructor_04() { runner2.runOneTest("dfdlUByte_constructor_04") }
  @Test def test_dfdlUByte_constructor_05() { runner2.runOneTest("dfdlUByte_constructor_05") }
  @Test def test_dfdlUByte_constructor_06() { runner2.runOneTest("dfdlUByte_constructor_06") }
  @Test def test_dfdlUByte_constructor_07() { runner2.runOneTest("dfdlUByte_constructor_07") }
  @Test def test_dfdlUByte_constructor_08() { runner2.runOneTest("dfdlUByte_constructor_08") }
  @Test def test_dfdlUByte_constructor_09() { runner2.runOneTest("dfdlUByte_constructor_09") }
  @Test def test_dfdlUByte_constructor_11() { runner2.runOneTest("dfdlUByte_constructor_11") }

  @Test def test_dfdlShort_constructor_01() { runner2.runOneTest("dfdlShort_constructor_01") }
  @Test def test_dfdlShort_constructor_02() { runner2.runOneTest("dfdlShort_constructor_02") }
  @Test def test_dfdlShort_constructor_03() { runner2.runOneTest("dfdlShort_constructor_03") }
  @Test def test_dfdlShort_constructor_04() { runner2.runOneTest("dfdlShort_constructor_04") }
  @Test def test_dfdlShort_constructor_05() { runner2.runOneTest("dfdlShort_constructor_05") }
  @Test def test_dfdlShort_constructor_06() { runner2.runOneTest("dfdlShort_constructor_06") }
  @Test def test_dfdlShort_constructor_07() { runner2.runOneTest("dfdlShort_constructor_07") }
  @Test def test_dfdlShort_constructor_08() { runner2.runOneTest("dfdlShort_constructor_08") }
  @Test def test_dfdlShort_constructor_09() { runner2.runOneTest("dfdlShort_constructor_09") }
  @Test def test_dfdlShort_constructor_11() { runner2.runOneTest("dfdlShort_constructor_11") }

  @Test def test_dfdlUShort_constructor_01() { runner2.runOneTest("dfdlUShort_constructor_01") }
  @Test def test_dfdlUShort_constructor_02() { runner2.runOneTest("dfdlUShort_constructor_02") }
  @Test def test_dfdlUShort_constructor_03() { runner2.runOneTest("dfdlUShort_constructor_03") }
  @Test def test_dfdlUShort_constructor_04() { runner2.runOneTest("dfdlUShort_constructor_04") }
  @Test def test_dfdlUShort_constructor_05() { runner2.runOneTest("dfdlUShort_constructor_05") }
  @Test def test_dfdlUShort_constructor_06() { runner2.runOneTest("dfdlUShort_constructor_06") }
  @Test def test_dfdlUShort_constructor_07() { runner2.runOneTest("dfdlUShort_constructor_07") }
  @Test def test_dfdlUShort_constructor_08() { runner2.runOneTest("dfdlUShort_constructor_08") }
  @Test def test_dfdlUShort_constructor_09() { runner2.runOneTest("dfdlUShort_constructor_09") }
  @Test def test_dfdlUShort_constructor_11() { runner2.runOneTest("dfdlUShort_constructor_11") }

  @Test def test_dfdlInt_constructor_01() { runner2.runOneTest("dfdlInt_constructor_01") }
  @Test def test_dfdlInt_constructor_02() { runner2.runOneTest("dfdlInt_constructor_02") }
  @Test def test_dfdlInt_constructor_03() { runner2.runOneTest("dfdlInt_constructor_03") }
  @Test def test_dfdlInt_constructor_04() { runner2.runOneTest("dfdlInt_constructor_04") }
  @Test def test_dfdlInt_constructor_05() { runner2.runOneTest("dfdlInt_constructor_05") }
  @Test def test_dfdlInt_constructor_06() { runner2.runOneTest("dfdlInt_constructor_06") }
  @Test def test_dfdlInt_constructor_07() { runner2.runOneTest("dfdlInt_constructor_07") }
  @Test def test_dfdlInt_constructor_08() { runner2.runOneTest("dfdlInt_constructor_08") }
  @Test def test_dfdlInt_constructor_09() { runner2.runOneTest("dfdlInt_constructor_09") }
  @Test def test_dfdlInt_constructor_11() { runner2.runOneTest("dfdlInt_constructor_11") }
  @Test def test_dfdlInt_constructor_12() { runner2.runOneTest("dfdlInt_constructor_12") }
  @Test def test_dfdlInt_constructor_13() { runner2.runOneTest("dfdlInt_constructor_13") }
  @Test def test_dfdlInt_constructor_14() { runner2.runOneTest("dfdlInt_constructor_14") }

  @Test def test_dfdlUInt_constructor_01() { runner2.runOneTest("dfdlUInt_constructor_01") }
  @Test def test_dfdlUInt_constructor_02() { runner2.runOneTest("dfdlUInt_constructor_02") }
  @Test def test_dfdlUInt_constructor_03() { runner2.runOneTest("dfdlUInt_constructor_03") }
  @Test def test_dfdlUInt_constructor_04() { runner2.runOneTest("dfdlUInt_constructor_04") }
  @Test def test_dfdlUInt_constructor_05() { runner2.runOneTest("dfdlUInt_constructor_05") }
  @Test def test_dfdlUInt_constructor_06() { runner2.runOneTest("dfdlUInt_constructor_06") }
  @Test def test_dfdlUInt_constructor_07() { runner2.runOneTest("dfdlUInt_constructor_07") }
  @Test def test_dfdlUInt_constructor_08() { runner2.runOneTest("dfdlUInt_constructor_08") }
  @Test def test_dfdlUInt_constructor_09() { runner2.runOneTest("dfdlUInt_constructor_09") }
  @Test def test_dfdlUInt_constructor_11() { runner2.runOneTest("dfdlUInt_constructor_11") }
  @Test def test_dfdlUInt_constructor_12() { runner2.runOneTest("dfdlUInt_constructor_12") }

  @Test def test_dfdlLong_constructor_01() { runner2.runOneTest("dfdlLong_constructor_01") }
  @Test def test_dfdlLong_constructor_02() { runner2.runOneTest("dfdlLong_constructor_02") }
  @Test def test_dfdlLong_constructor_03() { runner2.runOneTest("dfdlLong_constructor_03") }
  @Test def test_dfdlLong_constructor_04() { runner2.runOneTest("dfdlLong_constructor_04") }
  @Test def test_dfdlLong_constructor_05() { runner2.runOneTest("dfdlLong_constructor_05") }
  @Test def test_dfdlLong_constructor_06() { runner2.runOneTest("dfdlLong_constructor_06") }
  @Test def test_dfdlLong_constructor_07() { runner2.runOneTest("dfdlLong_constructor_07") }
  @Test def test_dfdlLong_constructor_08() { runner2.runOneTest("dfdlLong_constructor_08") }
  @Test def test_dfdlLong_constructor_09() { runner2.runOneTest("dfdlLong_constructor_09") }
  @Test def test_dfdlLong_constructor_11() { runner2.runOneTest("dfdlLong_constructor_11") }
  @Test def test_dfdlLong_constructor_12() { runner2.runOneTest("dfdlLong_constructor_12") }

  @Test def test_dfdlULong_constructor_01() { runner2.runOneTest("dfdlULong_constructor_01") }
  @Test def test_dfdlULong_constructor_02() { runner2.runOneTest("dfdlULong_constructor_02") }
  @Test def test_dfdlULong_constructor_03() { runner2.runOneTest("dfdlULong_constructor_03") }
  @Test def test_dfdlULong_constructor_04() { runner2.runOneTest("dfdlULong_constructor_04") }
  @Test def test_dfdlULong_constructor_05() { runner2.runOneTest("dfdlULong_constructor_05") }
  @Test def test_dfdlULong_constructor_06() { runner2.runOneTest("dfdlULong_constructor_06") }
  @Test def test_dfdlULong_constructor_07() { runner2.runOneTest("dfdlULong_constructor_07") }
  @Test def test_dfdlULong_constructor_08() { runner2.runOneTest("dfdlULong_constructor_08") }
  @Test def test_dfdlULong_constructor_09() { runner2.runOneTest("dfdlULong_constructor_09") }
  @Test def test_dfdlULong_constructor_11() { runner2.runOneTest("dfdlULong_constructor_11") }
  @Test def test_dfdlULong_constructor_12() { runner2.runOneTest("dfdlULong_constructor_12") }

  @Test def test_xsDateTime_constructor_06() { runner2.runOneTest("xsDateTime_constructor_06") }
  @Test def test_xsDateTime_constructor_07() { runner2.runOneTest("xsDateTime_constructor_07") }
  @Test def test_xsDateTime_constructor_08() { runner2.runOneTest("xsDateTime_constructor_08") }
  @Test def test_xsDateTime_constructor_09() { runner2.runOneTest("xsDateTime_constructor_09") }
  @Test def test_xsDateTime_constructor_10() { runner2.runOneTest("xsDateTime_constructor_10") }
  @Test def test_date_constructor_05() { runner2.runOneTest("date_constructor_05") }
  @Test def test_date_constructor_06() { runner2.runOneTest("date_constructor_06") }
  @Test def test_date_constructor_07() { runner2.runOneTest("date_constructor_07") }
  @Test def test_date_constructor_08() { runner2.runOneTest("date_constructor_08") }
  @Test def test_time_constructor_05() { runner2.runOneTest("time_constructor_05") }
  @Test def test_time_constructor_06() { runner2.runOneTest("time_constructor_06") }
  @Test def test_time_constructor_07() { runner2.runOneTest("time_constructor_07") }
  @Test def test_time_constructor_08() { runner2.runOneTest("time_constructor_08") }
  @Test def test_time_constructor_09() { runner2.runOneTest("time_constructor_09") }

  @Test def test_time_constructor_01() { runner2.runOneTest("time_constructor_01") }
  @Test def test_time_constructor_02() { runner2.runOneTest("time_constructor_02") }
  @Test def test_time_constructor_03() { runner2.runOneTest("time_constructor_03") }
  @Test def test_time_constructor_04() { runner2.runOneTest("time_constructor_04") }

  //DFDL-1124
  //@Test def test_date_constructor_01() { runner2.runOneTest("date_constructor_01") }
  @Test def test_date_constructor_02() { runner2.runOneTest("date_constructor_02") }
  @Test def test_date_constructor_03() { runner2.runOneTest("date_constructor_03") }
  @Test def test_date_constructor_04() { runner2.runOneTest("date_constructor_04") }

  @Test def test_xsDateTime_constructor_01() { runner2.runOneTest("xsDateTime_constructor_01") }
  @Test def test_xsDateTime_constructor_02() { runner2.runOneTest("xsDateTime_constructor_02") }
  //DFDL-1115
  //@Test def test_xsDateTime_constructor_03() { runner2.runOneTest("xsDateTime_constructor_03") }
  @Test def test_xsDateTime_constructor_04() { runner2.runOneTest("xsDateTime_constructor_04") }
  @Test def test_xsDateTime_constructor_05() { runner2.runOneTest("xsDateTime_constructor_05") }

  @Test def test_double_constructor_01() { runner2.runOneTest("double_constructor_01") }
  @Test def test_double_constructor_02() { runner2.runOneTest("double_constructor_02") }
  @Test def test_double_constructor_03() { runner2.runOneTest("double_constructor_03") }
  @Test def test_double_constructor_04() { runner2.runOneTest("double_constructor_04") }
  @Test def test_double_constructor_05() { runner2.runOneTest("double_constructor_05") }
  @Test def test_double_constructor_06() { runner2.runOneTest("double_constructor_06") }
  @Test def test_double_constructor_07() { runner2.runOneTest("double_constructor_07") }

  @Test def test_float_constructor_01() { runner2.runOneTest("float_constructor_01") }
  @Test def test_float_constructor_02() { runner2.runOneTest("float_constructor_02") }
  @Test def test_float_constructor_03() { runner2.runOneTest("float_constructor_03") }
  @Test def test_float_constructor_04() { runner2.runOneTest("float_constructor_04") }

  @Test def test_decimal_constructor_01() { runner2.runOneTest("decimal_constructor_01") }
  @Test def test_decimal_constructor_02() { runner2.runOneTest("decimal_constructor_02") }
  @Test def test_decimal_constructor_03() { runner2.runOneTest("decimal_constructor_03") }
  @Test def test_decimal_constructor_04() { runner2.runOneTest("decimal_constructor_04") }
  @Test def test_decimal_constructor_05() { runner2.runOneTest("decimal_constructor_05") }
  @Test def test_decimal_constructor_06() { runner2.runOneTest("decimal_constructor_06") }

  @Test def test_short_constructor_01() { runner2.runOneTest("short_constructor_01") }
  @Test def test_short_constructor_02() { runner2.runOneTest("short_constructor_02") }
  @Test def test_short_constructor_03() { runner2.runOneTest("short_constructor_03") }
  @Test def test_short_constructor_04() { runner2.runOneTest("short_constructor_04") }
  @Test def test_short_constructor_05() { runner2.runOneTest("short_constructor_05") }
  @Test def test_short_constructor_06() { runner2.runOneTest("short_constructor_06") }

  @Test def test_ushort_constructor_01() { runner2.runOneTest("ushort_constructor_01") }
  @Test def test_ushort_constructor_02() { runner2.runOneTest("ushort_constructor_02") }
  @Test def test_ushort_constructor_03() { runner2.runOneTest("ushort_constructor_03") }
  @Test def test_ushort_constructor_04() { runner2.runOneTest("ushort_constructor_04") }
  @Test def test_ushort_constructor_05() { runner2.runOneTest("ushort_constructor_05") }

  @Test def test_ulong_constructor_01() { runner2.runOneTest("ulong_constructor_01") }
  @Test def test_ulong_constructor_02() { runner2.runOneTest("ulong_constructor_02") }
  @Test def test_ulong_constructor_03() { runner2.runOneTest("ulong_constructor_03") }
  @Test def test_ulong_constructor_04() { runner2.runOneTest("ulong_constructor_04") }
  @Test def test_ulong_constructor_05() { runner2.runOneTest("ulong_constructor_05") }

  @Test def test_long_constructor_01() { runner2.runOneTest("long_constructor_01") }
  @Test def test_long_constructor_02() { runner2.runOneTest("long_constructor_02") }
  @Test def test_long_constructor_03() { runner2.runOneTest("long_constructor_03") }
  @Test def test_long_constructor_04() { runner2.runOneTest("long_constructor_04") }

  @Test def test_int_constructor_01() { runner2.runOneTest("int_constructor_01") }
  @Test def test_int_constructor_02() { runner2.runOneTest("int_constructor_02") }
  @Test def test_int_constructor_03() { runner2.runOneTest("int_constructor_03") }
  @Test def test_int_constructor_04() { runner2.runOneTest("int_constructor_04") }

  @Test def test_fnDateTime_constructor_01() { runner2.runOneTest("fnDateTime_constructor_01") }
  @Test def test_fnDateTime_constructor_02() { runner2.runOneTest("fnDateTime_constructor_02") }
  @Test def test_fnDateTime_constructor_03() { runner2.runOneTest("fnDateTime_constructor_03") }
  @Test def test_fnDateTime_constructor_04() { runner2.runOneTest("fnDateTime_constructor_04") }
  @Test def test_fnDateTime_constructor_05() { runner2.runOneTest("fnDateTime_constructor_05") }
  @Test def test_fnDateTime_constructor_06() { runner2.runOneTest("fnDateTime_constructor_06") }
  @Test def test_fnDateTime_constructor_07() { runner2.runOneTest("fnDateTime_constructor_07") }
  @Test def test_fnDateTime_constructor_08() { runner2.runOneTest("fnDateTime_constructor_08") }
  @Test def test_fnDateTime_constructor_09() { runner2.runOneTest("fnDateTime_constructor_09") }
  @Test def test_fnDateTime_constructor_10() { runner2.runOneTest("fnDateTime_constructor_10") }

  @Test def test_integer_constructor_01() { runner2.runOneTest("integer_constructor_01") }
  @Test def test_integer_constructor_02() { runner2.runOneTest("integer_constructor_02") }
  @Test def test_integer_constructor_03() { runner2.runOneTest("integer_constructor_03") }
  @Test def test_integer_constructor_04() { runner2.runOneTest("integer_constructor_04") }
  @Test def test_integer_constructor_05() { runner2.runOneTest("integer_constructor_05") }
  @Test def test_integer_constructor_06() { runner2.runOneTest("integer_constructor_06") }
  @Test def test_integer_constructor_07() { runner2.runOneTest("integer_constructor_07") }

  @Test def test_testBit_0() { runner2.runOneTest("testBit_0") }
  @Test def test_testBit_1() { runner2.runOneTest("testBit_1") }
  @Test def test_testBit_2() { runner2.runOneTest("testBit_2") }
  @Test def test_testBit_3() { runner2.runOneTest("testBit_3") }
  @Test def test_testBit_4() { runner2.runOneTest("testBit_4") }
  @Test def test_testBit_5() { runner2.runOneTest("testBit_5") }

  @Test def test_stringLiteralFromString_obsolete() { runner2.runOneTest("stringLiteralFromString_obsolete") }
  @Test def test_containsEntity_obsolete() { runner2.runOneTest("containsEntity_obsolete") }

  @Test def test_encodeDFDLEntities_0() { runner2.runOneTest("encodeDFDLEntities_0") }
  @Test def test_encodeDFDLEntities_1() { runner2.runOneTest("encodeDFDLEntities_1") }
  @Test def test_encodeDFDLEntities_2() { runner2.runOneTest("encodeDFDLEntities_2") }
  @Test def test_encodeDFDLEntities_3() { runner2.runOneTest("encodeDFDLEntities_3") }
  @Test def test_encodeDFDLEntities_4() { runner2.runOneTest("encodeDFDLEntities_4") }

  @Test def test_setBits_0() { runner2.runOneTest("setBits_0") }
  @Test def test_setBits_1() { runner2.runOneTest("setBits_1") }
  @Test def test_setBits_2() { runner2.runOneTest("setBits_2") }

  @Test def test_containsDFDLEntities_0() { runner2.runOneTest("containsDFDLEntities_0") }
  @Test def test_containsDFDLEntities_1() { runner2.runOneTest("containsDFDLEntities_1") }
  @Test def test_containsDFDLEntities_2() { runner2.runOneTest("containsDFDLEntities_2") }
  @Test def test_containsDFDLEntities_3() { runner2.runOneTest("containsDFDLEntities_3") }
  @Test def test_containsDFDLEntities_4() { runner2.runOneTest("containsDFDLEntities_4") }

  //DFDL-1118
  //@Test def test_more_count_0() { runner2.runOneTest("more_count_0") }
  //@Test def test_more_count_1() { runner2.runOneTest("more_count_1") }
  @Test def test_more_count_1b() { runner2.runOneTest("more_count_1b") }
  //@Test def test_more_count_1b_2() { runner2.runOneTest("more_count_1b_2") }
  //@Test def test_more_count_2() { runner2.runOneTest("more_count_2") }
  @Test def test_more_count_3() { runner2.runOneTest("more_count_3") }
  @Test def test_more_count_3b() { runner2.runOneTest("more_count_3b") }

  @Test def test_more_count_4() { runner2.runOneTest("more_count_4") }

  @Test def test_valueLength_0() { runner2.runOneTest("valueLength_0") }
  @Test def test_valueLength_1() { runner2.runOneTest("valueLength_1") }
  // DFDL-1516:dfdl:contentLength & dfdl:valueLength specifying lengthUnits 'characters' and variable-width encodings
  @Test def test_valueLength_2() { runner2.runOneTest("valueLength_2") }
  @Test def test_valueLength_3() { runner2.runOneTest("valueLength_3") }
  @Test def test_valueLength_4() { runner2.runOneTest("valueLength_4") }
  @Test def test_valueLength_5() { runner2.runOneTest("valueLength_5") }
  @Test def test_valueLength_sde() { runner2.runOneTest("valueLength_sde") }
  @Test def test_valueLength_unparse_0() { runner2.runOneTest("valueLength_unparse_0") }
  // DFDL-1516:dfdl:contentLength & dfdl:valueLength specifying lengthUnits 'characters' and variable-width encodings
  @Test def test_valueLength_unparse_1() { runner2.runOneTest("valueLength_unparse_1") }
  @Test def test_valueLength_unparse_2() { runner2.runOneTest("valueLength_unparse_2") }
  @Test def test_valueLength_unparse_3() { runner2.runOneTest("valueLength_unparse_3") }
  @Test def test_valueLength_unparse_4() { runner2.runOneTest("valueLength_unparse_4") }

  @Test def test_contentLength_0() { runner2.runOneTest("contentLength_0") }
  @Test def test_contentLength_1() { runner2.runOneTest("contentLength_1") }

  @Test def test_valueContentLength1() { runner2.runOneTest("valueContentLength1") }
  @Test def test_valueContentLength2() { runner2.runOneTest("valueContentLength2") }

  @Test def test_fn_not_declared() { runner2b.runOneTest("fn_not_declared") }
  @Test def test_fn_not_declared_2() { runner2b.runOneTest("fn_not_declared_2") }

  // DFDL-313
  // Verified that we do get an error regarding an improperly formatted
  // DFDL expression.  This test needs its own file since it fails at the
  // schema loading (SAXParse) level and would cause other tests within
  // the same file to fail.
  //
  @Test def test_no_closing_brace() { runner3.runOneTest("no_closing_brace") } // no closing } for expression

  @Test def test_valueLengthPair1() { runner5.runOneTest("valueLengthPair1") }
  @Test def test_valueLengthPair2() { runner5.runOneTest("valueLengthPair2") }
  @Test def test_valueLengthPair3() { runner5.runOneTest("valueLengthPair3") }
  @Test def test_valueLengthAndOccurs1() { runner5.runOneTest("valueLengthAndOccurs1") }

  // Added from scala-new for DFDL-1198
  @Test def test_array_index_oob_01() { runner.runOneTest("array_index_oob_01") }
  @Test def test_array_index_oob_02() { runner.runOneTest("array_index_oob_02") }
  @Test def test_array_index_oob_03() { runner.runOneTest("array_index_oob_03") }
  @Test def test_array_index_oob_04() { runner.runOneTest("array_index_oob_04") }
  @Test def test_array_index_oob_05() { runner.runOneTest("array_index_oob_05") }

  // Added from scala-new for DFDL-1660
  @Test def test_array_index_relative_path_subexpression_01() { runner.runOneTest("array_index_relative_path_subexpression_01") }

  //DFDL-1702
  @Test def test_mathPow01 { runner2.runOneTest("mathPow01") }
  @Test def test_mathPow02 { runner2.runOneTest("mathPow02") }
  @Test def test_mathPow03 { runner2.runOneTest("mathPow03") }
  @Test def test_mathPow04 { runner2.runOneTest("mathPow04") }
  @Test def test_mathPow05 { runner2.runOneTest("mathPow05") }
  @Test def test_mathPow06 { runner2.runOneTest("mathPow06") }
  @Test def test_mathPow07 { runner2.runOneTest("mathPow07") }
  @Test def test_mathPow08 { runner2.runOneTest("mathPow08") }
  @Test def test_mathPow09 { runner2.runOneTest("mathPow09") }
  @Test def test_mathPow10 { runner2.runOneTest("mathPow10") }
  @Test def test_mathPow11 { runner2.runOneTest("mathPow11") }
  @Test def test_mathPow12 { runner2.runOneTest("mathPow12") }
  @Test def test_mathPow13 { runner2.runOneTest("mathPow13") }
  @Test def test_mathPow14 { runner2.runOneTest("mathPow14") }
  @Test def test_mathPow15 { runner2.runOneTest("mathPow15") }
  @Test def test_mathPow16 { runner2.runOneTest("mathPow16") }
  @Test def test_mathPow17 { runner2.runOneTest("mathPow17") }
  @Test def test_mathPow18 { runner2.runOneTest("mathPow18") }
  @Test def test_mathPow19 { runner2.runOneTest("mathPow19") }
  @Test def test_mathPow20 { runner2.runOneTest("mathPow20") }
  @Test def test_mathPow21 { runner2.runOneTest("mathPow21") }
  @Test def test_mathPow22 { runner2.runOneTest("mathPow22") }
  @Test def test_mathPow23 { runner2.runOneTest("mathPow23") }
  @Test def test_mathPow24 { runner2.runOneTest("mathPow24") }
  @Test def test_mathPow25 { runner2.runOneTest("mathPow25") }
  @Test def test_mathPow26 { runner2.runOneTest("mathPow26") }
  @Test def test_mathPow27 { runner2.runOneTest("mathPow27") }
  @Test def test_mathPow28 { runner2.runOneTest("mathPow28") }
  @Test def test_mathPow29 { runner2.runOneTest("mathPow29") }
  @Test def test_mathPow30 { runner2.runOneTest("mathPow30") }
  @Test def test_mathPow31 { runner2.runOneTest("mathPow31") }
  @Test def test_mathPow32 { runner2.runOneTest("mathPow32") }
  @Test def test_mathPow33 { runner2.runOneTest("mathPow33") }
  @Test def test_mathPow34 { runner2.runOneTest("mathPow34") }

  // DFDL-1771
  @Test def test_expr_path_past_root1 { runner7.runOneTest("test_expr_path_past_root1") }

  // DFDL-1804
  @Test def test_traceComplex { runner7.runOneTest("traceComplex") }

}
