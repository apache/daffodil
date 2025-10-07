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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Ignore
import org.junit.Test

object TestDFDLExpressions extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section23/dfdl_expressions/expressions.tdml"
}

object TestDFDLExpressions2 extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section23/dfdl_expressions/expressions2.tdml"
}

object TestDFDLExpressions2CompileAll extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section23/dfdl_expressions/expressions2.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, compileAllTopLevel = true)
}

object TestDFDLExpressionsFail extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section23/dfdl_expressions/expression_fail.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, validateTDMLFile = false)
}

object TestDFDLExpressionsNoValidate extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section23/dfdl_expressions/expressions.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, validateDFDLSchemas = false)
}

object TestDFDLFunctions extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section23/dfdl_functions/Functions.tdml"
}

object TestDFDLFunctions2 extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section23/dfdl_expressions/functions.tdml"
}

object TestDFDLFunctionsNeg extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section23/dfdl_functions/Functions-neg.tdml"
}

object TestDFDLFunctionsUTF8 extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section23/dfdl_functions/Functions_UTF8.tdml"
}

object TestRuntimeProperties extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section23/runtime_properties/runtime-properties.tdml"
  override def createRunner() =
    Runner(tdmlDir, tdmlFile, validateTDMLFile = true, validateDFDLSchemas = false)
}

object TestValueLength extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section23/dfdl_expressions/valueLength.tdml"
}

class TestDFDLExpressions extends TdmlTests {
  val tdmlSuite = TestDFDLExpressions

  // DFDL-1111
  @Ignore @Test def diagnostics_01 = test
  @Ignore @Test def diagnostics_02 = test
  @Ignore @Test def diagnostics_03 = test

  @Test def sequenceReturned_01 = test
  @Test def sequenceReturned_02 = test
  @Test def sequenceReturned_03 = test
  @Test def longPath_01 = test
  @Test def longPath_02 = test

  @Test def hiddenDataExpression = test
  @Test def hiddenDataExpression2 = test

  @Test def arrayIndexOutOfBounds_01 = test
  @Test def arrayIndexOutOfBounds_02 = test
  @Test def arrayIndexOutOfBounds_03 = test

  @Test def arrayIndexOutOfBounds_05 = test

  @Test def asterisk_01 = test
  @Test def asterisk_02 = test
  @Test def asterisk_03 = test

  // DFDL-1146
  @Ignore @Test def attribute_axis_01 = test
  @Ignore @Test def attribute_axis_02 = test
  @Ignore @Test def attribute_axis_03 = test

  @Test def comparison_operators_01 = test
  @Test def comparison_operators_02 = test
  @Test def comparison_operators_03 = test
  @Test def comparison_operators_04 = test
  @Test def comparison_operators_05 = test
  @Test def comparison_operators_06 = test
  @Test def comparison_operators_07 = test
  @Test def comparison_operators_08 = test
  @Test def comparison_operators_09 = test
  @Test def comparison_operators_10 = test
  @Test def comparison_operators_11 = test
  @Test def comparison_operators_12 = test
  @Test def comparison_operators_13 = test
  @Test def comparison_operators_14 = test
  @Test def comparison_operators_15 = test
  @Test def comparison_operators_16 = test
  @Test def comparison_operators_17 = test
  @Test def comparison_operators_18 = test
  @Test def comparison_operators_19 = test
  @Test def comparison_operators_20 = test
  @Test def comparison_operators_21 = test
  @Test def comparison_operators_22 = test
  @Test def comparison_operators_23 = test
  @Test def comparison_operators_24 = test
  @Test def comparison_operators_25 = test
  @Test def comparison_operators_26 = test
  @Test def comparison_operators_27 = test
  @Test def comparison_operators_28 = test

  @Test def comparison_operators_29 = test
  @Test def comparison_operators_30 = test
  @Test def comparison_operators_31 = test
  @Test def comparison_operators_32 = test
  @Test def comparison_operators_33 = test
  @Test def comparison_operators_34 = test
  @Test def comparison_operators_35 = test
  @Test def comparison_operators_36 = test
  @Test def comparison_operators_37 = test
  @Test def comparison_operators_38 = test
  @Test def comparison_operators_39 = test
  @Test def comparison_operators_40 = test
  @Test def comparison_operators_41 = test
  @Test def comparison_operators_42 = test
  @Test def comparison_operators_43 = test
  @Test def comparison_operators_44 = test
  @Test def comparison_operators_45 = test
  @Test def comparison_operators_46 = test
  @Test def comparison_operators_46a = test

  // from XPath Spec Sec 10.4.6.1 Examples
  @Test def comparison_operators_47 = test
  @Test def comparison_operators_48 = test
  @Test def comparison_operators_49 = test
  @Test def comparison_operators_50 = test
  @Test def comparison_operators_51 = test
  @Test def comparison_operators_52 = test
  @Test def comparison_operators_53 = test

  @Test def comparison_operators_54 = test
  @Test def comparison_operators_55 = test
  @Test def comparison_operators_56 = test
  @Test def comparison_operators_57 = test
  @Test def comparison_operators_58 = test
  @Test def comparison_operators_59 = test
  @Test def comparison_operators_60 = test
  @Test def comparison_operators_61 = test
  @Test def comparison_operators_62 = test
  @Test def comparison_operators_63 = test
  @Test def comparison_operators_64 = test
  @Test def comparison_operators_65 = test
  @Test def comparison_operators_66 = test
  @Test def comparison_operators_67 = test
  @Test def comparison_operators_68 = test
  @Test def comparison_operators_69 = test
  @Test def comparison_operators_70 = test
  @Test def comparison_operators_71 = test
  @Test def comparison_operators_72 = test
  @Test def comparison_operators_73 = test
  @Test def comparison_operators_74 = test
  @Test def comparison_operators_75 = test
  @Test def comparison_operators_76 = test
  @Test def comparison_operators_77 = test
  @Test def comparison_operators_78 = test
  @Test def comparison_operators_79 = test
  @Test def comparison_operators_80 = test
  @Test def comparison_operators_81 = test
  @Test def comparison_operators_82 = test
  @Test def comparison_operators_83 = test

  @Test def regexCompatFail = test

  @Test def expressionRules01 = test
  @Test def expressionRules02 = test
  @Test def expressionRules03 = test
  @Test def expressionRules04 = test
  @Test def expressionRules05 = test
  @Test def expressionRules06 = test

  // uses lengthUnits bytes with utf-8 and lengthKind Explicit
  @Test def lke3_rel = test

  @Test def lke1_rel = test
  @Test def lke1_abs = test
  @Test def ocke_from_string_01 = test
  @Test def ocke_from_string_02 = test
  @Test def ocke1 = test
  @Test def ocke2 = test
  @Test def ArrayOptElem_01 = test
  @Test def lke2_rel = test
  @Test def expression_type_error1 = test
  @Test def expression_type_error2 = test
  @Test def expression_type_error3 = test
  @Test def expression_type_error4 = test
  @Test def expression_type_error5 = test
  @Test def expression_type_error6 = test
  @Test def expression_type_error7 = test
  @Test def expression_unknown_prefix = test
  @Test def ocke_rel = test
  @Test def ocke_rel2 = test
  @Test def ocke_rel3 = test
  @Test def ocke_rel4 = test
  @Test def ocke_step_dne = test
  @Test def ocke_array_index_step_dne = test
  @Test def ocke_non_upward = test
  @Test def ocke_single_upward = test
  @Test def ocke_optional_separator_01 = test
  @Test def ocke_optional_separator_02 = test
  @Test def ocke_optional_separator_03 = test
  @Test def ocke_optional_separator_04 = test

  @Test def internal_space_preserved = test
  @Test def internal_space_preserved2 = test
  @Test def internal_space_preserved3a = test
  @Test def internal_space_preserved3b = test
  @Test def internal_space_preserved4 = test
  @Test def internal_space_not_preserved1 = test
  @Test def internal_space_not_preserved2 = test

  @Test def whitespace_expression = test
  @Test def whitespace_expression2 = test

  @Test def expresion_bad_path_to_element = test
  @Test def ArrayOptElem_02 = test

  @Test def dfdlSelfReferencingExpression1 = test
  @Test def dfdlSelfReferencingExpression2 = test
  @Test def dfdlSelfReferencingExpression3 = test

  @Test def repeatFlags1 = test
  @Test def repeatFlags2 = test
  @Test def repeatFlags3 = test
  @Test def repeatFlags4 = test
  @Test def repeatFlags5 = test

  @Test def repeatBitFlags1 = test
  @Test def repeatBitFlags2 = test
  @Test def repeatBitFlags3 = test
  @Test def repeatBitFlags4 = test
  @Test def repeatBitFlags5 = test
  @Test def repeatBitFlags6 = test

  @Test def invalid_enum_1 = test
  @Test def invalid_enum_2 = test
  @Test def invalid_enum_3 = test

  // Test removed including TDML for it. DPath no longer will even execution expressions that have
  // type errors, and type errors at runtime cause SDE so you can't if-then-else them into
  // some usable thing.
  @Ignore @Test def trueFalseTypeError = test
  @Test def trueFalseTypeCorrect = test

  @Test def predicate_01 = test
  // DFDL-1164
  @Ignore @Test def predicate_02 = test
  @Ignore @Test def predicate_03 = test
  @Test def predicate_04 = test
  @Test def predicate_05 = test
  @Test def predicate_06 = test

  @Test def sequential_and_01 = test
  @Test def sequential_and_02 = test
  @Test def sequential_and_03 = test
  @Test def sequential_and_04 = test
  @Test def sequential_and_05 = test
  @Test def sequential_or_01 = test
  @Test def sequential_or_02 = test
  @Test def sequential_or_03 = test
  @Test def sequential_or_04 = test
  @Test def sequential_or_05 = test
  @Test def sequential_and_or_01 = test
  @Test def sequential_and_or_02 = test
  @Test def sequential_and_or_03 = test
  @Test def sequential_and_or_04 = test
  @Test def sequential_and_or_05 = test
  @Test def sequential_and_or_06 = test
  @Test def sequential_and_or_07 = test

  // DFDL-1059
  @Test def parent_axis_01 = test
  @Test def child_axis_01 = test
  @Test def self_axis_01 = test
  @Ignore @Test def multiple_axis_01 = test

  @Test def attribute_axis_04 = test

  @Test def ancestor_axis_01 = test
  @Test def ancestor_or_self_axis_01 = test
  @Test def descendant_axis_01 = test
  @Test def descendant_or_self_axis_01 = test
  @Test def following_axis_01 = test
  @Test def following_sibling_axis_01 = test
  @Test def namespace_axis_01 = test
  @Test def preceding_axis_01 = test
  @Test def preceding_sibling_axis_01 = test

  // Added from scala-new for DFDL-1198
  @Test def array_index_oob_01 = test
  @Test def array_index_oob_02 = test
  @Test def array_index_oob_03 = test
  @Test def array_index_oob_04 = test
  @Test def array_index_oob_05 = test

  // Added from scala-new for DFDL-1660
  @Test def array_index_relative_path_subexpression_01 = test

  // DFDL-1221
  @Test def beyondRoot_01 = test

  // DFDL-711
  @Test def short_parent_axis_01 = test

  @Test def element_long_form_whitespace = test

  // DFDL-1691
  @Test def div01 = test
  @Test def div02 = test
  @Test def div03 = test
  @Test def div04 = test
  @Test def div05 = test
  @Test def div06 = test
  @Test def div07 = test
  @Test def div08 = test
  @Test def div09 = test
  @Test def div10 = test
  @Test def div11 = test
  @Test def div12 = test
  @Test def div13 = test
  @Test def div14 = test
  @Test def div15 = test
  @Test def div16 = test
  @Test def div17 = test
  @Test def div18 = test
  @Test def div19 = test
  @Test def div20 = test
  @Test def div21 = test
  @Test def div22 = test
  @Test def div23 = test
  @Test def div24 = test

  @Test def idiv01 = test
  @Test def idiv02 = test
  @Test def idiv03 = test
  @Test def idiv04 = test
  @Test def idiv05 = test
  @Test def idiv06 = test
  @Test def idiv07 = test
  @Test def idiv08 = test
  @Test def idiv09 = test
  @Test def idiv10 = test
  @Test def idiv11 = test
  @Test def idiv12 = test
  @Test def idiv13 = test
  @Test def idiv14 = test
  @Test def idiv15 = test
  @Test def idiv16 = test
  @Test def idiv17 = test
  @Test def idiv18 = test
  @Test def idiv19 = test
  @Test def idiv20 = test

  @Test def DFDLCheckRange_01 = test
  @Test def DFDLCheckRange_02 = test
  @Test def DFDLCheckRange_03 = test
  @Test def DFDLCheckRange_04 = test
  @Test def DFDLCheckRange_05 = test
  @Test def DFDLCheckRange_06 = test
  @Test def DFDLCheckRange_07 = test
  @Test def DFDLCheckRange_08 = test

  @Test def hexBinaryComparison_01 = test
  @Test def hexBinaryComparison_02 = test
  @Test def hexBinaryComparison_03 = test
  @Test def hexBinaryComparison_04 = test
  @Test def hexBinaryComparison_05 = test

  @Test def add01 = test
  @Test def add02 = test
  @Test def add03 = test
  @Test def add04 = test

  @Test def mul01 = test
  @Test def mul02 = test
  @Test def mul03 = test

  @Test def mod01 = test
  @Test def mod02 = test
  @Test def mod03 = test
  @Test def mod04 = test

  // DFDL-1617 - should detect errors due to query-style expressions
  @Test def query_style_01 = test
  @Test def query_style_02 = test
}

class TestDFDLExpressions2 extends TdmlTests {
  val tdmlSuite = TestDFDLExpressions2

  // DFDL-1669
  @Test def test_dfdl_1669_unsignedLong_conversion = test

  // DFDL-1719
  @Test def if_expression_type_01 = test
  @Test def if_expression_type_02 = test
  @Test def if_expression_type_03 = test
  @Test def if_expression_type_04 = test
  @Test def if_expression_type_05 = test
  @Test def if_expression_type_06 = test

  // DFDL-1771
  @Test def test_expr_path_past_root1 = test

  // DFDL-1804
  @Test def traceComplex = test

  // DFDL-2628
  @Test def traceReturnType = test
}

class TestDFDLExpressions2CompileAll extends TdmlTests {
  val tdmlSuite = TestDFDLExpressions2CompileAll

  @Test def unused_path_no_context_01 = test
}

class TestDFDLExpressionsFail extends TdmlTests {
  val tdmlSuite = TestDFDLExpressionsFail

  // DFDL-313
  // Verified that we do get an error regarding an improperly formatted
  // DFDL expression.  This test needs its own file since it fails at the
  // schema loading (SAXParse) level and would cause other tests within
  // the same file to fail.
  //
  @Test def no_closing_brace = test

}

class TestDFDLExpressionsNoValidate extends TdmlTests {
  val tdmlSuite = TestDFDLExpressionsNoValidate

  @Test def regexLookahead = test
  @Test def regexLookaheadFail = test
  @Test def regexLookaheadFail2 = test

  @Test def nonFunctionIsDetected = test
  @Test def constantFunction1 = test
  @Test def dfdlPosition1 = test
  @Test def dfdlPosition2 = test
  @Test def dfdlPosition3 = test
  @Test def dfdlPosition4 = test
  @Test def dfdlPosition5 = test
}

class TestDFDLFunctions extends TdmlTests {
  val tdmlSuite = TestDFDLFunctions

  @Test def boolFunctionChoice_01 = test
  @Test def boolFunctionChoice_02 = test
  @Test def boolFunctionChoice_03 = test

  @Test def boolFlags_01 = test
  @Test def boolFlags_02 = test
  @Test def boolFlags_03 = test
  @Test def boolFlags_04 = test
  @Test def boolFlags_05 = test

  @Test def not_01 = test
  @Test def not_02 = test
  @Test def not_03 = test
  // DFDL-1076
  @Ignore @Test def not_04 = test
  // DFDL-1075
  @Ignore @Test def not_05 = test
  @Ignore @Test def not_07 = test
  @Test def not_06 = test
  @Test def not_08 = test
  @Test def not_09 = test
  @Test def not_10 = test
  @Test def not_11 = test
  @Test def not_12 = test
  @Test def not_13 = test
  @Test def not_14 = test
  @Test def not_15 = test
  @Test def not_16 = test

  @Test def xPathFunc_abs_01 = test
  @Test def xPathFunc_abs_02 = test
  @Test def xPathFunc_abs_03 = test
  @Test def xPathFunc_abs_04 = test
  @Test def abs_05 = test
  @Test def abs_06 = test
  @Test def abs_07 = test
  @Test def abs_08 = test
  @Test def abs_09 = test

  @Test def xPathFunc_ceil_01 = test
  @Test def xPathFunc_ceil_02 = test
  @Test def xPathFunc_ceil_03 = test
  @Test def xPathFunc_ceil_04 = test
  @Test def xPathFunc_ceil_05 = test
  @Test def ceil_06 = test
  @Test def ceil_07 = test
  @Test def ceil_08 = test
  @Test def ceil_09 = test

  @Test def xPathFunc_floor_01 = test
  @Test def xPathFunc_floor_02 = test
  @Test def xPathFunc_floor_03 = test
  @Test def xPathFunc_floor_04 = test
  @Test def xPathFunc_floor_05 = test
  @Test def floor_06 = test
  @Test def floor_07 = test
  @Test def floor_08 = test
  @Test def floor_09 = test

  @Test def xPathFunc_round_01 = test
  @Test def xPathFunc_round_02 = test
  @Test def xPathFunc_round_03 = test
  @Test def xPathFunc_round_04 = test
  @Test def xPathFunc_round_05 = test
  @Test def xPathFunc_round_06 = test
  @Test def round_07 = test
  @Test def round_08 = test
  @Test def round_09 = test
  @Test def round_10 = test
  @Test def round_11 = test
  @Test def round_12 = test
  @Test def round_13 = test
  @Test def xPathFunc_round_14 = test
  @Test def xPathFunc_round_15 = test
  @Test def xPathFunc_round_16 = test
  @Test def round_17 = test
  @Test def round_18 = test
  @Test def round_19 = test

  @Test def xPathFunc_round_hte_01 = test
  @Test def xPathFunc_round_hte_02 = test
  @Test def xPathFunc_round_hte_03 = test
  @Test def xPathFunc_round_hte_04 = test
  @Test def xPathFunc_round_hte_05 = test
  @Test def xPathFunc_round_hte_06 = test
  @Test def xPathFunc_round_hte_07 = test
  @Test def xPathFunc_round_hte_08 = test
  @Test def round_hte_09 = test
  @Test def round_hte_10 = test
  @Test def round_hte_11 = test
  @Test def round_hte_12 = test
  @Test def round_hte_13 = test
  @Test def round_hte_14 = test
  @Test def round_hte_15 = test
  @Test def round_hte_16 = test
  @Test def round_hte_17 = test
  @Test def round_hte_18 = test

  // DAFFODIL-1080
  @Test def empty_02 = test
  @Test def exists_02 = test

  @Test def count_05b = test

  // DFDL-1097
  @Ignore @Test def local_name_06 = test

  @Test def empty_01 = test
  @Test def empty_03 = test
  @Test def empty_04 = test
  @Test def empty_05 = test
  @Test def empty_06 = test
  @Test def empty_07 = test
  @Test def empty_08 = test
  @Test def empty_09 = test

  @Test def exists_01 = test
  @Test def exists_03 = test
  @Test def exists_04 = test
  @Test def exists_06 = test
  @Test def exists_07 = test
  @Test def exists_05 = test
  @Test def exists_08 = test
  @Test def exists_09 = test
  @Test def exists_10 = test
  @Test def exists_11 = test
  @Test def exists_12 = test
  @Test def exists_13 = test
  @Test def exists_14 = test

  // DFDL-1189
  @Ignore @Test def exactly_one_01 = test
  @Ignore @Test def exactly_one_02 = test
  @Ignore @Test def exactly_one_03 = test
  @Test def exactly_one_04 = test
  @Ignore @Test def exactly_one_05 = test
  @Ignore @Test def exactly_one_06 = test

  @Test def count_01 = test
  @Test def count_02 = test
  @Test def count_03 = test
  @Test def count_03b = test
  @Test def count_04 = test
  @Test def count_05 = test
  @Test def count_05c = test
  @Test def count_06 = test
  @Test def count_06b = test
  @Test def count_07 = test
  @Test def count_08 = test
  @Test def count_08b = test
  @Test def count_09 = test
  @Test def count_10 = test
  @Test def count_11 = test

  @Test def count_12 = test
  @Test def count_13 = test
  @Test def count_14 = test
  @Test def count_14b = test
  @Test def count_15 = test
  @Test def count_16 = test
  @Test def count_17 = test

  @Test def count_18 = test

  @Test def count_19 = test

  @Test def count_19b = test

  @Test def count_20 = test

  @Test def count_21 = test

  @Test def count_21b = test

  @Test def local_name_01 = test
  @Test def local_name_02 = test
  @Test def local_name_03 = test
  @Test def local_name_04 = test
  @Test def local_name_05 = test
  // DFDL-1151
  @Ignore @Test def local_name_07 = test

  // DFDL-1101
  @Test def namespace_uri_01 = test
  @Test def namespace_uri_02 = test
  // DFDL-1114
  @Test def namespace_uri_03 = test
  @Test def namespace_uri_04 = test
  @Test def namespace_uri_05 = test
  @Test def namespace_uri_06 = test
  @Test def namespace_uri_07 = test
  @Test def namespace_uri_08 = test

  // DFDL-1233
  @Test def nilled_02 = test
  @Test def nilled_03 = test
  @Test def nilled_04 = test
  @Test def nilled_05 = test
  @Test def nilled_06 = test
  @Test def nilled_07 = test
  @Test def nilled_08 = test
  @Test def nilled_09 = test
  @Test def nilled_10 = test

  @Test def concat_01 = test
  @Test def concat_02 = test
  @Test def concat_03 = test
  @Test def concat_04 = test
  @Test def concat_05 = test

  @Test def substring_01 = test
  @Test def substring_02 = test
  @Test def substring_03 = test
  @Test def substring_04 = test
  @Test def substring_05 = test
  @Test def substring_06 = test
  @Test def substring_07 = test
  @Test def substring_08 = test
  @Test def substring_09 = test
  @Test def substring_10 = test
  @Test def substring_11 = test

  @Test def stringlength_01 = test
  @Test def stringlength_02 = test
  @Test def stringlength_03 = test
  @Test def stringlength_04 = test

  @Test def uppercase_01 = test
  @Test def uppercase_02 = test
  @Test def uppercase_03 = test

  @Test def lowercase_01 = test
  @Test def lowercase_02 = test
  @Test def lowercase_03 = test

  @Test def contains_01 = test
  @Test def contains_02 = test
  @Test def contains_03 = test
  @Test def contains_04 = test
  @Test def contains_05 = test
  @Test def contains_06 = test
  @Test def contains_07 = test

  @Test def startswith_01 = test
  @Test def startswith_02 = test
  @Test def startswith_03 = test
  @Test def startswith_04 = test
  @Test def startswith_05 = test
  @Test def startswith_06 = test
  @Test def startswith_07 = test

  @Test def endswith_01 = test
  @Test def endswith_02 = test
  @Test def endswith_03 = test
  @Test def endswith_04 = test
  @Test def endswith_05 = test
  @Test def endswith_06 = test
  @Test def endswith_07 = test

  @Test def substringbefore_01 = test
  @Test def substringbefore_02 = test
  @Test def substringbefore_03 = test
  @Test def substringbefore_04 = test
  @Test def substringbefore_05 = test
  @Test def substringbefore_06 = test
  @Test def substringbefore_07 = test

  @Test def substringafter_01 = test
  @Test def substringafter_02 = test
  @Test def substringafter_03 = test
  @Test def substringafter_04 = test
  @Test def substringafter_05 = test
  @Test def substringafter_06 = test
  @Test def substringafter_07 = test

  @Test def yearfromdatetime_01 = test
  @Test def yearfromdatetime_02 = test
  @Test def yearfromdatetime_03 = test
  @Test def monthfromdatetime_01 = test
  @Test def monthfromdatetime_02 = test
  @Test def dayfromdatetime_01 = test
  @Test def dayfromdatetime_02 = test
  @Test def hoursfromdatetime_01 = test
  @Test def hoursfromdatetime_02 = test
  @Test def minutesfromdatetime_01 = test
  @Test def minutesfromdatetime_02 = test
  @Test def secondsfromdatetime_01 = test
  @Test def secondsfromdatetime_02 = test
  @Test def secondsfromdatetime_03 = test
  @Test def timezonefromdatetime_01 = test
  @Test def timezonefromdatetime_02 = test

  @Test def xfromdatetime_01 = test
  @Test def xfromdatetime_02 = test
  @Test def xfromdatetime_03 = test
  @Test def xfromdatetime_04 = test

  @Test def yearfromdate_01 = test
  @Test def yearfromdate_02 = test
  @Test def yearfromdate_03 = test
  @Test def monthfromdate_01 = test
  @Test def monthfromdate_02 = test
  @Test def dayfromdate_01 = test
  @Test def dayfromdate_02 = test
  @Test def timezonefromdate_01 = test
  @Test def timezonefromdate_02 = test

  @Test def xfromdate_01 = test
  @Test def xfromdate_02 = test
  @Test def xfromdate_03 = test

  @Test def hoursfromtime_01 = test
  @Test def hoursfromtime_02 = test
  @Test def minutesfromtime_01 = test
  @Test def minutesfromtime_02 = test
  @Test def secondsfromtime_01 = test
  @Test def secondsfromtime_02 = test
  @Test def timezonefromtime_01 = test
  @Test def timezonefromtime_02 = test
  @Test def timezonefromtime_03 = test

  @Test def xfromtime_01 = test
  @Test def xfromtime_02 = test
  @Test def xfromtime_03 = test

  @Test def fn_text_01 = test
  @Test def fn_text_02 = test

  @Test def ubyte_constructor_01 = test
  @Test def ubyte_constructor_02 = test
  @Test def ubyte_constructor_03 = test
  @Test def ubyte_constructor_04 = test

  @Test def uint_constructor_01 = test
  @Test def uint_constructor_02 = test
  @Test def uint_constructor_03 = test
  @Test def uint_constructor_04 = test

  @Test def nonNeg_constructor_01 = test
  @Test def nonNeg_constructor_02 = test
  @Test def nonNeg_constructor_03 = test
  @Test def nonNeg_constructor_04 = test

  @Test def byte_constructor_01 = test
  @Test def byte_constructor_02 = test
  @Test def byte_constructor_03 = test
  @Test def byte_constructor_04 = test

  @Test def hexBinary_constructor_01 = test
  @Test def hexBinary_constructor_02 = test
  @Test def hexBinary_constructor_03 = test
  @Test def hexBinary_constructor_04 = test

  @Test def dfdlHexBinary_constructor_01 = test
  @Test def dfdlHexBinary_constructor_02 = test
  @Test def dfdlHexBinary_constructor_03 = test
  @Test def dfdlHexBinary_constructor_04 = test
  @Test def dfdlHexBinary_constructor_05 = test

  @Test def dfdlByte_constructor_01 = test
  @Test def dfdlByte_constructor_02 = test
  @Test def dfdlByte_constructor_03 = test
  @Test def dfdlByte_constructor_04 = test
  @Test def dfdlByte_constructor_05 = test
  @Test def dfdlByte_constructor_06 = test
  @Test def dfdlByte_constructor_07 = test
  @Test def dfdlByte_constructor_08 = test
  @Test def dfdlByte_constructor_09 = test
  @Test def dfdlByte_constructor_10 = test
  @Test def dfdlByte_constructor_11 = test

  @Test def dfdlUByte_constructor_01 = test
  @Test def dfdlUByte_constructor_02 = test
  @Test def dfdlUByte_constructor_03 = test
  @Test def dfdlUByte_constructor_04 = test
  @Test def dfdlUByte_constructor_05 = test
  @Test def dfdlUByte_constructor_06 = test
  @Test def dfdlUByte_constructor_07 = test
  @Test def dfdlUByte_constructor_08 = test
  @Test def dfdlUByte_constructor_09 = test
  @Test def dfdlUByte_constructor_11 = test
  @Test def dfdlUByte_constructor_12 = test

  @Test def dfdlShort_constructor_01 = test
  @Test def dfdlShort_constructor_02 = test
  @Test def dfdlShort_constructor_03 = test
  @Test def dfdlShort_constructor_04 = test
  @Test def dfdlShort_constructor_05 = test
  @Test def dfdlShort_constructor_06 = test
  @Test def dfdlShort_constructor_07 = test
  @Test def dfdlShort_constructor_08 = test
  @Test def dfdlShort_constructor_09 = test
  @Test def dfdlShort_constructor_11 = test
  @Test def dfdlShort_constructor_12 = test

  @Test def dfdlUShort_constructor_01 = test
  @Test def dfdlUShort_constructor_02 = test
  @Test def dfdlUShort_constructor_03 = test
  @Test def dfdlUShort_constructor_04 = test
  @Test def dfdlUShort_constructor_05 = test
  @Test def dfdlUShort_constructor_06 = test
  @Test def dfdlUShort_constructor_07 = test
  @Test def dfdlUShort_constructor_08 = test
  @Test def dfdlUShort_constructor_09 = test
  @Test def dfdlUShort_constructor_11 = test
  @Test def dfdlUShort_constructor_12 = test

  @Test def dfdlInt_constructor_01 = test
  @Test def dfdlInt_constructor_02 = test
  @Test def dfdlInt_constructor_03 = test
  @Test def dfdlInt_constructor_04 = test
  @Test def dfdlInt_constructor_05 = test
  @Test def dfdlInt_constructor_06 = test
  @Test def dfdlInt_constructor_07 = test
  @Test def dfdlInt_constructor_08 = test
  @Test def dfdlInt_constructor_09 = test
  @Test def dfdlInt_constructor_11 = test
  @Test def dfdlInt_constructor_12 = test
  @Test def dfdlInt_constructor_13 = test
  @Test def dfdlInt_constructor_14 = test
  @Test def dfdlInt_constructor_15 = test

  @Test def dfdlUInt_constructor_01 = test
  @Test def dfdlUInt_constructor_02 = test
  @Test def dfdlUInt_constructor_03 = test
  @Test def dfdlUInt_constructor_04 = test
  @Test def dfdlUInt_constructor_05 = test
  @Test def dfdlUInt_constructor_06 = test
  @Test def dfdlUInt_constructor_07 = test
  @Test def dfdlUInt_constructor_08 = test
  @Test def dfdlUInt_constructor_09 = test
  @Test def dfdlUInt_constructor_11 = test
  @Test def dfdlUInt_constructor_12 = test
  @Test def dfdlUInt_constructor_13 = test

  @Test def dfdlLong_constructor_01 = test
  @Test def dfdlLong_constructor_02 = test
  @Test def dfdlLong_constructor_03 = test
  @Test def dfdlLong_constructor_04 = test
  @Test def dfdlLong_constructor_05 = test
  @Test def dfdlLong_constructor_06 = test
  @Test def dfdlLong_constructor_07 = test
  @Test def dfdlLong_constructor_08 = test
  @Test def dfdlLong_constructor_09 = test
  @Test def dfdlLong_constructor_11 = test
  @Test def dfdlLong_constructor_12 = test
  @Test def dfdlLong_constructor_13 = test

  @Test def dfdlULong_constructor_01 = test
  @Test def dfdlULong_constructor_02 = test
  @Test def dfdlULong_constructor_03 = test
  @Test def dfdlULong_constructor_04 = test
  @Test def dfdlULong_constructor_05 = test
  @Test def dfdlULong_constructor_06 = test
  @Test def dfdlULong_constructor_07 = test
  @Test def dfdlULong_constructor_08 = test
  @Test def dfdlULong_constructor_09 = test
  @Test def dfdlULong_constructor_11 = test
  @Test def dfdlULong_constructor_12 = test
  @Test def dfdlULong_constructor_13 = test

  @Test def xsDateTime_constructor_06 = test
  @Test def xsDateTime_constructor_07 = test
  @Test def xsDateTime_constructor_08 = test
  @Test def xsDateTime_constructor_09 = test
  @Test def xsDateTime_constructor_10 = test
  @Test def date_constructor_05 = test
  @Test def date_constructor_06 = test
  @Test def date_constructor_07 = test
  @Test def date_constructor_08 = test
  @Test def time_constructor_05 = test
  @Test def time_constructor_06 = test
  @Test def time_constructor_07 = test
  @Test def time_constructor_08 = test
  @Test def time_constructor_09 = test

  @Test def time_constructor_01 = test
  @Test def time_constructor_02 = test
  @Test def time_constructor_03 = test
  @Test def time_constructor_04 = test

  // DFDL-1124
  @Test def date_constructor_01 = test
  @Test def date_constructor_02 = test
  @Ignore @Test def date_constructor_02a = test
  @Test def date_constructor_03 = test
  @Ignore @Test def date_constructor_03a = test
  @Test def date_constructor_04 = test
  @Ignore @Test def nonNeg_constructor_02a = test

  @Test def xsDateTime_constructor_01 = test
  @Test def xsDateTime_constructor_02 = test
  // DFDL-1115
  @Ignore @Test def xsDateTime_constructor_03 = test
  @Test def xsDateTime_constructor_04 = test
  @Test def xsDateTime_constructor_05 = test

  @Test def double_constructor_01 = test
  @Test def double_constructor_02 = test
  @Test def double_constructor_03 = test
  @Test def double_constructor_04 = test
  @Test def double_constructor_05 = test
  @Test def double_constructor_06 = test
  @Test def double_constructor_07 = test

  @Test def float_constructor_01 = test
  @Test def float_constructor_02 = test
  @Test def float_constructor_03 = test
  @Test def float_constructor_04 = test

  @Test def decimal_constructor_01 = test
  @Test def decimal_constructor_02 = test
  @Test def decimal_constructor_03 = test
  @Test def decimal_constructor_04 = test
  @Test def decimal_constructor_05 = test
  @Test def decimal_constructor_06 = test

  @Test def short_constructor_01 = test
  @Test def short_constructor_02 = test
  @Test def short_constructor_03 = test
  @Test def short_constructor_04 = test
  @Test def short_constructor_05 = test
  @Test def short_constructor_06 = test

  @Test def ushort_constructor_01 = test
  @Test def ushort_constructor_02 = test
  @Test def ushort_constructor_03 = test
  @Test def ushort_constructor_04 = test
  @Test def ushort_constructor_05 = test

  @Test def ulong_constructor_01 = test
  @Test def ulong_constructor_02 = test
  @Test def ulong_constructor_03 = test
  @Test def ulong_constructor_04 = test
  @Test def ulong_constructor_05 = test
  @Test def ulong_constructor_06 = test
  @Test def ulong_constructor_07 = test

  @Test def long_constructor_01 = test
  @Test def long_constructor_02 = test
  @Test def long_constructor_03 = test
  @Test def long_constructor_04 = test

  @Test def int_constructor_01 = test
  @Test def int_constructor_02 = test
  @Test def int_constructor_03 = test
  @Test def int_constructor_04 = test

  @Test def fnDateTime_constructor_01 = test
  @Test def fnDateTime_constructor_02 = test
  @Test def fnDateTime_constructor_03 = test
  @Test def fnDateTime_constructor_04 = test
  @Test def fnDateTime_constructor_05 = test
  @Test def fnDateTime_constructor_06 = test
  @Test def fnDateTime_constructor_07 = test
  @Test def fnDateTime_constructor_08 = test
  @Test def fnDateTime_constructor_09 = test
  @Test def fnDateTime_constructor_10 = test

  @Test def integer_constructor_01 = test
  @Test def integer_constructor_02 = test
  @Test def integer_constructor_03 = test
  @Test def integer_constructor_04 = test
  @Test def integer_constructor_05 = test
  @Test def integer_constructor_06 = test
  @Test def integer_constructor_07 = test

  @Test def testBit_0 = test
  @Test def testBit_1 = test
  @Test def testBit_2 = test
  @Test def testBit_3 = test
  @Test def testBit_4 = test
  @Test def testBit_5 = test

  @Test def stringLiteralFromString_obsolete = test
  @Test def containsEntity_obsolete = test

  @Test def encodeDFDLEntities_0 = test
  @Test def encodeDFDLEntities_1 = test
  @Test def encodeDFDLEntities_2 = test
  @Test def encodeDFDLEntities_3 = test
  @Test def encodeDFDLEntities_4 = test

  @Test def setBits_0 = test
  @Test def setBits_1 = test
  @Test def setBits_2 = test

  @Test def containsDFDLEntities_0 = test
  @Test def containsDFDLEntities_1 = test
  @Test def containsDFDLEntities_2 = test
  @Test def containsDFDLEntities_3 = test
  @Test def containsDFDLEntities_4 = test

  // DFDL-1118
  @Ignore @Test def more_count_0 = test
  @Ignore @Test def more_count_1 = test
  @Test def more_count_1b = test
  @Ignore @Test def more_count_1b_2 = test
  @Ignore @Test def more_count_2 = test
  @Test def more_count_3 = test
  @Test def more_count_3b = test

  @Test def more_count_4 = test

  @Test def valueLength_0 = test
  @Test def valueLength_1 = test
  // DFDL-1516:dfdl:contentLength & dfdl:valueLength specifying lengthUnits 'characters' and variable-width encodings
  @Test def valueLength_2 = test
  @Test def valueLength_3 = test
  @Test def valueLength_4 = test
  @Test def valueLength_5 = test
  @Test def valueLength_6 = test
  @Test def valueLength_14 = test
  // DAFFODIL-2795
  @Ignore @Test def valueLength_15 = test
  @Test def valueLength_16 = test
  @Test def valueLength_sde = test
  @Test def valueLength_unparse_0 = test
  // DFDL-1516:dfdl:contentLength & dfdl:valueLength specifying lengthUnits 'characters' and variable-width encodings
  @Test def valueLength_unparse_1 = test
  @Test def valueLength_unparse_2 = test
  @Test def valueLength_unparse_3 = test
  @Test def valueLength_unparse_4 = test

  @Test def contentLength_0 = test
  @Test def contentLength_1 = test
  @Test def contentLength_2 = test
  @Test def contentLength_4 = test
  // DAFFODIL-2795
  @Ignore @Test def contentLength_5 = test
  @Test def contentLength_6 = test

  @Test def valueContentLength1 = test
  @Test def valueContentLength2 = test

  // DFDL-1702
  @Test def mathPow01 = test
  @Test def mathPow02 = test
  @Test def mathPow03 = test
  @Test def mathPow04 = test
  @Test def mathPow05 = test
  @Test def mathPow06 = test
  @Test def mathPow07 = test
  @Test def mathPow08 = test
  @Test def mathPow09 = test
  @Test def mathPow10 = test
  @Test def mathPow11 = test
  @Test def mathPow12 = test
  @Test def mathPow13 = test
  @Test def mathPow14 = test
  @Test def mathPow15 = test
  @Test def mathPow16 = test
  @Test def mathPow17 = test
  @Test def mathPow18 = test
  @Test def mathPow19 = test
  @Test def mathPow20 = test
  @Test def mathPow21 = test
  @Test def mathPow22 = test
  @Test def mathPow23 = test
  @Test def mathPow24 = test
  @Test def mathPow25 = test
  @Test def mathPow26 = test
  @Test def mathPow27 = test
  @Test def mathPow28 = test
  @Test def mathPow29 = test
  @Test def mathPow30 = test
  @Test def mathPow31 = test
  @Test def mathPow32 = test
  @Test def mathPow33 = test
  @Test def mathPow34 = test

  // DFDL-1076
  @Test def nilled_01 = test

  @Test def DoubleFromRawLong = test
  @Test def DoubleToRawLong = test
}

class TestDFDLFunctions2 extends TdmlTests {
  val tdmlSuite = TestDFDLFunctions2

  @Test def dateTimeFunctions01 = test
  @Test def dateTimeFunctions02 = test
  @Test def dateFunctions01 = test
  @Test def dateFunctions02 = test
  @Test def timeFunctions01 = test
  @Test def timeFunctions02 = test
  @Test def functionFail01 = test
  @Test def functionFail02 = test
  @Test def functionFail03 = test

  @Test def substringFunction01 = test
  @Test def substringFunction02 = test
  @Test def substringFunction03 = test
  @Test def substringFunction04 = test
  @Test def substringFunction05 = test
  @Test def substringFunction06 = test
  @Test def substringFunction07 = test
  @Test def substringFunction08 = test
  @Test def substringFunction12 = test
  @Test def substringFunction13 = test
  @Test def substringFunction14 = test
  @Test def substringFunction15 = test
}

class TestDFDLFunctionsNeg extends TdmlTests {
  val tdmlSuite = TestDFDLFunctionsNeg

  @Test def fn_not_declared = test
  @Test def fn_not_declared_2 = test
}

class TestDFDLFunctionsUTF8 extends TdmlTests {
  val tdmlSuite = TestDFDLFunctionsUTF8

  @Test def lowercase_04 = test
  @Test def uppercase_04 = test
  @Test def uppercase_05 = test
  @Test def lowercase_05 = test
}

class TestRuntimeProperties extends TdmlTests {
  val tdmlSuite = TestRuntimeProperties

  @Test def variableRefError = test

  @Test def byteOrderExpr1 = test
  @Test def byteOrderExpr1b = test
  @Test def byteOrderExpr2 = test
  @Test def byteOrderExpr2b = test
  @Test def byteOrderExpr3 = test
  @Test def byteOrderExpr4 = test
  @Test def byteOrderExpr5 = test
  @Test def byteOrderExpr6 = test
  @Test def byteOrderExpr7 = test
  @Test def byteOrderExpr7b = test
  @Test def byteOrderExpr7c = test
  @Test def byteOrderExpr8 = test
  @Test def byteOrderExpr9 = test
}

class TestValueLength extends TdmlTests {
  val tdmlSuite = TestValueLength

  @Test def valueLengthPair1 = test
  @Test def valueLengthPair2 = test
  @Test def valueLengthPair3 = test
  @Test def valueLengthAndOccurs1 = test
  //
  // DFDL-1657
  @Test def valueLengthRef1 = test

  // DFDL-1706
  @Test def valueLengthDfdlLength = test
  @Test def valueLengthDfdlOccursCount = test
  @Test def valueLengthDfdlEncoding = test

  // DAFFODIL-2635
  @Test def valueLengthDelimitedHexBinary1 = test
  @Test def valueLengthDelimitedHexBinary2 = test
  @Test def valueLengthDelimitedHexBinary3 = test
  @Test def valueLengthDelimitedHexBinary4 = test
}
