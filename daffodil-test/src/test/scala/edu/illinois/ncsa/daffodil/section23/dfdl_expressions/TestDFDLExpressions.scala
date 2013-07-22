package edu.illinois.ncsa.daffodil.section23.dfdl_expressions

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import junit.framework.Assert._
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File
import edu.illinois.ncsa.daffodil.debugger.Debugger

class TestDFDLExpressions {
  val testDir = "/edu/illinois/ncsa/daffodil/section23/dfdl_expressions/"
  
  // I'm not sure these belong in section23, but there is no section of the spec that 
  // is about all these properties together, yet, since there is common mechanism here
  // I really think their tests should not be scattered all over to the sections where each
  // property is defined.
  val testDir4 = "/edu/illinois/ncsa/daffodil/section23/runtime_properties/"
  val rp = testDir4 + "runtime-properties.tdml"
  lazy val runner4 = new DFDLTestSuite(Misc.getRequiredResource(rp))

  @Test def test_byteOrderExpr1 { runner4.runOneTest("byteOrderExpr1") }
//  @Test def test_byteOrderExpr1b { runner4.runOneTest("byteOrderExpr1b") }
  @Test def test_byteOrderExpr2 { runner4.runOneTest("byteOrderExpr2") }
  @Test def test_byteOrderExpr3 { runner4.runOneTest("byteOrderExpr3") }
  @Test def test_byteOrderExpr4 { runner4.runOneTest("byteOrderExpr4") }
  @Test def test_byteOrderExpr5 { runner4.runOneTest("byteOrderExpr5") }
  @Test def test_byteOrderExpr6 { runner4.runOneTest("byteOrderExpr6") }
  @Test def test_byteOrderExpr7 { runner4.runOneTest("byteOrderExpr7") }
//  @Test def test_byteOrderExpr7b { runner4.runOneTest("byteOrderExpr7b") }
  @Test def test_byteOrderExpr8 { runner4.runOneTest("byteOrderExpr8") }
//  @Test def test_byteOrderExpr9 { runner4.runOneTest("byteOrderExpr9") }

  val tdml = testDir + "expressions.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml), validateTDMLFile = false)

  @Test def test_regexLookahead() { runner.runOneTest("regexLookahead") }
  @Test def test_regexLookaheadFail() { runner.runOneTest("regexLookaheadFail") }
  @Test def test_regexLookaheadFail2() { runner.runOneTest("regexLookaheadFail2") }
  //  @Test def test_regexCompatFail() { runner.runOneTest("regexCompatFail") }

  @Test def test_expressionRules01() { runner.runOneTest("expressionRules01") }
  @Test def test_expressionRules02() { runner.runOneTest("expressionRules02") }

//  DFDL-667  
//  @Test def test_expressionRules03() { runner.runOneTest("expressionRules03") }
//  @Test def test_expressionRules04() { runner.runOneTest("expressionRules04") }

  @Test def test_lke3_rel() { runner.runOneTest("lke3_rel") }
  @Test def test_lke1_rel() { runner.runOneTest("lke1_rel") }
  @Test def test_lke1_abs() { runner.runOneTest("lke1_abs") }
  @Test def test_ocke1() { runner.runOneTest("ocke1") }
  @Test def test_ocke2() { runner.runOneTest("ocke2") }
  @Test def test_ArrayOptElem_01() { runner.runOneTest("ArrayOptElem_01") }
  @Test def test_lke2_rel() { runner.runOneTest("lke2_rel") }
  @Test def test_expression_type_error1() { runner.runOneTest("expression_type_error1") }
  @Test def test_expression_type_error2() { runner.runOneTest("expression_type_error2") }
  @Test def test_expression_type_error3() { runner.runOneTest("expression_type_error3") }
  @Test def test_expression_type_error4() { runner.runOneTest("expression_type_error4") }
  @Test def test_expression_unknown_prefix() { runner.runOneTest("expression_unknown_prefix") }
  @Test def test_ocke_rel() { runner.runOneTest("ocke_rel") }
  @Test def test_ocke_rel2() { runner.runOneTest("ocke_rel2") }
  @Test def test_ocke_rel3() { runner.runOneTest("ocke_rel3") }
  @Test def test_ocke_rel4() { runner.runOneTest("ocke_rel4") }
  @Test def test_internal_space_preserved() { runner.runOneTest("internal_space_preserved") }
//  @Test def test_internal_space_preserved2() { runner.runOneTest("internal_space_preserved2") }
//  @Test def test_internal_space_preserved3a() { runner.runOneTest("internal_space_preserved3a") }
//  @Test def test_internal_space_preserved3b() { runner.runOneTest("internal_space_preserved3b") }

  @Test def test_expresion_bad_path_to_element() { runner.runOneTest("expresion_bad_path_to_element") }
  @Test def test_ArrayOptElem_02() { runner.runOneTest("ArrayOptElem_02") }
  @Test def test_dfdlCheckConstraints() { runner.runOneTest("dfdlCheckConstraints") }

  @Test def test_nonFunctionIsDetected() = { runner.runOneTest("nonFunctionIsDetected") }
  @Test def test_constantFunction1() { runner.runOneTest("constantFunction1") }
  @Test def test_dfdlPosition1() { runner.runOneTest("dfdlPosition1") }
  @Test def test_dfdlPosition2() { runner.runOneTest("dfdlPosition2") }
  @Test def test_dfdlPosition3() { runner.runOneTest("dfdlPosition3") }
  @Test def test_dfdlPosition4() { runner.runOneTest("dfdlPosition4") }
  @Test def test_dfdlPosition5() { runner.runOneTest("dfdlPosition5") }

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

  @Test def test_trueFalseTypeError() { runner.runOneTest("trueFalseTypeError") }
  @Test def test_trueFalseTypeCorrect() { runner.runOneTest("trueFalseTypeCorrect") }

  val tdml2 = testDir + "functions.tdml"
  lazy val runner_fun = new DFDLTestSuite(Misc.getRequiredResource(tdml2))

  @Test def test_dateTimeFunctions01() { runner_fun.runOneTest("dateTimeFunctions01") }
  @Test def test_dateTimeFunctions02() { runner_fun.runOneTest("dateTimeFunctions02") }
  @Test def test_dateFunctions01() { runner_fun.runOneTest("dateFunctions01") }
  @Test def test_dateFunctions02() { runner_fun.runOneTest("dateFunctions02") }
  @Test def test_timeFunctions01() { runner_fun.runOneTest("timeFunctions01") }
  @Test def test_timeFunctions02() { runner_fun.runOneTest("timeFunctions02") }
  @Test def test_functionFail01() { runner_fun.runOneTest("functionFail01") }
  @Test def test_functionFail02() { runner_fun.runOneTest("functionFail02") }
  @Test def test_functionFail03() { runner_fun.runOneTest("functionFail03") }

  val testDir2 = "/edu/illinois/ncsa/daffodil/section23/dfdl_functions/"
  val aa = testDir2 + "Functions.tdml"
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(aa))
  
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

//  DFDL-622  
//  @Test def test_fnDateTime_constructor_01() { runner2.runOneTest("fnDateTime_constructor_01") }
//  @Test def test_fnDateTime_constructor_02() { runner2.runOneTest("fnDateTime_constructor_02") }
//  @Test def test_fnDateTime_constructor_03() { runner2.runOneTest("fnDateTime_constructor_03") }
  @Test def test_fnDateTime_constructor_04() { runner2.runOneTest("fnDateTime_constructor_04") }
  
  @Test def test_integer_constructor_01() { runner2.runOneTest("integer_constructor_01") }
  @Test def test_integer_constructor_02() { runner2.runOneTest("integer_constructor_02") }
  @Test def test_integer_constructor_03() { runner2.runOneTest("integer_constructor_03") }
  @Test def test_integer_constructor_04() { runner2.runOneTest("integer_constructor_04") }
//  DFDL-818
//  @Test def test_integer_constructor_05() { runner2.runOneTest("integer_constructor_05") }
//  @Test def test_integer_constructor_06() { runner2.runOneTest("integer_constructor_06") }
  @Test def test_integer_constructor_07() { runner2.runOneTest("integer_constructor_07") }

  @Test def test_testBit_0() { runner2.runOneTest("testBit_0") }
  @Test def test_testBit_1() { runner2.runOneTest("testBit_1") }
  @Test def test_testBit_2() { runner2.runOneTest("testBit_2") }
  // This test should be giving a runtime SDE instead of parse error (DFDL-710)
  // @Test def test_testBit_3() { runner2.runOneTest("testBit_3") }

  @Test def test_stringLiteralFromString_0() { runner2.runOneTest("stringLiteralFromString_0") }
  @Test def test_stringLiteralFromString_1() { runner2.runOneTest("stringLiteralFromString_1") }
  @Test def test_stringLiteralFromString_2() { runner2.runOneTest("stringLiteralFromString_2") }

  @Test def test_setBits_0() { runner2.runOneTest("setBits_0") }
  @Test def test_setBits_1() { runner2.runOneTest("setBits_1") }
  @Test def test_setBits_2() { runner2.runOneTest("setBits_2") }

  @Test def test_containsEntity_0() { runner2.runOneTest("containsEntity_0") }
  @Test def test_containsEntity_1() { runner2.runOneTest("containsEntity_1") }
  @Test def test_containsEntity_2() { runner2.runOneTest("containsEntity_2") }
  @Test def test_containsEntity_3() { runner2.runOneTest("containsEntity_3") }
  @Test def test_containsEntity_4() { runner2.runOneTest("containsEntity_4") }

  @Test def test_occursCount_0() { runner2.runOneTest("occursCount_0") }
  @Test def test_occursCount_1() { runner2.runOneTest("occursCount_1") }
//  @Test def test_occursCount_1b() { runner2.runOneTest("occursCount_1b") }
  @Test def test_occursCount_2() { runner2.runOneTest("occursCount_2") }
  @Test def test_occursCount_3() { runner2.runOneTest("occursCount_3") }
//  @Test def test_occursCount_3b() { runner2.runOneTest("occursCount_3b") }

  //  @Test def test_valueLength_0() { runner2.runOneTest("valueLength_0") }
  //  @Test def test_valueLength_1() { runner2.runOneTest("valueLength_1") }

  //  @Test def test_contentLength_0() { runner2.runOneTest("contentLength_0") }
  //  @Test def test_contentLength_1() { runner2.runOneTest("contentLength_1") }
  
  @Test def test_xPathFunc_abs_01() { runner2.runOneTest("xPathFunc_abs_01") }
  @Test def test_xPathFunc_abs_02() { runner2.runOneTest("xPathFunc_abs_02") }
  @Test def test_xPathFunc_abs_03() { runner2.runOneTest("xPathFunc_abs_03") }
  @Test def test_xPathFunc_abs_04() { runner2.runOneTest("xPathFunc_abs_04") }
  
  @Test def test_xPathFunc_ceil_01() { runner2.runOneTest("xPathFunc_ceil_01") }
  @Test def test_xPathFunc_ceil_02() { runner2.runOneTest("xPathFunc_ceil_02") }
  @Test def test_xPathFunc_ceil_03() { runner2.runOneTest("xPathFunc_ceil_03") }
  @Test def test_xPathFunc_ceil_04() { runner2.runOneTest("xPathFunc_ceil_04") }
  @Test def test_xPathFunc_ceil_05() { runner2.runOneTest("xPathFunc_ceil_05") }
  
  @Test def test_xPathFunc_floor_01() { runner2.runOneTest("xPathFunc_floor_01") }
  @Test def test_xPathFunc_floor_02() { runner2.runOneTest("xPathFunc_floor_02") }
  @Test def test_xPathFunc_floor_03() { runner2.runOneTest("xPathFunc_floor_03") }
  @Test def test_xPathFunc_floor_04() { runner2.runOneTest("xPathFunc_floor_04") }
  @Test def test_xPathFunc_floor_05() { runner2.runOneTest("xPathFunc_floor_05") }
  
  @Test def test_xPathFunc_round_01() { runner2.runOneTest("xPathFunc_round_01") }
  @Test def test_xPathFunc_round_02() { runner2.runOneTest("xPathFunc_round_02") }
  @Test def test_xPathFunc_round_03() { runner2.runOneTest("xPathFunc_round_03") }
  @Test def test_xPathFunc_round_04() { runner2.runOneTest("xPathFunc_round_04") }
  @Test def test_xPathFunc_round_05() { runner2.runOneTest("xPathFunc_round_05") }
  @Test def test_xPathFunc_round_06() { runner2.runOneTest("xPathFunc_round_06") }
  
  @Test def test_xPathFunc_round_hte_01() { runner2.runOneTest("xPathFunc_round_hte_01") }
  @Test def test_xPathFunc_round_hte_02() { runner2.runOneTest("xPathFunc_round_hte_02") }
//  @Test def test_xPathFunc_round_hte_03() { runner2.runOneTest("xPathFunc_round_hte_03") }
  @Test def test_xPathFunc_round_hte_04() { runner2.runOneTest("xPathFunc_round_hte_04") }
  @Test def test_xPathFunc_round_hte_05() { runner2.runOneTest("xPathFunc_round_hte_05") }
  @Test def test_xPathFunc_round_hte_06() { runner2.runOneTest("xPathFunc_round_hte_06") }
//  @Test def test_xPathFunc_round_hte_07() { runner2.runOneTest("xPathFunc_round_hte_07") }
//  @Test def test_xPathFunc_round_hte_08() { runner2.runOneTest("xPathFunc_round_hte_08") }
  
  val testDir2b = "/edu/illinois/ncsa/daffodil/section23/dfdl_functions/"
  val aab = testDir2b + "Functions-neg.tdml"
  lazy val runner2b = new DFDLTestSuite(Misc.getRequiredResource(aab))

  @Test def test_fn_not_declared() { runner2b.runOneTest("fn_not_declared") }
  @Test def test_fn_not_declared_2() { runner2b.runOneTest("fn_not_declared_2") }
  
  val tdml3 = testDir + "expression_fail.tdml"
  lazy val runner3 = new DFDLTestSuite(Misc.getRequiredResource(tdml3), validateTDMLFile = false)

  // DFDL-313
  // Verified that we do get an error regarding an improperly formatted
  // DFDL expression.  This test needs its own file since it fails at the
  // schema loading (SAXParse) level and would cause other tests within
  // the same file to fail.
  //
  @Test def test_no_closing_brace() { runner3.runOneTest("no_closing_brace") } // no closing } for expression

}
