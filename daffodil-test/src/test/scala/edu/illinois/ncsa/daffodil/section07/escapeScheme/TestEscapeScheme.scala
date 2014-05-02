package edu.illinois.ncsa.daffodil.section07.escapeScheme

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

class TestEscapeScheme {
  val testDir = "/edu/illinois/ncsa/daffodil/section07/escapeScheme/"
  val tdml = testDir + "escapeScheme.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml),
    validateTDMLFile = false)

  // Debug Template
  // @Test def test_name() = Debugger.withDebugger { 
  // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
  // runner.runOneTest("test_name") 
  // }

  @Test def test_escapeSchemeSimple() { runner.runOneTest("escapeSchemeSimple") }
  @Test def test_escapeSchemeFail() { runner.runOneTest("escapeSchemeFail") }
  @Test def test_escapeSchemeFail2() { runner.runOneTest("escapeSchemeFail2") }
  @Test def test_escapeSchemeFail3() { runner.runOneTest("escapeSchemeFail3") }
  @Test def test_escapeSchemeEmpty() { runner.runOneTest("escapeSchemeEmpty") }
  @Test def test_escapeSchemeNonEmpty() { runner.runOneTest("escapeSchemeNonEmpty") }
  @Test def test_escapeSchemeUnused() { runner.runOneTest("escapeSchemeUnused") }

  @Test def test_escapeExpressions_01() { runner.runOneTest("escapeExpressions_01") }
  @Test def test_escapeExpressions_01b() { runner.runOneTest("escapeExpressions_01b") }
  @Test def test_escapeExpressions_02() { runner.runOneTest("escapeExpressions_02") }
  @Test def test_escapeExpressions_03() { runner.runOneTest("escapeExpressions_03") }
  @Test def test_escapeExpressions_04() { runner.runOneTest("escapeExpressions_04") }
  @Test def test_escapeExpressions_05() { runner.runOneTest("escapeExpressions_05") }
  @Test def test_escapeExpressions_06() { runner.runOneTest("escapeExpressions_06") }

  val tdmlNeg = testDir + "escapeSchemeNeg.tdml"
  lazy val runnerNeg = new DFDLTestSuite(Misc.getRequiredResource(tdmlNeg),
    validateTDMLFile = false)

  @Test def test_escapeSchemeNeg() { runnerNeg.runOneTest("escapeSchemeNeg") }

  val bb = testDir + "escapeScenarios.tdml"
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(bb), validateTDMLFile = false)

  @Test def test_scenario1_1() { runner2.runOneTest("scenario1_1") }
  @Test def test_scenario1_2() { runner2.runOneTest("scenario1_2") }
  @Test def test_scenario1_3() { runner2.runOneTest("scenario1_3") }
  @Test def test_scenario1_4() { runner2.runOneTest("scenario1_4") }
  @Test def test_scenario1_5() { runner2.runOneTest("scenario1_5") }
  @Test def test_scenario1_6() { runner2.runOneTest("scenario1_6") }
  @Test def test_scenario1_7() { runner2.runOneTest("scenario1_7") }
  @Test def test_scenario1_7_postfix() { runner2.runOneTest("scenario1_7_postfix") }
  @Test def test_scenario1_8() { runner2.runOneTest("scenario1_8") }
  @Test def test_scenario1_8_req_term() { runner2.runOneTest("scenario1_8_req_term") }
  @Test def test_scenario1_9() { runner2.runOneTest("scenario1_9") }
  @Test def test_scenario1_9_postfix() { runner2.runOneTest("scenario1_9_postfix") }
  @Test def test_scenario1_10() { runner2.runOneTest("scenario1_10") }
  @Test def test_scenario1_10_postfix() { runner2.runOneTest("scenario1_10_postfix") }
  @Test def test_scenario1_11() { runner2.runOneTest("scenario1_11") }
  @Test def test_scenario1_11_postfix() { runner2.runOneTest("scenario1_11_postfix") }
  @Test def test_scenario1_12() { runner2.runOneTest("scenario1_12") }
  @Test def test_scenario1_12_postfix() { runner2.runOneTest("scenario1_12_postfix") }
  @Test def test_scenario1_13() { runner2.runOneTest("scenario1_13") }
  @Test def test_scenario1_13_postfix() { runner2.runOneTest("scenario1_13_postfix") }

  @Test def test_scenario2_1() { runner2.runOneTest("scenario2_1") }
  @Test def test_scenario2_2() { runner2.runOneTest("scenario2_2") }
  @Test def test_scenario2_3() { runner2.runOneTest("scenario2_3") }
  @Test def test_scenario2_4() { runner2.runOneTest("scenario2_4") }
  @Test def test_scenario2_5() { runner2.runOneTest("scenario2_5") }
  @Test def test_scenario2_6() { runner2.runOneTest("scenario2_6") }
  @Test def test_scenario2_7() { runner2.runOneTest("scenario2_7") }
  @Test def test_scenario2_8() { runner2.runOneTest("scenario2_8") }
  @Test def test_scenario2_9() { runner2.runOneTest("scenario2_9") }
  @Test def test_scenario2_10() { runner2.runOneTest("scenario2_10") }
  @Test def test_scenario2_10_postfix() { runner2.runOneTest("scenario2_10_postfix") }
  @Test def test_scenario2_11() { runner2.runOneTest("scenario2_11") }
  @Test def test_scenario2_11_req_term() { runner2.runOneTest("scenario2_11_req_term") }
  @Test def test_scenario2_12() { runner2.runOneTest("scenario2_12") }
  @Test def test_scenario2_12_postfix() { runner2.runOneTest("scenario2_12_postfix") }
  @Test def test_scenario2_13() { runner2.runOneTest("scenario2_13") }
  @Test def test_scenario2_13_postfix() { runner2.runOneTest("scenario2_13_postfix") }
  @Test def test_scenario2_14() { runner2.runOneTest("scenario2_14") }
  @Test def test_scenario2_14_req_term() { runner2.runOneTest("scenario2_14_req_term") }

  @Test def test_scenario3_1() { runner2.runOneTest("scenario3_1") }
  @Test def test_scenario3_2() { runner2.runOneTest("scenario3_2") }
  @Test def test_scenario3_3() { runner2.runOneTest("scenario3_3") }
  @Test def test_scenario3_4() { runner2.runOneTest("scenario3_4") }
  @Test def test_scenario3_5() { runner2.runOneTest("scenario3_5") }
  @Test def test_scenario3_6() { runner2.runOneTest("scenario3_6") }
  @Test def test_scenario3_7() { runner2.runOneTest("scenario3_7") }
  @Test def test_scenario3_8() { runner2.runOneTest("scenario3_8") }
  @Test def test_scenario3_9() { runner2.runOneTest("scenario3_9") }
  @Test def test_scenario3_10() { runner2.runOneTest("scenario3_10") }
  @Test def test_scenario3_10_postfix() { runner2.runOneTest("scenario3_10_postfix") }
  @Test def test_scenario3_11() { runner2.runOneTest("scenario3_11") }
  @Test def test_scenario3_12() { runner2.runOneTest("scenario3_12") }
  @Test def test_scenario3_12_req_term() { runner2.runOneTest("scenario3_12_req_term") }
  @Test def test_scenario3_13() { runner2.runOneTest("scenario3_13") }
  @Test def test_scenario3_13_postfix() { runner2.runOneTest("scenario3_13_postfix") }
  @Test def test_scenario3_14() { runner2.runOneTest("scenario3_14") }
  @Test def test_scenario3_14_req_term() { runner2.runOneTest("scenario3_14_req_term") }

  @Test def test_scenario4_1() { runner2.runOneTest("scenario4_1") }
  @Test def test_scenario4_2() { runner2.runOneTest("scenario4_2") }
  @Test def test_scenario4_3() { runner2.runOneTest("scenario4_3") }
  @Test def test_scenario4_4() { runner2.runOneTest("scenario4_4") }
  @Test def test_scenario4_5() { runner2.runOneTest("scenario4_5") }
  @Test def test_scenario4_6() { runner2.runOneTest("scenario4_6") }
  @Test def test_scenario4_7() { runner2.runOneTest("scenario4_7") }
  @Test def test_scenario4_7_req_term() { runner2.runOneTest("scenario4_7_req_term") }
  @Test def test_scenario4_8() { runner2.runOneTest("scenario4_8") }
  @Test def test_scenario4_8_postfix() { runner2.runOneTest("scenario4_8_postfix") }
  @Test def test_scenario4_9() { runner2.runOneTest("scenario4_9") }
  @Test def test_scenario4_9_req_term() { runner2.runOneTest("scenario4_9_req_term") }
  @Test def test_scenario4_10() { runner2.runOneTest("scenario4_10") }
  @Test def test_scenario4_10_req_term() { runner2.runOneTest("scenario4_10_req_term") }
  @Test def test_scenario4_11() { runner2.runOneTest("scenario4_11") }
  @Test def test_scenario4_11_postfix() { runner2.runOneTest("scenario4_11_postfix") }
  @Test def test_scenario4_12() { runner2.runOneTest("scenario4_12") }
  @Test def test_scenario4_12_req_term() { runner2.runOneTest("scenario4_12_req_term") }

}
