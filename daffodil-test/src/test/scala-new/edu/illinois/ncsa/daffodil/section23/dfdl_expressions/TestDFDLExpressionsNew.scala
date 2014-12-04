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

class TestDFDLExpressionsNew {

  val testDir = "/edu/illinois/ncsa/daffodil/section23/dfdl_expressions/"

  val tdml = testDir + "expressions.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  val testDir3 = "/edu/illinois/ncsa/daffodil/section23/runtime_properties/"
  val rp = testDir3 + "runtime-properties.tdml"
  lazy val runner3 = new DFDLTestSuite(Misc.getRequiredResource(rp))

  val testDir2 = "/edu/illinois/ncsa/daffodil/section23/dfdl_functions/"
  val aa = testDir2 + "Functions.tdml"
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(aa))

  val tdml2 = testDir + "functions.tdml"
  lazy val runner_fun = new DFDLTestSuite(Misc.getRequiredResource(tdml2))

  @Test def test_xPathFunc_round_14() { runner2.runOneTest("xPathFunc_round_14") }
  @Test def test_xPathFunc_round_15() { runner2.runOneTest("xPathFunc_round_15") }

  //DFDL-1116
  @Test def test_count_03() { runner2.runOneTest("count_03") }

  @Test def test_internal_space_preserved2() { runner.runOneTest("internal_space_preserved2") }
  @Test def test_internal_space_preserved3a() { runner.runOneTest("internal_space_preserved3a") }
  @Test def test_internal_space_preserved3b() { runner.runOneTest("internal_space_preserved3b") }

  //DFDL-1079
  @Test def test_empty_05() { runner2.runOneTest("empty_05") }
  @Test def test_exists_05() { runner2.runOneTest("exists_05") }

  //DFDL-1198
  @Test def test_array_index_oob_01() { runner.runOneTest("array_index_oob_01") }
  @Test def test_array_index_oob_02() { runner.runOneTest("array_index_oob_02") }
  @Test def test_array_index_oob_03() { runner.runOneTest("array_index_oob_03") }
  @Test def test_array_index_oob_04() { runner.runOneTest("array_index_oob_04") }
  @Test def test_array_index_oob_05() { runner.runOneTest("array_index_oob_05") }

  //DFDL-1171
  @Test def test_comparison_operators_23() { runner.runOneTest("comparison_operators_23") }
  @Test def test_comparison_operators_24() { runner.runOneTest("comparison_operators_24") }
  @Test def test_comparison_operators_25() { runner.runOneTest("comparison_operators_25") }
  @Test def test_comparison_operators_26() { runner.runOneTest("comparison_operators_26") }

  //DFDL-1180 
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

  // DFDL-1180 from XPath Spec Sec 10.4.6.1 Examples
  @Test def test_comparison_operators_47() { runner.runOneTest("comparison_operators_47") }
  @Test def test_comparison_operators_48() { runner.runOneTest("comparison_operators_48") }
  @Test def test_comparison_operators_49() { runner.runOneTest("comparison_operators_49") }
  @Test def test_comparison_operators_50() { runner.runOneTest("comparison_operators_50") }
  @Test def test_comparison_operators_51() { runner.runOneTest("comparison_operators_51") }
  @Test def test_comparison_operators_52() { runner.runOneTest("comparison_operators_52") }
  @Test def test_comparison_operators_53() { runner.runOneTest("comparison_operators_53") }

}
