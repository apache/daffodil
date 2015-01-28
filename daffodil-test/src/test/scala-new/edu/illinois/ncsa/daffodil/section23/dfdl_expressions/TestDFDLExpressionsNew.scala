/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.section23.dfdl_expressions

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

  //DFDL-1198
  @Test def test_array_index_oob_01() { runner.runOneTest("array_index_oob_01") }
  @Test def test_array_index_oob_02() { runner.runOneTest("array_index_oob_02") }
  @Test def test_array_index_oob_03() { runner.runOneTest("array_index_oob_03") }
  @Test def test_array_index_oob_04() { runner.runOneTest("array_index_oob_04") }
  @Test def test_array_index_oob_05() { runner.runOneTest("array_index_oob_05") }

  //DFDL-1059
  @Test def test_parent_axis_01() { runner.runOneTest("parent_axis_01") }
  @Test def test_child_axis_01() { runner.runOneTest("child_axis_01") }

}
