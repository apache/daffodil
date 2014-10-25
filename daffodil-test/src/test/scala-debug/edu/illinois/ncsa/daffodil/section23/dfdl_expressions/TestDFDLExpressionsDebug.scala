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

class TestDFDLExpressionsDebug {
  val testDir = "/edu/illinois/ncsa/daffodil/section23/dfdl_expressions/"
  val tdml = testDir + "expressions.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_internal_space_preserved2() { runner.runOneTest("internal_space_preserved2") }
  @Test def test_internal_space_preserved3a() { runner.runOneTest("internal_space_preserved3a") }
  @Test def test_internal_space_preserved3b() { runner.runOneTest("internal_space_preserved3b") }

  @Test def test_regexCompatFail() { runner.runOneTest("regexCompatFail") }

  // lengthUnits bytes with variable-width charater set and specified lengthKind
  @Test def test_lke3_rel() { runner.runOneTest("lke3_rel") } // uses lengthUnits bytes with utf-8 

  val testDir2 = "/edu/illinois/ncsa/daffodil/section23/dfdl_functions/"
  val aa = testDir2 + "Functions.tdml"
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(aa))

  //DFDL-1076
  @Test def test_not_04() { runner2.runOneTest("not_04") }

  //These tests should work once DPath is pushed up
  @Test def test_abs_07() { runner2.runOneTest("abs_07") }
  @Test def test_ceil_06() { runner2.runOneTest("ceil_06") }
  @Test def test_ceil_08() { runner2.runOneTest("ceil_08") }

  //  DFDL-819
  @Test def test_integer_constructor_05() { runner2.runOneTest("integer_constructor_05") }
  @Test def test_integer_constructor_06() { runner2.runOneTest("integer_constructor_06") }

  // DFDL-827
  @Test def test_time_constructor_01() { runner2.runOneTest("time_constructor_01") }
  @Test def test_date_constructor_02() { runner2.runOneTest("date_constructor_02") }
  @Test def test_date_constructor_04() { runner2.runOneTest("date_constructor_04") }
  @Test def test_xsDateTime_constructor_01() { runner2.runOneTest("xsDateTime_constructor_01") }
  @Test def test_xsDateTime_constructor_03() { runner2.runOneTest("xsDateTime_constructor_03") }
  @Test def test_fnDateTime_constructor_01() { runner2.runOneTest("fnDateTime_constructor_01") }
  @Test def test_fnDateTime_constructor_02() { runner2.runOneTest("fnDateTime_constructor_02") }
  @Test def test_fnDateTime_constructor_03() { runner2.runOneTest("fnDateTime_constructor_03") }

  // DFDL-710
  @Test def test_testBit_3() { runner2.runOneTest("testBit_3") }

  // DFDL-581
  @Test def test_valueLength_0() { runner2.runOneTest("valueLength_0") }
  @Test def test_valueLength_1() { runner2.runOneTest("valueLength_1") }

  // DFDL-578
  @Test def test_contentLength_0() { runner2.runOneTest("contentLength_0") }
  @Test def test_contentLength_1() { runner2.runOneTest("contentLength_1") }

  @Test def test_substringbefore_04() { runner2.runOneTest("substringbefore_04") }
  @Test def test_substringbefore_05() { runner2.runOneTest("substringbefore_05") }
  @Test def test_substringafter_04() { runner2.runOneTest("substringafter_04") }
  @Test def test_substringafter_05() { runner2.runOneTest("substringafter_05") }

  val testDir4 = "/edu/illinois/ncsa/daffodil/section23/runtime_properties/"
  val rp = testDir4 + "runtime-properties.tdml"
  lazy val runner4 = new DFDLTestSuite(Misc.getRequiredResource(rp))

  @Test def test_element_long_form_whitespace() { runner.runOneTest("element_long_form_whitespace") }
}
