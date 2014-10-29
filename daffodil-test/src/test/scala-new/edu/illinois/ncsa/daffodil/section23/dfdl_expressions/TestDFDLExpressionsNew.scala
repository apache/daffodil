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

  val testDir3 = "/edu/illinois/ncsa/daffodil/section23/runtime_properties/"
  val rp = testDir3 + "runtime-properties.tdml"
  lazy val runner3 = new DFDLTestSuite(Misc.getRequiredResource(rp))

  val testDir2 = "/edu/illinois/ncsa/daffodil/section23/dfdl_functions/"
  val aa = testDir2 + "Functions.tdml"
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(aa))

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

  val tdml2 = testDir + "functions.tdml"
  lazy val runner_fun = new DFDLTestSuite(Misc.getRequiredResource(tdml2))

  @Test def test_substringFunction01() { runner_fun.runOneTest("substringFunction01") }
  @Test def test_substringFunction02() { runner_fun.runOneTest("substringFunction02") }
  @Test def test_substringFunction03() { runner_fun.runOneTest("substringFunction03") }
  @Test def test_substringFunction04() { runner_fun.runOneTest("substringFunction04") }
  @Test def test_substringFunction05() { runner_fun.runOneTest("substringFunction05") }
  @Test def test_substringFunction06() { runner_fun.runOneTest("substringFunction06") }
  @Test def test_substringFunction07() { runner_fun.runOneTest("substringFunction07") }
  @Test def test_substringFunction08() { runner_fun.runOneTest("substringFunction08") }
  // Fails fn:substring((), 1, 3) returns "".
  //    @Test def test_substringFunction09() { runner_fun.runOneTest("substringFunction09") }
  @Test def test_substringFunction10() { runner_fun.runOneTest("substringFunction10") }
  @Test def test_substringFunction11() { runner_fun.runOneTest("substringFunction11") }
  @Test def test_substringFunction12() { runner_fun.runOneTest("substringFunction12") }
  @Test def test_substringFunction13() { runner_fun.runOneTest("substringFunction13") }
  @Test def test_substringFunction14() { runner_fun.runOneTest("substringFunction14") }
  @Test def test_substringFunction15() { runner_fun.runOneTest("substringFunction15") }

}
