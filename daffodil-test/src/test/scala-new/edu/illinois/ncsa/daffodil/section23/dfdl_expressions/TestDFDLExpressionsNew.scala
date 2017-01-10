/* Copyright (c) 2012-2016 Tresys Technology, LLC. All rights reserved.
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

import org.junit.Test
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite

object TestDFDLExpressionsNew {

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

  val testDir4 = "/edu/illinois/ncsa/daffodil/section23/dfdl_expressions/"
  val tdml4 = testDir4 + "expressions.tdml"
  lazy val runner4 = new DFDLTestSuite(Misc.getRequiredResource(tdml4))

  val tdml5 = testDir4 + "expressions2.tdml"
  lazy val runner5 = new DFDLTestSuite(Misc.getRequiredResource(tdml5))

  val tdml6 = testDir + "valueLength.tdml"
  lazy val runner6 = new DFDLTestSuite(Misc.getRequiredResource(tdml6))
}

class TestDFDLExpressionsNew {
  import TestDFDLExpressionsNew._

  //DFDL-1076
  @Test def test_nilled_01() { runner2.runOneTest("nilled_01") }
  //DFDL-1233
  @Test def test_nilled_02() { runner2.runOneTest("nilled_02") }
  @Test def test_nilled_03() { runner2.runOneTest("nilled_03") }

  // DFDL-1669
  @Test def test_dfdl_1669_unsignedLong_conversion() { runner5.runOneTest("test_dfdl_1669_unsignedLong_conversion") }

  //DFDL-1657
  @Test def test_valueLengthRef1 { runner6.runOneTest("valueLengthRef1") }
}
