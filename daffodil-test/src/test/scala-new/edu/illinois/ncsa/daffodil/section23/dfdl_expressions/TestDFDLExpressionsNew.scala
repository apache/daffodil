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

  //DFDL-1706
  @Test def test_valueLengthDfdlLength { runner6.runOneTest("valueLengthDfdlLength") }
  @Test def test_valueLengthDfdlOccursCount { runner6.runOneTest("valueLengthDfdlOccursCount") }
  @Test def test_valueLengthDfdlEncoding { runner6.runOneTest("valueLengthDfdlEncoding") }

  //DFDL-1702
  @Test def test_mathPow01 { runner2.runOneTest("mathPow01") }
  @Test def test_mathPow02 { runner2.runOneTest("mathPow02") }
  @Test def test_mathPow03 { runner2.runOneTest("mathPow03") }
  @Test def test_mathPow04 { runner2.runOneTest("mathPow04") }
  @Test def test_mathPow05 { runner2.runOneTest("mathPow05") }
  @Test def test_mathPow06 { runner2.runOneTest("mathPow06") }
  @Test def test_mathPow07 { runner2.runOneTest("mathPow07") }
  @Test def test_mathPow08 { runner2.runOneTest("mathPow08") }
  //@Test def test_mathPow09 { runner2.runOneTest("mathPow09") }
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
}
