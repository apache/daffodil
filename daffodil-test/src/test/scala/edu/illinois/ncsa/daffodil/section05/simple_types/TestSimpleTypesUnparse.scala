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

package edu.illinois.ncsa.daffodil.section05.simple_types

import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit._

object TestSimpleTypesUnparse {

  val testDir = "/edu/illinois/ncsa/daffodil/section05/simple_types/"

  val runner = Runner(testDir, "SimpleTypesUnparse.tdml")

  @AfterClass def shutDown() {
    runner.reset
  }
}

class TestSimpleTypesUnparse {
  import TestSimpleTypesUnparse._
 
  //DFDL-1454
  //dfdl:hexBinary not behaving like xs:hexBinary 
  //@Test def test_hexBinary_unparse_13() { runner.runOneTest("hexBinary_unparse_13") }
  
  //DFDL-1455
  //result doesn't match example in the spec
  //@Test def test_hexBinary_unparse_17() { runner.runOneTest("hexBinary_unparse_17") }
  
  @Test def test_hexBinary_unparse_01() { runner.runOneTest("hexBinary_unparse_01") }
  @Test def test_hexBinary_unparse_02() { runner.runOneTest("hexBinary_unparse_02") }
  @Test def test_hexBinary_unparse_03() { runner.runOneTest("hexBinary_unparse_03") }
  @Test def test_hexBinary_unparse_04() { runner.runOneTest("hexBinary_unparse_04") }
  @Test def test_hexBinary_unparse_05() { runner.runOneTest("hexBinary_unparse_05") }
  @Test def test_hexBinary_unparse_06() { runner.runOneTest("hexBinary_unparse_06") }
  @Test def test_hexBinary_unparse_07() { runner.runOneTest("hexBinary_unparse_07") }
  @Test def test_hexBinary_unparse_08() { runner.runOneTest("hexBinary_unparse_08") }
  @Test def test_hexBinary_unparse_09() { runner.runOneTest("hexBinary_unparse_09") }
  @Test def test_hexBinary_unparse_10() { runner.runOneTest("hexBinary_unparse_10") }
  @Test def test_hexBinary_unparse_11() { runner.runOneTest("hexBinary_unparse_11") }
  @Test def test_hexBinary_unparse_12() { runner.runOneTest("hexBinary_unparse_12") }
  @Test def test_hexBinary_unparse_14() { runner.runOneTest("hexBinary_unparse_14") }
  @Test def test_hexBinary_unparse_15() { runner.runOneTest("hexBinary_unparse_15") }
  @Test def test_hexBinary_unparse_16() { runner.runOneTest("hexBinary_unparse_16") }
  @Test def test_hexBinary_unparse_18() { runner.runOneTest("hexBinary_unparse_18") }
  @Test def test_hexBinary_unparse_19() { runner.runOneTest("hexBinary_unparse_19") }

  @Test def test_hexBinary_variable_unparse_01() { runner.runOneTest("hexBinary_variable_unparse_01") }
  @Test def test_hexBinary_variable_unparse_02() { runner.runOneTest("hexBinary_variable_unparse_02") }
  @Test def test_hexBinary_variable_unparse_03() { runner.runOneTest("hexBinary_variable_unparse_03") }
  @Test def test_hexBinary_variable_unparse_04() { runner.runOneTest("hexBinary_variable_unparse_04") }
}
