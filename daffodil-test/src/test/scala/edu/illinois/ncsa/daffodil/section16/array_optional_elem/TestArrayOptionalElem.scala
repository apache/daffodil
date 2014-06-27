package edu.illinois.ncsa.daffodil.section16.array_optional_elem

/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

class TestArrayOptionalElem {
  val testDir = "/edu/illinois/ncsa/daffodil/section16/array_optional_elem/"
  val aa = testDir + "ArrayOptionalElem.tdml"

  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(aa))

  @Test def test_arrayExpressions01() { runner.runOneTest("arrayExpressions01") }
  // DFDL-886
  //  @Test def test_arrayExpressions02() { runner.runOneTest("arrayExpressions02") }
  @Test def test_arrayExpressions02b() { runner.runOneTest("arrayExpressions02b") }
  @Test def test_arrayExpressions02c() { runner.runOneTest("arrayExpressions02c") }
  @Test def test_arrayExpressions02d() { runner.runOneTest("arrayExpressions02d") }
  //  @Test def test_arrayExpressions03() { runner.runOneTest("arrayExpressions03") }
  @Test def test_arrayExpressions04() { runner.runOneTest("arrayExpressions04") }

  @Test def test_error01() { runner.runOneTest("error01") }
  @Test def test_postfixNoErr() { runner.runOneTest("postfixNoErr") }

  @Test def test_optionalElem() { runner.runOneTest("optionalElem") }
  @Test def test_optionalWithSeparators() { runner.runOneTest("optionalWithSeparators") }
  @Test def test_Lesson6_optional_element() { runner.runOneTest("Lesson6_optional_element") }
  @Test def test_Lesson6_optional_element_01() { runner.runOneTest("Lesson6_optional_element_01") }
  @Test def test_Lesson6_fixed_array() { runner.runOneTest("Lesson6_fixed_array") }
  @Test def test_Lesson6_variable_array() { runner.runOneTest("Lesson6_variable_array") }
  @Test def test_Lesson6_variable_array_01() { runner.runOneTest("Lesson6_variable_array_01") }
  @Test def test_Lesson6_variable_array_02() { runner.runOneTest("Lesson6_variable_array_02") }

  val testDir01 = "/edu/illinois/ncsa/daffodil/section05/facets/"
  val ab = testDir01 + "Facets.tdml"
  lazy val runner01 = new DFDLTestSuite(Misc.getRequiredResource(ab), validateTDMLFile = false)
  @Test def test_leftOverData_Neg() { runner01.runOneTest("leftOverData_Neg") }

  val testDir1 = "/edu/illinois/ncsa/daffodil/ibm-tests/"
  val tdml1 = testDir1 + "dpaext2.tdml"
  lazy val runner1 = new DFDLTestSuite(Misc.getRequiredResource(tdml1))
  @Test def test_arrays_16_01() { runner1.runOneTest("arrays_16_01") }
  
  val tdmlBack = testDir + "backtracking.tdml"
  lazy val rBack = new DFDLTestSuite(Misc.getRequiredResource(tdmlBack))

  @Test def test_backtrack1() = { rBack.runOneTest("backtrack1") }
}
