package edu.illinois.ncsa.daffodil.section16.array_optional_elem

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

import junit.framework.Assert._
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File
import org.junit.AfterClass

object TestArrayOptionalElemNew {

  val testDir = "/edu/illinois/ncsa/daffodil/section16/array_optional_elem/"
  //  val aa_fixed = testDir + "UnparseArrayFixedOptionalElem.tdml"
  //  var runner_fixed = new DFDLTestSuite(Misc.getRequiredResource(aa_fixed))
  //
  //  val aa_imp = testDir + "UnparseArrayImplicitOptionalElem.tdml"
  //  var runner_imp = new DFDLTestSuite(Misc.getRequiredResource(aa_imp))
  //
  //  val aa_parsed = testDir + "UnparseArrayParsedOptionalElem.tdml"
  //  var runner_parsed = new DFDLTestSuite(Misc.getRequiredResource(aa_parsed))
  //
  //  val aa_expr = testDir + "UnparseArrayExpressionConstant.tdml"
  //  var runner_expr = new DFDLTestSuite(Misc.getRequiredResource(aa_expr))

  val aa_delim = testDir + "UnparseArrayDelimitedOptionalElem.tdml"
  var runner_delim = new DFDLTestSuite(Misc.getRequiredResource(aa_delim))

  /**
   * Avoid memory leak of adding more and more test suites to static objects as we run more and more test suites.
   */
  @AfterClass def tearDown() {
    //    runner_fixed = null
    //    runner_imp = null
    //    runner_parsed = null
    //    runner_expr = null
    runner_delim = null
  }

}

class TestArrayOptionalElemNew {

  import TestArrayOptionalElemNew._

  // DFDL-1254
  @Test def test_delimOptPresent() { runner_delim.runOneTest("delimOptPresent") }
  @Test def test_delimOptPresentArray() { runner_delim.runOneTest("delimOptPresentArray") }
  @Test def test_delimOptPresentArrayMax2() { runner_delim.runOneTest("delimOptPresentArrayMax2") }
  @Test def test_delimOptAbsentArray() { runner_delim.runOneTest("delimOptAbsentArray") }
  @Test def test_delimOptTwoArrays() { runner_delim.runOneTest("delimOptTwoArrays") }

}
