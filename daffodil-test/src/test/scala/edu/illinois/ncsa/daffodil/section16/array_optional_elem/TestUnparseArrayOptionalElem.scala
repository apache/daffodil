/* Copyright (c) 2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.section16.array_optional_elem

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
import org.junit._

object TestUnparseArrayOptionalElem {
  
  val testDir = "/edu/illinois/ncsa/daffodil/section16/array_optional_elem/"
  val aa_fixed = testDir + "UnparseArrayFixedOptionalElem.tdml"
  var runner_fixed = new DFDLTestSuite(Misc.getRequiredResource(aa_fixed))

  val aa_imp = testDir + "UnparseArrayImplicitOptionalElem.tdml"
  var runner_imp = new DFDLTestSuite(Misc.getRequiredResource(aa_imp))

  val aa_parsed = testDir + "UnparseArrayParsedOptionalElem.tdml"
  var runner_parsed = new DFDLTestSuite(Misc.getRequiredResource(aa_parsed))

  val aa_expr = testDir + "UnparseArrayExpressionConstant.tdml"
  var runner_expr = new DFDLTestSuite(Misc.getRequiredResource(aa_expr))

  /**
   * Avoid memory leak of adding more and more test suites to static objects as we run more and more test suites.
   */
  @AfterClass def tearDown() { 
    runner_fixed = null 
    runner_imp = null
    runner_parsed = null
    runner_expr = null
  }

}

class TestUnparseArrayOptionalElem {

  import TestUnparseArrayOptionalElem._
  
  //DFDL-1296
  //@Test def test_exprOptPresent() { runner_expr.runOneTest("exprOptPresent") }
  //@Test def test_exprOptPresentArray() { runner_expr.runOneTest("exprOptPresentArray") }
  //@Test def test_exprOptAbsentArray() { runner_expr.runOneTest("exprOptAbsentArray") }
  //@Test def test_exprOptTwoArrays() { runner_expr.runOneTest("exprOptTwoArrays") }
  //@Test def test_exprOptScalarThenArray() { runner_expr.runOneTest("exprOptScalarThenArray") }
  //@Test def test_exprOptArrayThenScalar() { runner_expr.runOneTest("exprOptArrayThenScalar") }
  
  //DFDL-1301
  //@Test def test_fixedUnparseArrayTooManyElements01() { runner_fixed.runOneTest("fixedUnparseArrayTooManyElements01") }
  
  //DFDL-1301 - ticket to improve diagnostic. test will need to be updated.
  @Test def test_fixedUnparseArrayTooFewElements01() { runner_fixed.runOneTest("fixedUnparseArrayTooFewElements01") }
  @Test def test_impOptScalarThenArray03() { runner_imp.runOneTest("impOptScalarThenArray03") }
  @Test def test_impOptArrayThenScalar03() { runner_imp.runOneTest("impOptArrayThenScalar03") }
  
  //DFDL-1302
  //@Test def test_parsedOptArrayThenScalar03() { runner_parsed.runOneTest("parsedOptArrayThenScalar03") }

  @Test def test_fixedOptPresent() { runner_fixed.runOneTest("fixedOptPresent") }
  @Test def test_fixedOptPresentArray() { runner_fixed.runOneTest("fixedOptPresentArray") }
  @Test def test_fixedOptAbsentArray() { runner_fixed.runOneTest("fixedOptAbsentArray") }
  @Test def test_fixedOptTwoArrays() { runner_fixed.runOneTest("fixedOptTwoArrays") }
  @Test def test_fixedOptScalarThenArray() { runner_fixed.runOneTest("fixedOptScalarThenArray") }
  @Test def test_fixedOptArrayThenScalar() { runner_fixed.runOneTest("fixedOptArrayThenScalar") }

  @Test def test_impOptPresent() { runner_imp.runOneTest("impOptPresent") }
  @Test def test_impOptPresentArray() { runner_imp.runOneTest("impOptPresentArray") }
  @Test def test_impOptPresentArrayMax2() { runner_imp.runOneTest("impOptPresentArrayMax2") }
  @Test def test_impOptAbsentArray() { runner_imp.runOneTest("impOptAbsentArray") }
  @Test def test_impOptTwoArrays() { runner_imp.runOneTest("impOptTwoArrays") }

  @Test def test_impOptScalarThenArray() { runner_imp.runOneTest("impOptScalarThenArray") }
  @Test def test_impOptScalarThenArray02() { runner_imp.runOneTest("impOptScalarThenArray02") }

  @Test def test_impOptArrayThenScalar() { runner_imp.runOneTest("impOptArrayThenScalar") }
  @Test def test_impOptArrayThenScalar02() { runner_imp.runOneTest("impOptArrayThenScalar02") }

  @Test def test_parsedOptPresent() { runner_parsed.runOneTest("parsedOptPresent") }
  @Test def test_parsedOptPresentArray() { runner_parsed.runOneTest("parsedOptPresentArray") }
  @Test def test_parsedOptAbsentArray() { runner_parsed.runOneTest("parsedOptAbsentArray") }
  @Test def test_parsedOptTwoArrays() { runner_parsed.runOneTest("parsedOptTwoArrays") }

  @Test def test_parsedOptScalarThenArray() { runner_parsed.runOneTest("parsedOptScalarThenArray") }
  @Test def test_parsedOptScalarThenArray02() { runner_parsed.runOneTest("parsedOptScalarThenArray02") }
  @Test def test_parsedOptArrayThenScalar() { runner_parsed.runOneTest("parsedOptArrayThenScalar") }
  @Test def test_parsedOptArrayThenScalar02() { runner_parsed.runOneTest("parsedOptArrayThenScalar02") }

}
