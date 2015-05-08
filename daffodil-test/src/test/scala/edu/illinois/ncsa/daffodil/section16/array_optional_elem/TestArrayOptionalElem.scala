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
import edu.illinois.ncsa.daffodil.debugger.InteractiveDebugger
import edu.illinois.ncsa.daffodil.debugger.TraceDebuggerRunner
import edu.illinois.ncsa.daffodil.dsom.ExpressionCompiler

object TestArrayOptionalElem {
  private val testDir = "/edu/illinois/ncsa/daffodil/section16/array_optional_elem/"
  private val aa = testDir + "ArrayOptionalElem.tdml"

  private var runnerv: DFDLTestSuite = null

  def runner = {
    if (runnerv == null) runnerv = new DFDLTestSuite(Misc.getRequiredResource(aa))
    runnerv
  }

  private val testDir01 = "/edu/illinois/ncsa/daffodil/section05/facets/"
  private val ab = testDir01 + "Facets.tdml"
  private var runner01v: DFDLTestSuite = null
  def runner01 = {
    if (runner01v == null) runner01v = new DFDLTestSuite(Misc.getRequiredResource(ab), validateTDMLFile = false)
    runner01v
  }

  private val testDir1 = "/edu/illinois/ncsa/daffodil/ibm-tests/"
  private val tdml1 = testDir1 + "dpaext2.tdml"
  private var runner1v: DFDLTestSuite = null
  def runner1 = {
    if (runner1v == null) runner1v = new DFDLTestSuite(Misc.getRequiredResource(tdml1))
    runner1v
  }

  private val tdmlBack = testDir + "backtracking.tdml"
  private var rBackv: DFDLTestSuite = null
  def rBack = {
    if (rBackv == null) rBackv = new DFDLTestSuite(Misc.getRequiredResource(tdmlBack))
    rBackv
  }

  def dbg = {
    Debugger.setDebugger(new InteractiveDebugger(new TraceDebuggerRunner, ExpressionCompiler))
    Debugger.withTracing(false)
    // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
  }
}

class TestArrayOptionalElem {

  import TestArrayOptionalElem._

  @Test def test_arrayExpressions01() { runner.runOneTest("arrayExpressions01") }
  @Test def test_arrayExpressions02() { runner.runOneTest("arrayExpressions02") }
  @Test def test_arrayExpressions02b() { runner.runOneTest("arrayExpressions02b") }
  @Test def test_arrayExpressions02c() { runner.runOneTest("arrayExpressions02c") }
  @Test def test_arrayExpressions02d() { runner.runOneTest("arrayExpressions02d") }
  //  @Test def test_arrayExpressions03() { runner.runOneTest("arrayExpressions03") }
  @Test def test_arrayExpressions04() { runner.runOneTest("arrayExpressions04") }
  @Test def test_arrayExpressions05() { runner.runOneTest("arrayExpressions05") }

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

  @Test def test_leftOverData_Neg() { runner01.runOneTest("leftOverData_Neg") }

  @Test def test_arrays_16_01() { runner1.runOneTest("arrays_16_01") }

  @Test def test_backtrack1Text() = { rBack.runOneTest("backtrack1Text") }
}
