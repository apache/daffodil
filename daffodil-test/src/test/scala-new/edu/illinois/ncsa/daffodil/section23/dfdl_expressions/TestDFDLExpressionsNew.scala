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
  val tdml = testDir + "expressions.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml), validateTDMLFile = false)

  // DFDL-313
  // Believe this is no longer an issue as the spaces in the
  // following tests do not negatively affect the parse.
  //
  @Test def test_ocke_rel2() { runner.runOneTest("ocke_rel2") } // NL expr SP
  @Test def test_ocke_rel3() { runner.runOneTest("ocke_rel3") } // SP expr SP
  @Test def test_ocke_rel4() { runner.runOneTest("ocke_rel4") } // SP expr NL
  @Test def test_internal_space_preserved() { runner.runOneTest("internal_space_preserved") } // space preservation w/in expr

  val tdml2 = testDir + "expression_fail.tdml"
  lazy val runner2 = new DFDLTestSuite(Misc.getRequiredResource(tdml2), validateTDMLFile = false)

  // DFDL-313
  // Verified that we do get an error regarding an improperly formatted
  // DFDL expression.  This test needs its own file since it fails at the
  // schema loading (SAXParse) level and would cause other tests within
  // the same file to fail.
  //
  @Test def test_no_closing_brace() { runner2.runOneTest("no_closing_brace") } // no closing } for expression
}

