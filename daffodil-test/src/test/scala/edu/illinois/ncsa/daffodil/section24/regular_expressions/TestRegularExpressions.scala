package edu.illinois.ncsa.daffodil.section24.regular_expressions

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

class TestRegularExpressions {
  val testDir = "/edu/illinois/ncsa/daffodil/section24/regular_expressions/"
  val tdml = testDir + "RegularExpressions.tdml"
  lazy val runner = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  @Test def test_entity_in_regex_fail() { runner.runOneTest("entity_in_regex_fail") }
  @Test def test_entity_in_regex_fail_2() { runner.runOneTest("entity_in_regex_fail_2") }
  @Test def test_entity_in_regex_fail_3() { runner.runOneTest("entity_in_regex_fail_3") }
  @Test def test_entity_in_regex_fail_4() { runner.runOneTest("entity_in_regex_fail_4") }

  @Test def test_testRegEx_01() { runner.runOneTest("testRegEx_01") }
  @Test def test_testRegEx_02() { runner.runOneTest("testRegEx_02") }
  @Test def test_testRegEx_03() { runner.runOneTest("testRegEx_03") }

  // // Unsupported Java 7 features (should return Schema Definition Errors)
  // @Test def test_testRegEx_04() { runner.runOneTest("testRegEx_04") }
  // @Test def test_testRegEx_05() { runner.runOneTest("testRegEx_05") }
  // @Test def test_testRegEx_06() { runner.runOneTest("testRegEx_06") }
  // @Test def test_testRegEx_07() { runner.runOneTest("testRegEx_07") }
}
