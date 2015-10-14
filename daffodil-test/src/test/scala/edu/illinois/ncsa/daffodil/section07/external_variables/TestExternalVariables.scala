/* Copyright (c) 2013 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.section07.external_variables

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestExternalVariables {

  val testDir = "/edu/illinois/ncsa/daffodil/section07/external_variables/"
  val runner = Runner(testDir, "external_variables.tdml")

  @AfterClass def tearDown() {
    runner.reset
  }

}

class TestExternalVariables {

  import TestExternalVariables._

  // It's important to note here that external variables
  // via the TDMLRunner are currently passed in during
  // compilation.
  @Test def test_override_define_vars_01() {
    runner.runOneTest("override_define_vars_01")
  }

  @Test def test_override_define_vars_02() {
    runner.runOneTest("override_define_vars_02")
  }

  @Test def test_override_define_vars_03() {
    runner.runOneTest("override_define_vars_03")
  }

  @Test def test_override_define_vars_04() {
    runner.runOneTest("override_define_vars_04")
  }

  @Test def test_override_define_vars_05() {
    runner.runOneTest("override_define_vars_05")
  }

  @Test def test_access_default_predefined_vars() {
    runner.runOneTest("access_default_predefined_vars")
  }

  @Test def test_set_a_predefined_var() {
    runner.runOneTest("set_predefined_var")
  }

}
