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

package org.apache.daffodil.section08.property_scoping

import org.junit.Test
import org.apache.daffodil.util._
import org.apache.daffodil.tdml.DFDLTestSuite
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestPropertyScopingDebug {
  val testDir = "/org/apache/daffodil/section08/property_scoping/"
  val runner = Runner(testDir, "PropertyScoping.tdml")
  val runner_01 = Runner(testDir, "PropertyScoping_01.tdml")

  @AfterClass def shutDown {
    runner.reset
    runner_01.reset
  }
}

class TestPropertyScopingDebug {

  import TestPropertyScopingDebug._

  //DFDL-1036 (was fixed) now DFDL-1159
  @Test def test_localAnnotation_05() { runner.runOneTest("localAnnotation_05") }

  val tdml = testDir + "PropertyScoping_01.tdml"
  lazy val runner_01 = new DFDLTestSuite(Misc.getRequiredResource(tdml))

  // See DFDL-1337
  @Test def test_unparse_property_scoping_02() { runner_01.runOneTest("unparse_property_scoping_02") }
  // See DFDL-1342
  @Test def test_unparse_property_scoping_04() { runner_01.runOneTest("unparse_property_scoping_04") }
  @Test def test_unparse_property_scoping_05() { runner_01.runOneTest("unparse_property_scoping_05") }
}
