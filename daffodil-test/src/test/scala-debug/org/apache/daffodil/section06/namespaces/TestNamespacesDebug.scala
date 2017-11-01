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

package org.apache.daffodil.section06.namespaces

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestNamespacesDebug {
  val testDir = "/org/apache/daffodil/section06/namespaces/"

  val runner = Runner(testDir, "namespaces.tdml", validateTDMLFile = true, validateDFDLSchemas = false)
  val runner2 = Runner(testDir, "multiFile.tdml", validateTDMLFile = false, validateDFDLSchemas = false)
  val runner3 = Runner(testDir, "includeImport.tdml")
  val runnerWithSchemaValidation = Runner(testDir, "multiFile.tdml", validateTDMLFile = true, validateDFDLSchemas = true)

  @AfterClass def shutDown {
    runner.reset
    runner2.reset
    runner3.reset
    runnerWithSchemaValidation.reset
  }
}

class TestNamespacesDebug {

  import TestNamespacesDebug._

  @Test def test_multi_encoding_04() { runner.runOneTest("multi_encoding_04") }
  @Test def test_multi_encoding_05() { runner.runOneTest("multi_encoding_05") }
  @Test def test_indexOutOfBounds_01() { runner.runOneTest("indexOutOfBounds_01") }

  // DFDL-1204 - this test no longer works. New loader won't accept character U+00B7 as a character
  // in a prefix name.
  @Test def test_namespaceSpecialChars() { runner.runOneTest("namespaceSpecialChars") }

  // DFDL-1663
  @Test def test_namespaceLimitParse() { runner.runOneTest("namespaceLimitParse") }
  @Test def test_namespaceLimitUnparse() { runner.runOneTest("namespaceLimitUnparse") }
}
