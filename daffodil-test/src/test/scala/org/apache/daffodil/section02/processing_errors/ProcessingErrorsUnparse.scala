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

package org.apache.daffodil.section02.processing_errors

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestProcessingErrorsUnparse {
  val testDir = "/org/apache/daffodil/section02/processing_errors/"

  val runner02 = Runner(testDir, "ProcessingErrorsUnparse.tdml", validateTDMLFile = false, validateDFDLSchemas = false)
  val runner02Validate = Runner(testDir, "ProcessingErrorsUnparse.tdml", validateTDMLFile = true, validateDFDLSchemas = true)

  @AfterClass def shutDown {
    runner02.reset
    runner02Validate.reset
  }

}

class TestProcessingErrorsUnparse {

  import TestProcessingErrorsUnparse._

  @Test def test_roundTripErrorHalfwayThrough() { runner02Validate.runOneTest("roundTripErrorHalfwayThrough") }

  @Test def test_upaInvalidSchemaUnparse() { runner02Validate.runOneTest("upaInvalidSchemaUnparse") }
  @Test def test_upaInvalidSchemaUnparse2() { runner02Validate.runOneTest("upaInvalidSchemaUnparse2") }
  @Test def test_missingNamespacePrefixUnparse() { runner02.runOneTest("missingNamespacePrefixUnparse") }
  @Test def test_incorrectRootUnparse() { runner02.runOneTest("incorrectRootUnparse") }
  @Test def test_incorrectNamespaceUnparse() { runner02.runOneTest("incorrectNamespaceUnparse") }

}
