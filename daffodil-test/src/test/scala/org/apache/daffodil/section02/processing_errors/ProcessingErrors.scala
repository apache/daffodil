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

package org.apache.daffodil.section02.processing_errors

import org.junit.Test
import org.apache.daffodil.util._
import org.apache.daffodil.tdml.DFDLTestSuite
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestProcessingErrors {
  val testDir = "/org/apache/daffodil/section02/processing_errors/"
  val aa = testDir + "dfdl-schema-validation-diagnostics.tdml"
  var runner = new DFDLTestSuite(Misc.getRequiredResource(aa), validateTDMLFile = false, validateDFDLSchemas = false)

  val runner02 = Runner(testDir, "ProcessingErrors.tdml", validateTDMLFile = false, validateDFDLSchemas = false)
  val runner02Validate = Runner(testDir, "ProcessingErrors.tdml", validateTDMLFile = true, validateDFDLSchemas = true)

  @AfterClass def shutDown {
    runner = null
    runner02.reset
    runner02Validate.reset
  }

}

class TestProcessingErrors {

  import TestProcessingErrors._

  runner.setCheckAllTopLevel(true) // check every top level construct. Not just the one under specific test.
  @Test def test_twoDFDLSchemaValidationErrors() { runner.runOneTest("twoDFDLSchemaValidationErrors") }
  @Test def test_twoDFDLSchemaValidationErrors2() { runner.runOneTest("twoDFDLSchemaValidationErrors2") }
  @Test def test_fiveDFDLSchemaValidationErrors() { runner.runOneTest("fiveDFDLSchemaValidationErrors") }

  @Test def test_upaInvalidSchema() { runner02Validate.runOneTest("upaInvalidSchema") }
  @Test def test_upaInvalidSchema2() { runner02Validate.runOneTest("upaInvalidSchema2") }

  //  DFDL-756
  //  @Test def test_delimiterNotFound01() { runner02.runOneTest("delimiterNotFound01") }

}
