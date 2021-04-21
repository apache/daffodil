/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.section02.processing_errors

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestProcessingErrors {
  val testDir = "/org/apache/daffodil/section02/processing_errors/"

  val runner = Runner(testDir, "dfdl-schema-validation-diagnostics.tdml", validateTDMLFile = false)

  val runner02 = Runner(testDir, "ProcessingErrors.tdml", validateTDMLFile = false, validateDFDLSchemas = false)
  val runner02Validate = Runner(testDir, "ProcessingErrors.tdml", validateTDMLFile = true, validateDFDLSchemas = true)

  @AfterClass def shutDown(): Unit = {
    runner.reset
    runner02.reset
    runner02Validate.reset
  }

}

class TestProcessingErrors {

  import TestProcessingErrors._

  @Test def test_twoDFDLSchemaValidationErrors(): Unit = { runner.runOneTest("twoDFDLSchemaValidationErrors") }
  @Test def test_twoDFDLSchemaValidationErrors2(): Unit = { runner.runOneTest("twoDFDLSchemaValidationErrors2") }
  @Test def test_fiveDFDLSchemaValidationErrors(): Unit = { runner.runOneTest("fiveDFDLSchemaValidationErrors") }

  @Test def test_upaInvalidSchema(): Unit = { runner02Validate.runOneTest("upaInvalidSchema") }
  @Test def test_upaInvalidSchema2(): Unit = { runner02Validate.runOneTest("upaInvalidSchema2") }

  //  DFDL-756
  //  @Test def test_delimiterNotFound01() { runner02.runOneTest("delimiterNotFound01") }

}
