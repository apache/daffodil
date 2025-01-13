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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Ignore
import org.junit.Test

object TestSchemaValidation extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section02/processing_errors/dfdl-schema-validation-diagnostics.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, validateTDMLFile = false)
}

object TestProcessingErrorsNoValidate extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section02/processing_errors/ProcessingErrors.tdml"

  override def createRunner() =
    Runner(tdmlDir, tdmlFile, validateTDMLFile = false, validateDFDLSchemas = false)
}

object TestProcessingErrorsValidate extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section02/processing_errors/ProcessingErrors.tdml"

  override def createRunner() =
    Runner(tdmlDir, tdmlFile, validateTDMLFile = true, validateDFDLSchemas = true)
}

class TestSchemaValidation extends TdmlTests {
  val tdmlSuite = TestSchemaValidation

  @Test def twoDFDLSchemaValidationErrors = test
  @Test def twoDFDLSchemaValidationErrors2 = test
  @Test def fiveDFDLSchemaValidationErrors = test
}

class TestProcessingErrorsNoValidate extends TdmlTests {
  val tdmlSuite = TestProcessingErrorsNoValidate

  // DFDL-756
  @Ignore @Test def delimiterNotFound01 = test
}

class TestProcessingErrorsValidate extends TdmlTests {
  val tdmlSuite = TestProcessingErrorsValidate

  @Test def upaInvalidSchema = test
  @Test def upaInvalidSchema2 = test
}
