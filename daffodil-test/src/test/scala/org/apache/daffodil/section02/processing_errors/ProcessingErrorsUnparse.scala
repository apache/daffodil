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

import org.junit.Test

object TestProcessingErrorsUnparseNoValidate extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section02/processing_errors/ProcessingErrorsUnparse.tdml"

  override def createRunner() =
    Runner(tdmlDir, tdmlFile, validateTDMLFile = false, validateDFDLSchemas = false)
}

object TestProcessingErrorsUnparseValidate extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section02/processing_errors/ProcessingErrorsUnparse.tdml"

  override def createRunner() = Runner(
    tdmlDir,
    tdmlFile,
    validateTDMLFile = true,
    validateDFDLSchemas = true,
    compileAllTopLevel = true
  )
}

class TestProcessingErrorsUnparseNoValidate extends TdmlTests {
  val tdmlSuite = TestProcessingErrorsUnparseNoValidate

  @Test def missingNamespacePrefixUnparse = test
  @Test def incorrectNamespaceUnparse = test
}

class TestProcessingErrorsUnparseValidate extends TdmlTests {
  val tdmlSuite = TestProcessingErrorsUnparseValidate

  @Test def roundTripErrorHalfwayThrough = test
  @Test def upaInvalidSchemaUnparse = test
  @Test def upaInvalidSchemaUnparse2 = test
}
