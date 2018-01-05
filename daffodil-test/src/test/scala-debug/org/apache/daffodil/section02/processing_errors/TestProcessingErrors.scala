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
import org.apache.daffodil.util._
import org.apache.daffodil.tdml.DFDLTestSuite
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner

object TestProcessingErrorsDebug {
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

class TestProcessingErrorsDebug {

  import TestProcessingErrorsDebug._

  @Test def test_delimiterNotFound01() { runner02.runOneTest("delimiterNotFound01") }

}
