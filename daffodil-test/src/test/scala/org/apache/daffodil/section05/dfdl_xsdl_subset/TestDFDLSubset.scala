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

package org.apache.daffodil.section05.dfdl_xsdl_subset

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Test

object TestDFDLSubset extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section05/dfdl_xsdl_subset/DFDLSubset.tdml"
}

object TestDFDLSubsetNoValidate extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section05/dfdl_xsdl_subset/DFDLSubset.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, validateDFDLSchemas = false)
}

class TestDFDLSubset extends TdmlTests {
  val tdmlSuite = TestDFDLSubset

  @Test def groupRefGroupRef = test
  @Test def refInitiator3 = test
  @Test def badGroupRef = test
  @Test def badSeq = test

  @Test def groupRefDFDL = test
}

class TestDFDLSubsetNoValidate extends TdmlTests {
  val tdmlSuite = TestDFDLSubsetNoValidate

  @Test def groupRef = test
  @Test def groupRefChoice = test
}
