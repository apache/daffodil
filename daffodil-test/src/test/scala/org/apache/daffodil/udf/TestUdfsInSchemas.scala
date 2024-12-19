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
package org.apache.daffodil.udf

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.tdml.Runner

import org.junit.Test

object TestUdfsInSchemas extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/udf/udfs.tdml"

  override def createRunner() = Runner(tdmlDir, tdmlFile, validateTDMLFile = false)
}

class TestUdfsInSchemas extends TdmlTests {
  val tdmlSuite = TestUdfsInSchemas

  @Test def test_udf_defaultNamespace = test
  @Test def test_udf_fnNotFound = test
  @Test def test_udf_numArgsIncorrect = test
  @Test def test_udf_argsTypesIncorrect = test
  @Test def test_udf_noArgs = test
  @Test def test_boxedIntParamRetType = test
  @Test def test_primitiveIntParamRetType = test
  @Test def test_boxedByteParamRetType = test
  @Test def test_primitiveByteParamRetType = test
  @Test def test_primitiveByteArrayParamRetType = test
  @Test def test_boxedShortParamRetType = test
  @Test def test_primitiveShortParamRetType = test
  @Test def test_boxedLongParamRetType = test
  @Test def test_primitiveLongParamRetType = test
  @Test def test_boxedDoubleParamRetType = test
  @Test def test_primitiveDoubleParamRetType = test
  @Test def test_boxedFloatParamRetType = test
  @Test def test_primitiveFloatParamRetType = test
  @Test def test_boxedBooleanParamRetType = test
  @Test def test_primitiveBooleanParamRetType = test
  @Test def test_javaBigIntegerParamRetType = test
  @Test def test_javaBigDecimalParamRetType = test
  @Test def test_stringParamRetType = test
}
