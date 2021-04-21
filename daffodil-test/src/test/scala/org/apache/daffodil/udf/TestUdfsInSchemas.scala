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

import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass
import org.junit.Test

object TestUdfsInSchemas {
  val testDir = "/org/apache/daffodil/udf/"

  val runner = Runner(testDir, "udfs.tdml", validateTDMLFile = true)

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }

}

class TestUdfsInSchemas {
  import TestUdfsInSchemas._

  @Test def test_udf_defaultNamespace(): Unit = { runner.runOneTest("test_udf_defaultNamespace") }
  @Test def test_udf_fnNotFound(): Unit = { runner.runOneTest("test_udf_fnNotFound") }
  @Test def test_udf_numArgsIncorrect(): Unit = { runner.runOneTest("test_udf_numArgsIncorrect") }
  @Test def test_udf_argsTypesIncorrect(): Unit = { runner.runOneTest("test_udf_argsTypesIncorrect") }
  @Test def test_udf_noArgs(): Unit = { runner.runOneTest("test_udf_noArgs") }
  @Test def test_boxedIntParamRetType(): Unit = { runner.runOneTest("test_boxedIntParamRetType") }
  @Test def test_primitiveIntParamRetType(): Unit = { runner.runOneTest("test_primitiveIntParamRetType") }
  @Test def test_boxedByteParamRetType(): Unit = { runner.runOneTest("test_boxedByteParamRetType") }
  @Test def test_primitiveByteParamRetType(): Unit = { runner.runOneTest("test_primitiveByteParamRetType") }
  @Test def test_primitiveByteArrayParamRetType(): Unit = { runner.runOneTest("test_primitiveByteArrayParamRetType") }
  @Test def test_boxedShortParamRetType(): Unit = { runner.runOneTest("test_boxedShortParamRetType") }
  @Test def test_primitiveShortParamRetType(): Unit = { runner.runOneTest("test_primitiveShortParamRetType") }
  @Test def test_boxedLongParamRetType(): Unit = { runner.runOneTest("test_boxedLongParamRetType") }
  @Test def test_primitiveLongParamRetType(): Unit = { runner.runOneTest("test_primitiveLongParamRetType") }
  @Test def test_boxedDoubleParamRetType(): Unit = { runner.runOneTest("test_boxedDoubleParamRetType") }
  @Test def test_primitiveDoubleParamRetType(): Unit = { runner.runOneTest("test_primitiveDoubleParamRetType") }
  @Test def test_boxedFloatParamRetType(): Unit = { runner.runOneTest("test_boxedFloatParamRetType") }
  @Test def test_primitiveFloatParamRetType(): Unit = { runner.runOneTest("test_primitiveFloatParamRetType") }
  @Test def test_boxedBooleanParamRetType(): Unit = { runner.runOneTest("test_boxedBooleanParamRetType") }
  @Test def test_primitiveBooleanParamRetType(): Unit = { runner.runOneTest("test_primitiveBooleanParamRetType") }
  @Test def test_javaBigIntegerParamRetType(): Unit = { runner.runOneTest("test_javaBigIntegerParamRetType") }
  @Test def test_javaBigDecimalParamRetType(): Unit = { runner.runOneTest("test_javaBigDecimalParamRetType") }
  @Test def test_stringParamRetType(): Unit = { runner.runOneTest("test_stringParamRetType") }

}
