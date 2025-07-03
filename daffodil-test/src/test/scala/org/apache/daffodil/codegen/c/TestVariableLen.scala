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

package org.apache.daffodil.codegen.c

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.lib.iapi.TDMLImplementation
import org.apache.daffodil.tdml.Runner

import org.junit.Test

object TestVariableLen extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/codegen/c/variablelen.tdml"
}

class TestVariableLen extends TdmlTests {
  val tdmlSuite = TestVariableLen

  @Test def fixed = test
  @Test def implicitLen = test
  @Test def parsed = test
  @Test def expression = test
  @Test def expression_00 = test
  @Test def expression_01 = test
  @Test def expression_16 = test
  @Test def expression_17 = test
  @Test def stopValue = test
}

object TestVariableLenC extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/codegen/c/variablelen.tdml"
  override def createRunner() = Runner(tdmlDir, tdmlFile, TDMLImplementation.DaffodilC)
}

class TestVariableLenC extends TdmlTests {
  val tdmlSuite = TestVariableLenC

  @Test def fixed = test
  @Test def expression = test
  @Test def expression_00 = test
  @Test def expression_01 = test
  @Test def expression_16 = test
  @Test def expression_17_error = test
  @Test def stopValue = test
}
