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

package org.apache.daffodil.section16.array_optional_elem

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestArrayOptionalElemDebug {
  private val testDir = "/org/apache/daffodil/section16/array_optional_elem/"
  private val testDir01 = "/org/apache/daffodil/section05/facets/"
  private val testDir1 = "/org/apache/daffodil/ibm-tests/"

  val runner = Runner(testDir, "ArrayOptionalElem.tdml")
  val runner01 = Runner(testDir01, "Facets.tdml", validateTDMLFile = false)
  val runner1 = Runner(testDir1, "dpaext2.tdml")
  val rBack = Runner(testDir, "backtracking.tdml")

  @AfterClass def shutDown {
    runner.reset
    runner01.reset
    runner1.reset
    rBack.reset
  }

}

class TestArrayOptionalElemDebug {

  import TestArrayOptionalElemDebug._

  @Test def test_arrayExpressions03() { runner.runOneTest("arrayExpressions03") }

}
