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

import org.junit._
import org.apache.daffodil.tdml.Runner

object TestUnparseArrayOptionalElemDebug {

  val testDir = "/org/apache/daffodil/section16/array_optional_elem/"

  val runner_fixed = Runner(testDir, "UnparseArrayFixedOptionalElem.tdml")
  val runner_imp = Runner(testDir, "UnparseArrayImplicitOptionalElem.tdml")
  val runner_parsed = Runner(testDir, "UnparseArrayParsedOptionalElem.tdml")
  val runner_expr = Runner(testDir, "UnparseArrayExpressionConstant.tdml")
  val runner_delim = Runner(testDir, "UnparseArrayDelimitedOptionalElem.tdml")

  @AfterClass def tearDown() {
    runner_fixed.reset
    runner_imp.reset
    runner_parsed.reset
    runner_expr.reset
    runner_delim.reset
  }

}

class TestUnparseArrayOptionalElemDebug {

}
