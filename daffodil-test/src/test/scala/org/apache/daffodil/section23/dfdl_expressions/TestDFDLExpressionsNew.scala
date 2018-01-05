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

package org.apache.daffodil.section23.dfdl_expressions

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestDFDLExpressionsNew {

  val testDir2 = "/org/apache/daffodil/section23/dfdl_functions/"
  val runner2 = Runner(testDir2, "Functions.tdml")
  val testDir5 = "/org/apache/daffodil/section23/dfdl_expressions/"
  val runner5 = Runner(testDir5, "expressions.tdml")
  @AfterClass def shutdown = {
    runner2.reset
    runner5.reset

  }
}

class TestDFDLExpressionsNew {
  import TestDFDLExpressionsNew._

  //DFDL-1076
  @Test def test_nilled_01() { runner2.runOneTest("nilled_01") }

  // DFDL-1617 - should detect errors due to query-style expressions
  @Test def test_query_style_01 { runner5.runOneTest("query_style_01") }
  @Test def test_query_style_02 { runner5.runOneTest("query_style_02") }
}
