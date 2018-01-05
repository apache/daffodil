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

import org.apache.daffodil.util._
import org.apache.daffodil.tdml.DFDLTestSuite
import org.junit.AfterClass

object TestArrayOptionalElemNew {

  val testDir = "/org/apache/daffodil/section16/array_optional_elem/"
  //  val aa_fixed = testDir + "UnparseArrayFixedOptionalElem.tdml"
  //  var runner_fixed = new DFDLTestSuite(Misc.getRequiredResource(aa_fixed))
  //
  //  val aa_imp = testDir + "UnparseArrayImplicitOptionalElem.tdml"
  //  var runner_imp = new DFDLTestSuite(Misc.getRequiredResource(aa_imp))
  //
  //  val aa_parsed = testDir + "UnparseArrayParsedOptionalElem.tdml"
  //  var runner_parsed = new DFDLTestSuite(Misc.getRequiredResource(aa_parsed))
  //
  //  val aa_expr = testDir + "UnparseArrayExpressionConstant.tdml"
  //  var runner_expr = new DFDLTestSuite(Misc.getRequiredResource(aa_expr))

  val aa_delim = testDir + "UnparseArrayDelimitedOptionalElem.tdml"
  var runner_delim = new DFDLTestSuite(Misc.getRequiredResource(aa_delim))

  /**
   * Avoid memory leak of adding more and more test suites to static objects as we run more and more test suites.
   */
  @AfterClass def tearDown() {
    //    runner_fixed = null
    //    runner_imp = null
    //    runner_parsed = null
    //    runner_expr = null
    runner_delim = null
  }

}

class TestArrayOptionalElemNew {

}
