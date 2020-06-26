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

package org.apache.daffodil.section00.general

/* This section00 is for testing general features of DFDL that are
 * not related to any specific requirement
 */

import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestGeneralNew {

  val testDir = "/org/apache/daffodil/section00/general/"
  val runner = Runner(testDir, "general.tdml")

  @AfterClass def shutDown: Unit = {
    runner.reset
  }
}

class TestGeneralNew {

  // TODO: DFDL-451 - After speaking with Mike B. about this we are putting this functionality on the backburner
  // until we can figure out the appropriate behavior here.
  //
  //@Test def test_check_escape_separator_distinct_fail() { runner.runOneTest("check_escape_separator_distinct_fail") }
}
