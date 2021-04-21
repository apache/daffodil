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

package org.apache.daffodil.usertests

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object RCTest5 {
  val testDir = "/org/apache/daffodil/usertests/"
  val runner = Runner(testDir, "test-5.tdml.xml")
  val runner6 = Runner(testDir, "test-6.tdml.xml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
    runner6.reset
  }

}

class RCTest5 {

  import RCTest5._

  @Test def test5p(): Unit = { runner.runOneTest("parse-test-5") }

  // DAFFODIL-2217
  // @Test def test5u() { runner.runOneTest("unparse-test-5") }

  // DAFFODIL-2219
  // @Test def test6() { runner6.runOneTest("parse-test-6") }

}
