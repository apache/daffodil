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

package org.apache.daffodil.section13.nillable

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestNillable2 {
  val testDir = "/org/apache/daffodil/section13/nillable/"

  val runner = Runner(testDir, "nillable2.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }

}

class TestNillable2 {

  import TestNillable2._

  @Test def test_foo1(): Unit = {
    runner.runOneTest("foo1")
  }

  @Test def test_foo2(): Unit = {
    runner.runOneTest("foo2")
  }

  @Test def test_foo3(): Unit = {
    runner.runOneTest("foo3")
  }

  @Test def test_foo4(): Unit = {
    runner.runOneTest("foo4")
  }

}
