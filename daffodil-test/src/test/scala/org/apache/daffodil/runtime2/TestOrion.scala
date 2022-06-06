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

package org.apache.daffodil.runtime2

import org.apache.daffodil.api.TDMLImplementation
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass
import org.junit.Test

object TestOrion {
  val testDir = "/org/apache/daffodil/runtime2/"
  val runner = Runner(testDir, "orion.tdml", TDMLImplementation.DaffodilC)

  @AfterClass def shutDown(): Unit = { runner.reset }
}

class TestOrion {
  import TestOrion._

  @Test def test_orion_aptina(): Unit = { runner.runOneTest("orion_aptina") }
  @Test def test_orion_camera(): Unit = { runner.runOneTest("orion_camera") }
  @Test def test_orion_command(): Unit = { runner.runOneTest("orion_command") }
  @Test def test_orion_limits(): Unit = { runner.runOneTest("orion_limits") }
  @Test def test_orion_video(): Unit = { runner.runOneTest("orion_video") }
}
