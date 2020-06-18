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

package org.apache.daffodil.layers

/* This section00 is for testing general features of DFDL that are
 * not related to any specific requirement
 */

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestLayers {
  lazy val testDir = "/org/apache/daffodil/layers/"
  lazy val runner = Runner(testDir, "layers.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}

class TestLayers {

  import TestLayers._

  @Test def test_layers1(): Unit = { runner.runOneTest("layers1") }
  @Test def test_layers2(): Unit = { runner.runOneTest("layers2") }
  @Test def test_layers3(): Unit = { runner.runOneTest("layers3") }
  @Test def test_layers3_deprecated(): Unit = { runner.runOneTest("layers3_deprecated") }
  @Test def test_layersErr1(): Unit = { runner.runOneTest("layersErr1") }
  @Test def test_layers4(): Unit = { runner.runOneTest("layers4") }

}
