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
  lazy val midBitsToEndRunner = Runner(testDir, "midBitsToEnd.tdml")

  @AfterClass def shutDown() {
    runner.reset
  }
}

class TestLayers {

  import TestLayers._

  @Test def test_layers1() { runner.runOneTest("layers1") }
  @Test def test_layers2() { runner.runOneTest("layers2") }
  @Test def test_layers3() { runner.runOneTest("layers3") }
  @Test def test_layersErr1() { runner.runOneTest("layersErr1") }
  @Test def test_layers4() { runner.runOneTest("layers4") }

  @Test def test_midBitsToEndMSBF1() { midBitsToEndRunner.runOneTest("midBitsToEndMSBF1") }
  @Test def test_midBitsToEndLSBF1() { midBitsToEndRunner.runOneTest("midBitsToEndLSBF1") }

  @Test def test_midBitsToEnd_unsufficentBits_1() { midBitsToEndRunner.runOneTest("midBitsToEnd_unsufficentBits_1") }

}
