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

package org.apache.daffodil.runtime1.layers


import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestLayers {
  val testDir = "/org/apache/daffodil/layers/"
  val runner = Runner(testDir, "layers.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}

class TestLayers {

  import TestLayers._

  @Test def test_gzipLayer1(): Unit = { runner.runOneTest("gzipLayer1") }
  @Test def test_foldedIMFBase64Layers1(): Unit = { runner.runOneTest("foldedIMFBase64Layers1") }
  @Test def test_foldedIMF1(): Unit = { runner.runOneTest("foldedIMF1") }
  @Test def test_layersErr1(): Unit = { runner.runOneTest("layersErr1") }
  @Test def test_base64GzipLayers1(): Unit = { runner.runOneTest("base64GzipLayers1") }

}
