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

import org.apache.daffodil.tdml.Runner

import org.junit.AfterClass
import org.junit.Test

object TestLayers {

  val testDir = "/org/apache/daffodil/layers/"
  val runner = Runner(testDir, "TestLayers.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}

class TestLayers {

  import TestLayers._

  @Test def testAllTypes(): Unit = { runner.runOneTest("testAllTypes") }

  @Test def testOk1(): Unit = { runner.runOneTest("testOk1") }
  @Test def testOk2(): Unit = { runner.runOneTest("testOk2") }
  @Test def testOk3(): Unit = { runner.runOneTest("testOk3") }
  @Test def testOk4(): Unit = { runner.runOneTest("testOk4") }

  @Test def testBadTypeInLayerCode1(): Unit = { runner.runOneTest("testBadTypeInLayerCode1") }
  @Test def testBadTypeInLayerCode2(): Unit = { runner.runOneTest("testBadTypeInLayerCode2") }
  @Test def testBadNotInMETAINFServices(): Unit = {
    runner.runOneTest("testBadNotInMETAINFServices")
  }

  @Test def testBadMissingSetter(): Unit = { runner.runOneTest("testBadMissingSetter") }

  @Test def testBadMissingSetterArg(): Unit = { runner.runOneTest("testBadMissingSetterArg") }

  @Test def testBadMissingGetter(): Unit = { runner.runOneTest("testBadMissingGetter") }
  @Test def testBadMissingSetterVar(): Unit = { runner.runOneTest("testBadMissingSetterVar") }
  @Test def testBadMissingGetterVar(): Unit = { runner.runOneTest("testBadMissingGetterVar") }

  @Test def testBadMissingDefaultConstructor(): Unit = {
    runner.runOneTest("testBadMissingDefaultConstructor")
  }
  @Test def testBadNotALayer(): Unit = { runner.runOneTest("testBadNotALayer") }

}
