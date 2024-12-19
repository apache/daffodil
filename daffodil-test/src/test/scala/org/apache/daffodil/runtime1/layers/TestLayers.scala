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

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestLayers extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/layers/TestLayers.tdml"
}

class TestLayers extends TdmlTests {
  val tdmlSuite = TestLayers

  @Test def testAllTypes = test

  @Test def testOk1 = test
  @Test def testOk2 = test
  @Test def testOk3 = test
  @Test def testOk4 = test

  //
  // All the various ways a layer author can misconfigure a layer
  // These all result in SDE.
  //
  @Test def testBadTypeInLayerCode1 = test
  @Test def testBadTypeInLayerCode2 = test
  @Test def testBadNotInMETAINFServices = test
  @Test def testBadMissingSetter = test
  @Test def testBadMissingSetterArg = test
  @Test def testBadMissingGetter = test
  @Test def testBadMissingSetterVar = test
  @Test def testBadMissingGetterVar = test
  @Test def testBadMissingDefaultConstructor = test
  @Test def testBadNotALayer = test
}
