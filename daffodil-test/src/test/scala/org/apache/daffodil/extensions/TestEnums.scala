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

package org.apache.daffodil.extensions

import org.apache.daffodil.tdml.Runner

import org.junit.AfterClass
import org.junit.Test

object TestEnums {
  val testDir = "/org/apache/daffodil/extensions/enum/"

  val runner = Runner(testDir, "enums.tdml")
  val runner2 = Runner(testDir, "enumInvalid.tdml", validateTDMLFile = false)

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }

}

class TestEnums {
  import TestEnums._
  @Test def test_enumValid1(): Unit = { runner.runOneTest("enumValid1") }
  @Test def test_enumInvalid1(): Unit = { runner.runOneTest("enumInvalid1") }
  @Test def test_enumMiss1(): Unit = { runner.runOneTest("enumMiss1") }
  @Test def test_enumNoRepValues(): Unit = { runner.runOneTest("enumNoRepValues") }

  @Test def test_repTypeAlignment(): Unit = { runner.runOneTest("repTypeAlignment") }

  @Test def test_emptyRepValues(): Unit = { runner2.runOneTest("emptyRepValues") }
  @Test def test_noRepValuesDifferentTypes(): Unit = {
    runner2.runOneTest("noRepValuesDifferentTypes")
  }

}
