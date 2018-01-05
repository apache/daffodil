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

package org.apache.daffodil.section06.entities

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestEntitiesDebug {
  private val testDir = "/org/apache/daffodil/section06/entities/"
  private val testDir_02 = "/org/apache/daffodil/ibm-tests/"

  val runner = Runner(testDir, "charClassEntities.tdml")
  val runner_01 = Runner(testDir, "Entities.tdml")
  val runner_02 = Runner(testDir_02, "dpaext1.tdml")
  val runnerEntity = Runner(testDir, "entities_01.tdml")
  val runnerInvalid = Runner(testDir, "InvalidEntities.tdml")

  @AfterClass def shutDown {
    runner.reset
    runner_01.reset
    runner_02.reset
    runnerEntity.reset
    runnerInvalid.reset
  }
}

class TestEntitiesDebug {

  import TestEntitiesDebug._

  // DFDL-378
  @Test def test_dataDumpEncoding() { runner_01.runOneTest("dataDumpEncoding") }

  // JIRA DFDL-1400 - separator in different encoding than terms being separated.
  @Test def test_text_entities_6_03b() { runner_01.runOneTest("text_entities_6_03b") }

  //DFDL-258 - raw byte entities
  @Test def test_byte_entities_6_10() { runner_01.runOneTest("byte_entities_6_10") }

  // Needs dfdl:utf16Width='variable' implementation
  @Test def test_syntax_entities_6_03() { runner_02.runOneTest("syntax_entities_6_03") }

  // Regression - we used to just reject %ES; in terminators. Now we accept it, but it doesn't work
  // right. JIRA DFDL-1477
   @Test def test_entity_fail_03() { runnerEntity.runOneTest("entity_fail_03") }

}
