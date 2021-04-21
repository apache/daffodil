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

package org.apache.daffodil.section05.simple_types

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner

object TestRuntimeCalendarLanguage {
  private val testDir = "/org/apache/daffodil/section05/simple_types/"

  val runner = Runner(testDir, "RuntimeCalendarLanguage.tdml")

  @AfterClass def shutdown(): Unit = {
    runner.reset
  }
}

class TestRuntimeCalendarLanguage {
  import TestRuntimeCalendarLanguage._

  @Test def test_runtimeCalendarLanguage1(): Unit = { runner.runOneTest("runtimeCalendarLanguage1") }
  @Test def test_invalidCalendarLanguage1(): Unit = { runner.runOneTest("invalidCalendarLanguage1") }
  @Test def test_unparseRuntimeCalendarLanguageOVC(): Unit = { runner.runOneTest("unparseRuntimeCalendarLanguageOVC") }
  @Test def test_unparseRuntimeCalendarLanguageOVCCacheCheck(): Unit = { runner.runOneTest("unparseRuntimeCalendarLanguageOVCCacheCheck") }
}
