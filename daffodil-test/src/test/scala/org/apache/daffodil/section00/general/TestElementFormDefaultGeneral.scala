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

package org.apache.daffodil.section00.general

import org.junit.Test
import org.junit._
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestElementFormDefaultGeneral {
  
  val testDir = "/org/apache/daffodil/section00/general/"
  val runner = Runner(testDir, "testElementFormDefault.tdml")

  @AfterClass def shutDown(): Unit = { 
    runner.reset
  }

}

class TestElementFormDefaultGeneral {
  import TestElementFormDefaultGeneral._
 
  @Test def test_delimOptPresentQualified01(): Unit = { runner.runOneTest("delimOptPresentQualified01") }
  @Test def test_delimOptPresentQualified02(): Unit = { runner.runOneTest("delimOptPresentQualified02") }
  @Test def test_delimOptPresentQualified03(): Unit = { runner.runOneTest("delimOptPresentQualified03") }
  @Test def test_delimOptPresentQualified04(): Unit = { runner.runOneTest("delimOptPresentQualified04") }
  @Test def test_delimOptPresentQualified05(): Unit = { runner.runOneTest("delimOptPresentQualified05") }

  @Test def test_delimOptPresentUnqualified01(): Unit = { runner.runOneTest("delimOptPresentUnqualified01") }
  @Test def test_delimOptPresentUnqualified02(): Unit = { runner.runOneTest("delimOptPresentUnqualified02") }
  @Test def test_delimOptPresentUnqualified03(): Unit = { runner.runOneTest("delimOptPresentUnqualified03") }
  @Test def test_delimOptPresentUnqualified04(): Unit = { runner.runOneTest("delimOptPresentUnqualified04") }
  @Test def test_delimOptPresentMissing(): Unit = { runner.runOneTest("delimOptPresentMissing") }

  @Test def test_delimOptPresentGlobalQualified01(): Unit = { runner.runOneTest("delimOptPresentGlobalQualified01") }
  @Test def test_delimOptPresentGlobalQualified02(): Unit = { runner.runOneTest("delimOptPresentGlobalQualified02") }

}
