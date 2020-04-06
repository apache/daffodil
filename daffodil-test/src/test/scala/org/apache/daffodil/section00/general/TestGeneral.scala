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

/* This section00 is for testing general features of DFDL that are
 * not related to any specific requirement
 */

import org.junit.Assert._
import org.junit.Test
import org.apache.daffodil.Implicits.intercept
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestGeneral {
  lazy val testDir = "/org/apache/daffodil/section00/general/"
  lazy val runner = Runner(testDir, "general.tdml")

  lazy val runner1 = Runner(testDir, "largeInput.tdml")

  lazy val testDir2 = "/test space/"
  lazy val runnerA_B = Runner(testDir2, "A BTinyData.tdml.dat")

  lazy val testDir3 = "/test space/test 1/"
  lazy val runner_ns = Runner(testDir3, "namespaces.tdml")

  lazy val tunables_runner = Runner(testDir, "tunables.tdml")

  @AfterClass def shutDown() {
    runner.reset
    runner1.reset
    runnerA_B.reset
    runner_ns.reset
    tunables_runner.reset
  }
}

class TestGeneral {

  import TestGeneral._

  @Test def test_check_no_namespace_message() { runner.runOneTest("check_no_namespace_message") }

  @Test def test_capitalization() { runner.runOneTest("capitalization") }

  @Test def test_litNil1() { runner.runOneTest("litNil1") }
  @Test def test_litNil1FullPath() { runner.runOneTest("litNil1FullPath") }
  @Test def test_referentialIntegrity() { runner.runOneTest("referentialIntegrity") }

  // Test causes exception as the file is not found
  // @Test def test_fileDNE() { runner.runOneTest("fileDNE") }

  @Test def test_largeInput_01() { runner1.runOneTest("largeInput_01") }

  @Test def test_dir_and_file_with_spaces() {
    try {
      val e = intercept[Exception] {
        runnerA_B.runOneTest("AB006")
      }
      val m = e.getMessage()
      assertTrue(m.toLowerCase.contains("required resource"))
      assertTrue(m.contains("/test%20space/A%20BTinyData.tdml.dat"))
      assertTrue(m.toLowerCase.contains("not found"))
    } catch {
      case fnf: java.io.FileNotFoundException =>
        println("FUBAR")
    }
  }

  @Test def test_no_namespace_02() {
    val e = intercept[Exception] {
      runner_ns.runOneTest("no_namespace_02")
    }
    val m = e.getMessage()
    assertTrue(m.toLowerCase.contains("required resource"))
    assertTrue(m.contains("/test%20space/test%201/namespaces.tdml"))
    assertTrue(m.toLowerCase.contains("not found"))
  }

  // DFDL-1143
  @Test def test_unqualifiedPathStepPolicy_defaultNamespace_test_01() { tunables_runner.runOneTest("unqualifiedPathStepPolicy_defaultNamespace_test_01") }
  @Test def test_unqualifiedPathStepPolicy_noNamespace_test_02() { tunables_runner.runOneTest("unqualifiedPathStepPolicy_noNamespace_test_02") }
  @Test def test_unqualifiedPathStepPolicy_defaultNamespace_test_02() { tunables_runner.runOneTest("unqualifiedPathStepPolicy_defaultNamespace_test_02") }
  
  @Test def test_maxOccursBoundsExceeded() { tunables_runner.runOneTest("maxOccursBoundsExceeded") }
  @Test def test_textBidiYes() { tunables_runner.runOneTest("textBidiYes") }
  @Test def test_requireTextBidiTrue() { tunables_runner.runOneTest("requireTextBidiTrue") }
  @Test def test_requireTextBidiFalse() { tunables_runner.runOneTest("requireTextBidiFalse") }
  @Test def test_floatingYes() { tunables_runner.runOneTest("floatingYes") }
  @Test def test_requireFloatingTrue() { tunables_runner.runOneTest("requireFloatingTrue") }
  @Test def test_requireFloatingFalse() { tunables_runner.runOneTest("requireFloatingFalse") }
  @Test def test_encodingErrorPolicyError() { tunables_runner.runOneTest("encodingErrorPolicyError") }
  @Test def test_requireEncodingErrorPolicyTrue() { tunables_runner.runOneTest("requireEncodingErrorPolicyTrue") }
  @Test def test_requireEncodingErrorPolicyFalse() { tunables_runner.runOneTest("requireEncodingErrorPolicyFalse") }

}
