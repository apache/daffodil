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

import org.apache.daffodil.core.util.TestUtils.intercept
import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Assert._
import org.junit.Ignore
import org.junit.Test

object TestGeneral extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section00/general/general.tdml"
}

class TestGeneral extends TdmlTests {
  val tdmlSuite = TestGeneral

  @Test def check_no_namespace_message = test
  @Test def capitalization = test

  @Test def litNil1 = test
  @Test def litNil1FullPath = test
  @Test def referentialIntegrity = test

  @Test def nameAndRefError_01 = test

  // Test causes exception as the file is not found
  @Ignore @Test def fileDNE = test
}

object TestLargeInput extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section00/general/largeInput.tdml"
}

class TestLargeInput extends TdmlTests {
  val tdmlSuite = TestLargeInput

  @Test def largeInput_01 = test
}

object TestSpace1 extends TdmlSuite {
  val tdmlResource = "/test space/A BTinyData.tdml.dat"
}

class TestSpace1 extends TdmlTests {
  val tdmlSuite = TestSpace1

  @Test def AB006 = {
    try {
      val e = intercept[Exception] { test }
      val m = e.getMessage()
      assertTrue(m.toLowerCase.contains("required resource"))
      assertTrue(m.contains("/test%20space/A%20BTinyData.tdml.dat"))
      assertTrue(m.toLowerCase.contains("not found"))
    } catch {
      case fnf: java.io.FileNotFoundException =>
        println("FUBAR")
    }
  }
}

object TestSpace2 extends TdmlSuite {
  val tdmlResource = "/test space/test 1/namespaces.tdml"
}

class TestSpace2 extends TdmlTests {
  val tdmlSuite = TestSpace2

  @Test def no_namespace_02(): Unit = {
    val e = intercept[Exception] { test }
    val m = e.getMessage()
    assertTrue(m.toLowerCase.contains("required resource"))
    assertTrue(m.contains("/test%20space/test%201/namespaces.tdml"))
    assertTrue(m.toLowerCase.contains("not found"))
  }
}

object TestTunables extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section00/general/tunables.tdml"
}

class TestTunables extends TdmlTests {
  val tdmlSuite = TestTunables

  @Test def unqualifiedPathStepPolicy_test_01_defaultNamespace = test
  @Test def unqualifiedPathStepPolicy_test_01_preferDefaultNamespace = test
  @Test def unqualifiedPathStepPolicy_test_01_noNamespace = test
  @Test def unqualifiedPathStepPolicy_test_02_defaultNamespace = test
  @Test def unqualifiedPathStepPolicy_test_02_preferDefaultNamespace = test
  @Test def unqualifiedPathStepPolicy_test_02_noNamespace = test
  @Test def unqualifiedPathStepPolicy_test_03_defaultNamespace = test
  @Test def unqualifiedPathStepPolicy_test_03_preferDefaultNamespace = test
  @Test def unqualifiedPathStepPolicy_test_03_noNamespace = test
  @Test def unqualifiedPathStepPolicy_test_04_defaultNamespace = test
  @Test def unqualifiedPathStepPolicy_test_04_preferDefaultNamespace = test
  @Test def unqualifiedPathStepPolicy_test_04_noNamespace = test

  @Test def maxOccursBoundsExceeded = test
  @Test def textBidiYes = test
  @Test def requireTextBidiTrue = test
  @Test def requireTextBidiFalse = test
  @Test def floatingYes = test
  @Test def requireFloatingTrue = test
  @Test def requireFloatingFalse = test
  @Test def encodingErrorPolicyError = test
  @Test def requireEncodingErrorPolicyTrue = test
  @Test def requireEncodingErrorPolicyFalse = test
  @Test def maxHexBinaryError = test
  @Test def maxHexBinaryUnparseError = test

  @Test def invalidRestrictionPolicyError_01 = test
  @Test def invalidRestrictionPolicyIgnore_01 = test
  @Test def invalidRestrictionPolicyValidate_01 = test
  @Test def invalidRestrictionPolicyValidate_02 = test
}
