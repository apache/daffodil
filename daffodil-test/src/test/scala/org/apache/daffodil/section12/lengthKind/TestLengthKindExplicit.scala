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

package org.apache.daffodil.section12.lengthKind

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestLengthKindExplicit {
  val testDir = "/org/apache/daffodil/section12/lengthKind/"
  val runner = Runner(testDir, "ExplicitTests.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }

}

class TestLengthKindExplicit {

  import TestLengthKindExplicit._

  @Test def test_Lesson1_lengthKind_explicit(): Unit = { runner.runOneTest("Lesson1_lengthKind_explicit") }
  @Test def test_ExplicitLengthBytesNotFixed() = { runner.runOneTest("test_ExplicitLengthBytesNotFixed") }
  @Test def test_ExplicitLengthBitsFixed() = { runner.runOneTest("ExplicitLengthBitsFixed") }
  @Test def test_ExplicitLengthBytesFixed() = { runner.runOneTest("test_ExplicitLengthBytesFixed") }
  @Test def test_ExplicitLengthBitsNotFixed() = { runner.runOneTest("ExplicitLengthBitsNotFixed") }
  @Test def test_ExplicitLengthCharsNotFixed() = { runner.runOneTest("ExplicitLengthCharsNotFixed") }
  @Test def test_ExplicitLengthCharsFixed() = { runner.runOneTest("ExplicitLengthCharsFixed") }
  @Test def test_ExplicitLengthBytesFixed50() = { runner.runOneTest("ExplicitLengthBytesFixed50") }

  // This test should give runtime SDE, not parse error (DFDL-908)
  //@Test def test_lengthRuntimeSDENaN() = { runner.runOneTest("test_lengthRuntimeSDE") }
  @Test def test_lengthRuntimeSDENegative() = { runner.runOneTest("test_lengthRuntimeSDENegative") }
  @Test def test_ExplicitLengthBytesBroken() = { runner.runOneTest("test_ExplicitLengthBytesBroken") }

  @Test def test_ExplicitLengthBytesNotGiven() = { runner.runOneTest("test_ExplicitLengthBytesNotGiven") }

  @Test def test_ExplicitLengthBytesChoiceRef() = { runner.runOneTest("test_ExplicitLengthBytesChoiceRef") }
  @Test def test_ExplicitLengthChildLengthLessParent_Chars() = { runner.runOneTest("test_ExplicitLengthChildLengthLessParent_Chars") }
  @Test def test_ExplicitLengthChildLengthLessParent_Bytes() = { runner.runOneTest("test_ExplicitLengthChildLengthLessParent_Bytes") }
  @Test def test_ExplicitLengthChildLengthMoreParent_Chars() = { runner.runOneTest("test_ExplicitLengthChildLengthMoreParent_Chars") }

  @Test def test_explicitBytes_string_01() = { runner.runOneTest("explicitBytes_string_01") }
  @Test def test_explicitBytes_int_01() = { runner.runOneTest("explicitBytes_int_01") }
  @Test def test_explicitBytes_int_02() = { runner.runOneTest("explicitBytes_int_02") }

  // Added for issue related to DFDL-1674
  @Test def test_denseBit_lengthKind_explicit(): Unit = { runner.runOneTest("denseBit_lengthKind_explicit") }


}
