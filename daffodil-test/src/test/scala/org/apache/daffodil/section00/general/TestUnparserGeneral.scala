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
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner

object TestUnparserGeneral {
  val testDir = "/org/apache/daffodil/section00/general/"
  val runner = Runner(testDir, "testUnparserGeneral.tdml")

  @AfterClass def shutDown {
    runner.reset
  }
}
class TestUnparserGeneral {

  import TestUnparserGeneral._

  //DFDL-1420
  //@Test def test_apostrophe_01() { runner.runOneTest("apostrophe_01") }

  //DFDL-1395
  //@Test def test_puaInfosetChars_03() { runner.runOneTest("puaInfosetChars_03") }
  //@Test def test_puaInfosetChars_04() { runner.runOneTest("puaInfosetChars_04") }

  @Test def test_puaInfosetChars_01() { runner.runOneTest("puaInfosetChars_01") }
  @Test def test_puaInfosetChars_02() { runner.runOneTest("puaInfosetChars_02") }

  @Test def test_unparseFixedLengthString01() { runner.runOneTest("unparseFixedLengthString01") }
  @Test def test_unparseFixedLengthString02() { runner.runOneTest("unparseFixedLengthString02") }
  @Test def test_unparseFixedLengthString03() { runner.runOneTest("unparseFixedLengthString03") }

  @Test def test_parseFixedLengthString01() { runner.runOneTest("parseFixedLengthString01") }
  @Test def test_parseFixedLengthStringLength0() { runner.runOneTest("parseFixedLengthStringLength0") }

  @Test def test_negativeUnparseTest01() { runner.runOneTest("negativeUnparseTest01") }
  @Test def test_negativeUnparseTest02() { runner.runOneTest("negativeUnparseTest02") }
  @Test def test_negativeUnparseTest03() { runner.runOneTest("negativeUnparseTest03") }
  @Test def test_negativeUnparseTest04() { runner.runOneTest("negativeUnparseTest04") }
  @Test def test_negativeUnparseTest05() { runner.runOneTest("negativeUnparseTest05") }

  @Test def test_unparseDelimitedString01() { runner.runOneTest("unparseDelimitedString01") }
  @Test def test_unparseDelimitedString02() { runner.runOneTest("unparseDelimitedString02") }
  @Test def test_unparseDelimitedString03() { runner.runOneTest("unparseDelimitedString03") }
  @Test def test_unparseDelimitedString04() { runner.runOneTest("unparseDelimitedString04") }
  @Test def test_unparseDelimitedString05() { runner.runOneTest("unparseDelimitedString05") }
  @Test def test_unparseDelimitedString06() { runner.runOneTest("unparseDelimitedString06") }
  @Test def test_unparseDelimitedString07() { runner.runOneTest("unparseDelimitedString07") }

  @Test def test_parseDelimitedString01() { runner.runOneTest("parseDelimitedString01") }

  // DFDL-1650
  @Test def test_alignmentPaddingOVC1() { runner.runOneTest("alignmentPaddingOVC1") }
  @Test def test_alignmentPaddingOVC2() { runner.runOneTest("alignmentPaddingOVC2") }
  @Test def test_alignmentPaddingOVC3() { runner.runOneTest("alignmentPaddingOVC3") }

}
