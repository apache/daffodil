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

object TestUnparserFileBuffering {
  val testDir = "/org/apache/daffodil/section00/general/"
  val runner = Runner(testDir, "testUnparserFileBuffering.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}
class TestUnparserFileBuffering {

  import TestUnparserFileBuffering._

  @Test def test_puaInfosetChars_01_ffb(): Unit = { runner.runOneTest("puaInfosetChars_01_ffb") }
  @Test def test_puaInfosetChars_02_ffb(): Unit = { runner.runOneTest("puaInfosetChars_02_ffb") }

  @Test def test_unparseFixedLengthString01_ffb(): Unit = { runner.runOneTest("unparseFixedLengthString01_ffb") }
  @Test def test_unparseFixedLengthString02_ffb(): Unit = { runner.runOneTest("unparseFixedLengthString02_ffb") }
  @Test def test_unparseFixedLengthString03_ffb(): Unit = { runner.runOneTest("unparseFixedLengthString03_ffb") }

  @Test def test_parseFixedLengthString01_ffb(): Unit = { runner.runOneTest("parseFixedLengthString01_ffb") }
  @Test def test_parseFixedLengthStringLength0_ffb(): Unit = { runner.runOneTest("parseFixedLengthStringLength0_ffb") }

  @Test def test_negativeUnparseTest01_ffb(): Unit = { runner.runOneTest("negativeUnparseTest01_ffb") }
  @Test def test_negativeUnparseTest02_ffb(): Unit = { runner.runOneTest("negativeUnparseTest02_ffb") }
  @Test def test_negativeUnparseTest03_ffb(): Unit = { runner.runOneTest("negativeUnparseTest03_ffb") }
  @Test def test_negativeUnparseTest04_ffb(): Unit = { runner.runOneTest("negativeUnparseTest04_ffb") }
  @Test def test_negativeUnparseTest05_ffb(): Unit = { runner.runOneTest("negativeUnparseTest05_ffb") }

  @Test def test_unparseDelimitedString01_ffb(): Unit = { runner.runOneTest("unparseDelimitedString01_ffb") }
  @Test def test_unparseDelimitedString02_ffb(): Unit = { runner.runOneTest("unparseDelimitedString02_ffb") }
  @Test def test_unparseDelimitedString03_ffb(): Unit = { runner.runOneTest("unparseDelimitedString03_ffb") }
  @Test def test_unparseDelimitedString04_ffb(): Unit = { runner.runOneTest("unparseDelimitedString04_ffb") }
  @Test def test_unparseDelimitedString05_ffb(): Unit = { runner.runOneTest("unparseDelimitedString05_ffb") }
  @Test def test_unparseDelimitedString06_ffb(): Unit = { runner.runOneTest("unparseDelimitedString06_ffb") }
  @Test def test_unparseDelimitedString07_ffb(): Unit = { runner.runOneTest("unparseDelimitedString07_ffb") }

  @Test def test_parseDelimitedString01_ffb(): Unit = { runner.runOneTest("parseDelimitedString01_ffb") }

  // DFDL-1650
  @Test def test_alignmentPaddingOVC1_ffb(): Unit = { runner.runOneTest("alignmentPaddingOVC1_ffb") }
  @Test def test_alignmentPaddingOVC2_ffb(): Unit = { runner.runOneTest("alignmentPaddingOVC2_ffb") }
  @Test def test_alignmentPaddingOVC3_ffb(): Unit = { runner.runOneTest("alignmentPaddingOVC3_ffb") }
  @Test def test_alignmentPaddingOVC4_ffb(): Unit = { runner.runOneTest("alignmentPaddingOVC4_ffb") }

}
