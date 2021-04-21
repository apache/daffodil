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

package org.apache.daffodil.section10.representation_properties

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestRepProps2 {
  val testDir = "/org/apache/daffodil/section10/representation_properties/"
  val runner = Runner(testDir, "encodings.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}

class TestRepProps2 {
  import TestRepProps2._
  
  @Test def test_ebcdic1() = { runner.runOneTest("ebcdic1") }
  @Test def test_bits1() = { runner.runOneTest("bits1") }
  @Test def test_bits1a() = { runner.runOneTest("bits1a") }
  @Test def test_bits2() = { runner.runOneTest("bits2") }
  @Test def test_bits2a() = { runner.runOneTest("bits2a") }

  @Test def test_bitsTerm1() = { runner.runOneTest("bitsTerm1") }

  // fails Left-over data byte 1 limit(bytes) 2
  @Test def test_bitsTerm2() = { runner.runOneTest("bitsTerm2") }
  @Test def test_bitsTerm3() = { runner.runOneTest("bitsTerm3") }
  
  @Test def test_fiveBitDFI1661DUI001() = { runner.runOneTest("fiveBitDFI1661DUI001") }
  @Test def test_fiveBitDFI1661DUI001_roundTrip() = { runner.runOneTest("fiveBitDFI1661DUI001_roundTrip") }

  @Test def test_sixBit1() = { runner.runOneTest("sixBit1") }

  @Test def test_iso88591msbbitsmisaligned() = { runner.runOneTest("iso88591msbbitsmisaligned") }  
  @Test def test_iso88591lsbbitsmisaligned() = { runner.runOneTest("iso88591lsbbitsmisaligned") }
}
