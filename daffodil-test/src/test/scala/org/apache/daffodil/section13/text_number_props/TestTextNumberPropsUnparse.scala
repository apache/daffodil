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

package org.apache.daffodil.section13.text_number_props

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner

object TestTextNumberPropsUnparse {
  val testDir = "/org/apache/daffodil/section13/text_number_props/"

  val runner = Runner(testDir, "TextNumberPropsUnparse.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }
}
class TestTextNumberPropsUnparse {

  import TestTextNumberPropsUnparse._

  @Test def test_unparseDelimitedPaddedString01(): Unit = { runner.runOneTest("unparseDelimitedPaddedString01") }
  @Test def test_unparseDelimitedPaddedString02(): Unit = { runner.runOneTest("unparseDelimitedPaddedString02") }
  @Test def test_unparseDelimitedPaddedString03(): Unit = { runner.runOneTest("unparseDelimitedPaddedString03") }
  @Test def test_unparseDelimitedPaddedString04(): Unit = { runner.runOneTest("unparseDelimitedPaddedString04") }
  @Test def test_unparseDelimitedPaddedString05(): Unit = { runner.runOneTest("unparseDelimitedPaddedString05") }
  @Test def test_unparseDelimitedPaddedString06(): Unit = { runner.runOneTest("unparseDelimitedPaddedString06") }
  @Test def test_unparseDelimitedPaddedString07(): Unit = { runner.runOneTest("unparseDelimitedPaddedString07") }
  @Test def test_unparseDelimitedPaddedString08(): Unit = { runner.runOneTest("unparseDelimitedPaddedString08") }
  @Test def test_unparseDelimitedPaddedString09(): Unit = { runner.runOneTest("unparseDelimitedPaddedString09") }
  @Test def test_unparseDelimitedPaddedString11(): Unit = { runner.runOneTest("unparseDelimitedPaddedString11") }
  @Test def test_unparseDelimitedPaddedString12(): Unit = { runner.runOneTest("unparseDelimitedPaddedString12") }
  @Test def test_unparseDelimitedPaddedString13(): Unit = { runner.runOneTest("unparseDelimitedPaddedString13") }
  @Test def test_unparseDelimitedPaddedString14(): Unit = { runner.runOneTest("unparseDelimitedPaddedString14") }

  @Test def test_unparsePaddedString10(): Unit = { runner.runOneTest("unparsePaddedString10") }

  @Test def test_unparsePaddedStringTruncate01(): Unit = { runner.runOneTest("unparsePaddedStringTruncate01") }
  @Test def test_unparsePaddedStringTruncate02(): Unit = { runner.runOneTest("unparsePaddedStringTruncate02") }
  @Test def test_unparsePaddedStringTruncate03(): Unit = { runner.runOneTest("unparsePaddedStringTruncate03") }
  @Test def test_unparsePaddedStringTruncate04(): Unit = { runner.runOneTest("unparsePaddedStringTruncate04") }
  @Test def test_unparsePaddedStringTruncate05(): Unit = { runner.runOneTest("unparsePaddedStringTruncate05") }
  @Test def test_unparsePaddedStringTruncate06(): Unit = { runner.runOneTest("unparsePaddedStringTruncate06") }

  @Test def test_parseDelimitedPaddedString01(): Unit = { runner.runOneTest("parseDelimitedPaddedString01") }

  @Test def test_unparse_int_01(): Unit = { runner.runOneTest("unparse_int_01") }
  @Test def test_parse_int_01(): Unit = { runner.runOneTest("parse_int_01") }

  @Test def test_unparse_tnp_01(): Unit = { runner.runOneTest("unparse_tnp_01") }
  @Test def test_unparse_tnp_02(): Unit = { runner.runOneTest("unparse_tnp_02") }
  @Test def test_unparse_tnp_03(): Unit = { runner.runOneTest("unparse_tnp_03") }
  @Test def test_unparse_tnp_04(): Unit = { runner.runOneTest("unparse_tnp_04") }
  @Test def test_unparse_tnp_05a(): Unit = { runner.runOneTest("unparse_tnp_05a") }
  @Test def test_unparse_tnp_05b(): Unit = { runner.runOneTest("unparse_tnp_05b") }
}
