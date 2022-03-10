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

package org.apache.daffodil.section17.calc_value_properties

import org.junit.Test
import org.apache.daffodil.tdml.Runner
import org.junit.AfterClass

object TestComputedLengthFields {
  val testDir = "/org/apache/daffodil/section17/calc_value_properties/"

  val runner = Runner(testDir, "computedLengthFields.tdml")


  @AfterClass def shutDown(): Unit = {
    runner.reset
  }

}

class TestComputedLengthFields {
  import TestComputedLengthFields._

  @Test def test_computedLengthAroundPrefixedLengths1p(): Unit = { runner.runOneTest("computedLengthAroundPrefixedLengths1p") }

  // DAFFODIL-2626 - deadlock interaction between computed length and prefixed-length strings.
  // @Test
  def test_computedLengthAroundPrefixedLengths1u(): Unit = { runner.runOneTest("computedLengthAroundPrefixedLengths1u") }

  // This test shows you can work around DAFFODIL-2626 using the dfdlx:alignmentKind='manual' property.
  @Test def test_computedLengthAroundPrefixedLengths1uWithAlignmentKindManual(): Unit = {
    runner.runOneTest("computedLengthAroundPrefixedLengths1uWithAlignmentKindManual")
  }

  @Test def test_computedLengthAroundFixedLengths1(): Unit = { runner.runOneTest("computedLengthAroundFixedLengths1") }

  // DAFFODIL-2626 circular deadlock
  // Reproduces one of the circular issues - with prefixed length for the root element surrounding
  // text, where the alignment region isn't optimized out. 
  // @Test
  def test_prefixedAroundDelimitedString1(): Unit = { runner.runOneTest("prefixedAroundDelimitedString1") }

  // This test shows you can work around DAFFODIL-2626 using the dfdlx:alignmentKind='manual' property.
  @Test def test_prefixedAroundDelimitedString1WithAlignmentKindManual(): Unit = {
    runner.runOneTest("prefixedAroundDelimitedString1WithAlignmentKindManual")
  }
}
