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

package org.apache.daffodil.runtime1.processors

import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.MaybeULong

import org.junit.Assert.assertEquals
import org.junit.Test

class TestDataLoc {

  @Test def test_no_bitPos_overflow_01() = {
    val dl = new DataLoc(
      Int.MaxValue.toLong + 2,
      MaybeULong.Nope,
      Left(null),
      Maybe.Nope
    )
    assertEquals(2147483649L, dl.bitPos1b)
    assertEquals(2147483648L, dl.bitPos0b)
    assertEquals(268435457L, dl.bytePos1b)
    assertEquals(268435456L, dl.bytePos0b)
  }

  @Test def test_no_bitPos_overflow_02() = {
    val dl = new DataLoc(
      ((Int.MaxValue.toLong + 1) * 8) + 1,
      MaybeULong.Nope,
      Left(null),
      Maybe.Nope
    )
    assertEquals(17179869185L, dl.bitPos1b)
    assertEquals(17179869184L, dl.bitPos0b)
    assertEquals(2147483649L, dl.bytePos1b)
    assertEquals(2147483648L, dl.bytePos0b)
  }

}
