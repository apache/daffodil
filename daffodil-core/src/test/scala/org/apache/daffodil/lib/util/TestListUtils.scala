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

package org.apache.daffodil.lib.util

import org.junit.Assert.assertEquals
import org.junit.Test

class TestListUtils {

  @Test def testTailAfter1() = {
    val actual = ListUtils.tailAfter(List(1, 2, 3, 4, 5), 3)
    val expected = List(4, 5)
    assertEquals(expected, actual)
  }

  @Test def testTailAfter2() = {
    val actual = ListUtils.tailAfter(Nil, 3)
    val expected = Nil
    assertEquals(expected, actual)
  }

  @Test def testTailAfter3() = {
    val actual = ListUtils.tailAfter(List(1, 2, 3, 4, 5), 5)
    val expected = Nil
    assertEquals(expected, actual)
  }

  @Test def testTailAfter4() = {
    val actual = ListUtils.tailAfter(List(1, 2, 3, 4, 5), 0)
    val expected = Nil // The answer if not found at all is Nil
    assertEquals(expected, actual)
  }

  @Test def testPreceding1() = {
    val actual = ListUtils.preceding(List(1, 2, 3, 4, 5), 3)
    val expected = List(1, 2)
    assertEquals(expected, actual)
  }

  @Test def testPreceding2() = {
    val actual = ListUtils.preceding(Nil, 3)
    val expected = Nil
    assertEquals(expected, actual)
  }

  @Test def testPreceding3() = {
    val actual = ListUtils.preceding(List(1, 2, 3, 4, 5), 1)
    val expected = Nil
    assertEquals(expected, actual)
  }

  @Test def testPreceding4() = {
    val actual = ListUtils.preceding(List(1, 2, 3, 4, 5), 0)
    val expected = Nil // The answer if not found at all is Nil
    assertEquals(expected, actual)
  }

  @Test def testPreceding5() = {
    val actual = ListUtils.preceding(List(1, 2, 3, 4, 5), 5)
    val expected = List(1, 2, 3, 4)
    assertEquals(expected, actual)
  }

}
