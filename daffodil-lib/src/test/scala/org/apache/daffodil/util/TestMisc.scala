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

package org.apache.daffodil.util

import org.junit.Test
import org.junit.Assert._

class TestMisc {

  @Test def testIsAllUpper(): Unit = {
    assertTrue(Misc.isAllUpper("A", 0))
    assertFalse(Misc.isAllUpper("a", 0))
    assertTrue(Misc.isAllUpper("AB", 0))
    assertFalse(Misc.isAllUpper("Ab", 0))
    assertTrue(Misc.isAllUpper("ABC", 0))
    assertFalse(Misc.isAllUpper("ABc", 0))

    assertTrue(Misc.isAllUpper("AB", 1))
    assertFalse(Misc.isAllUpper("Ab", 1))
    assertTrue(Misc.isAllUpper("ABC", 1))
    assertFalse(Misc.isAllUpper("ABc", 1))
  }

  @Test def testToInitialLowerUnlessAllUpper(): Unit = {
    assertEquals("fooBar", Misc.toInitialLowerCaseUnlessAllUpperCase("FooBar"))
    assertEquals("FOOBAR", Misc.toInitialLowerCaseUnlessAllUpperCase("FOOBAR"))
    assertEquals("fOOBAR", Misc.toInitialLowerCaseUnlessAllUpperCase("fOOBAR"))
    assertEquals("foobar", Misc.toInitialLowerCaseUnlessAllUpperCase("Foobar"))
  }
}
