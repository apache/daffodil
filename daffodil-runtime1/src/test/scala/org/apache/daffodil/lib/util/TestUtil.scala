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

import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.exceptions._

import org.junit.Assert._
import org.junit.Test

class TestUtil {

  @Test def testGetRequiredResourceSucceeds(): Unit = {
    val res = Misc.getRequiredResource("org/apache/daffodil/xsd/XMLSchema.xsd")
    assertNotNull(res)
  }

  @Test def testGetRequiredResourceFails(): Unit = {
    val e = intercept[Exception] {
      Misc.getRequiredResource("org/apache/daffodil/xsd/NotAResourceName.foo")
    }
    assertTrue(e.getMessage().contains("NotAResourceName"))
  }

  @Test
  def testStripQuotes(): Unit = {
    assertEquals("foo", Misc.stripQuotes("\"foo\""))
  }

  @Test
  def testAssert(): Unit = {
    try {
      Assert.abort("yadda")
      // fail()
    } catch {
      case u: UnsuppressableException => // ok
        assertTrue(u.getMessage().contains("yadda"))
    }
  }

  @Test
  def testBitsConverters1(): Unit = {
    val bytes = Misc.bits2Bytes("11")
    val theByte = bytes(0)
    assertEquals(3, theByte.toInt)
  }

  @Test
  def testBitsConverters2(): Unit = {
    val bytes = Misc.bits2Bytes("110110110110")
    val byte0 = bytes(0)
    val byte1 = bytes(1)
    assertEquals(-37, byte0.toInt)
    assertEquals(6, byte1.toInt)
  }

}
