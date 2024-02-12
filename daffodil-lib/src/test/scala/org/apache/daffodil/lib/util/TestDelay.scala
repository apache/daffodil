/*
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.apache.daffodil.lib.util

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class TestDelay {
  object Context1 extends NamedMixinBase
  object Context2 extends NamedMixinBase

  @Test def testDelayToString1(): Unit = {
    val d = Delay("context1", "context2", 1 + 2)
    val ds = d.toString
    val v = d.value
    assertEquals("3", v.toString)
    val dsv = d.toString
    assertEquals("Delay(box(context1, context2))", ds)
    assertEquals("Delay(context1, 3)", dsv)
  }

  @Test def testDelayToString2(): Unit = {
    val d = Delay(Context1, Context2, 1 + 2)
    val ds = d.toString
    val v = d.value
    assertEquals("3", v.toString)
    val dsv = d.toString
    assertEquals("Delay(box(Context1, Context2))", ds)
    assertEquals("Delay(Context1, 3)", dsv)
  }

  @Test def testDelayToString3(): Unit = {
    val d = Delay(Context1, 1 + 2)
    val ds = d.toString
    val v = d.value
    assertEquals("3", v.toString)
    val dsv = d.toString
    assertEquals("Delay(box(Context1))", ds)
    assertEquals("Delay(Context1, 3)", dsv)
  }

  @Test def testDelayToString4(): Unit = {
    val d = Delay(null, Context2, 1 + 2)
    val ds = d.toString
    val v = d.value
    assertEquals("3", v.toString)
    val dsv = d.toString
    assertEquals("Delay(box(Context2))", ds)
    assertEquals("Delay(3)", dsv)
  }

  @Test def testDelayToString5(): Unit = {
    val d = Delay(null, null, 1 + 2)
    val ds = d.toString
    val v = d.value
    assertEquals("3", v.toString)
    val dsv = d.toString
    assertTrue(ds.startsWith("Delay(box@"))
    assertTrue(ds.endsWith(")"))
    assertEquals("Delay(3)", dsv)
  }

}
