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

package org.apache.daffodil.lib.testEquality

import org.apache.daffodil.lib.equality._

import org.junit.Assert._
import org.junit.Test

class TestEqualityOperators {

  @Test
  def testConveribleNumberEquality(): Unit = {
    val x = 5
    val y = 6L
    assertFalse(x.toLong =#= y)
    // assertFalse (x =#= "x")  // compile error - wrong types
    assertFalse(5 =#= 6.0.toInt)
  }

  @Test
  def testStronglyTypedEquality(): Unit = {
    val x = List(1, 2, 3)
    val y = Seq(1, 2, 3)
    assertTrue(x =:= y) // allowed since they're subtypes
    // assertFalse(x =:= "List(1, 2, 3") // compile error
    assertFalse(Nil =:= x)
  }

  // prevent optimizations from using constant objects
  val xObj = if (scala.math.random() == -0.0) "foo" else "bar"
  val yObj = if (scala.math.random() == -0.0) "bar" else "foo"

  @Test
  def testStronglyTypedEqualityInlineAnyRef(): Unit = {
    if (TestEqualityOperators.compare(xObj, yObj))
      fail("equal")
  }

  private val ylong = scala.math.random().toLong

  @Test
  def testStronglyTypedEqualityInline(): Unit = {
    val x = 5
    val y = ylong
    if (TestEqualityOperators.compareIntLong(x, y))
      fail("equal")
  }
}

/**
 * By looking at the byte code for the methods of this
 * object, one can determine whether these typed equality operators
 * are allocating or not, and what code is generated.
 */
object TestEqualityOperators {

  def compare(x: String, y: String) = {
    x =:= y
  }

  def compareIntLong(x: Int, y: Long) = {
    x.toLong =#= y
  }
}
