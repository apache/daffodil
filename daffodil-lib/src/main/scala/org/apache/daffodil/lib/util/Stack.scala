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

import scala.collection.mutable.ListBuffer

/**
 * TODO: scala 2.12 phase out
 * Compatibility class for 2.12 and 2.13 since ArrayStack and Stack
 * have been deprecated in 2.13. This allows us to maintain the same
 * functionality as stack while using ListBuffer instead
 */
class Stack[T] {
  def apply(index: Int): T = {
    _stackLike(index)
  }

  private val _stackLike: ListBuffer[T] = new ListBuffer[T]

  def pop(): T = {
    _stackLike.remove(_stackLike.length - 1)
  }

  def push(item: T): Unit = {
    _stackLike += item
  }

  def isEmpty: Boolean = {
    _stackLike.isEmpty
  }

  def clear(): Unit = {
    // TODO: not covered by tests
    _stackLike.clear()
  }

  def top(): T = {
    _stackLike.last
  }

  def nonEmpty: Boolean = {
    _stackLike.nonEmpty
  }
}
