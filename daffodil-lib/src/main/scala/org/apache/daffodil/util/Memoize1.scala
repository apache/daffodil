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

import scala.collection.mutable.Map

/**
 * A memoizing funtion is one that keeps a cache of what arguments it
 * has already seen, and if it sees the argument again, it does not recompute
 * it just takes the answer from the cache. This saves computation time
 * if finding the object in the cache is substantially cheaper than computing
 * the value, and it is expected that the function will be called with the
 * same arguments repeatedly.
 *
 * This just adds overhead if the argument is unique every time.
 *
 * The other reason to memoize is if you want the result to be EQ equal for equal or
 * equivalent arguments, so as to allow fast EQ comparisons elsewhere in your
 * system. Memoized functions will return the exact same object from the cache
 * every time.
 *
 * This 1-arg version of memoize can be composed using curried functions to obtain
 * 2, 3, or more argument versions, i.e., that memorize all their arguments.
 *
 * See the unit tests for examples of how to do that.
 *
 * Note that the argument must be an AnyRef. This is to force avoidance of
 * passing primitive number types (which are AnyVal) that get boxed, causing inefficient allocation.
 */
class Memoize1[-T <: AnyRef, +R] private (f: T => R) extends (T => R) {
  private[this] var vals = Map.empty[T, R]

  override def apply(x: T): R = {
    val opt = vals.get(x)
    opt match {
      case Some(y) => y
      case None => {
        val y = f(x)
        vals.put(x, y)
        y
      }
    }
  }
}

object Memoize1 {
  def apply[T <: AnyRef, R](f: T => R) = new Memoize1(f).apply _
}
