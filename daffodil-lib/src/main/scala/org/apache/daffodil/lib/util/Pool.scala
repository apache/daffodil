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

import scala.collection.mutable

import org.apache.daffodil.lib.equality._
import org.apache.daffodil.lib.exceptions.Assert

/**
 * A pool is a collection of objects that are manually recycled usually via
 * some sort of a stack discipline.
 *
 * The point of it is to avoid excessive allocation of little objects.
 *
 * This is not thread safe.
 *
 * Either a pool becomes part of a state block that is separate per thread,
 * or it must be made using a ThreadLocal to get one per thread automatically.
 */

/**
 *  Derive from this for debug purposes if you want to pool things.
 */
trait Poolable {

  /**
   * The debug label is used to report diagnostics about pool management errors.
   * It helps to pinpoint the problematic place where pooled items are being
   * mismanaged. You end up passing say, Misc.getNameFromClass() to this.
   */
  private var poolDebugLabel_ : String = null
  final def setPoolDebugLabel(debugLabel: String): Unit = { poolDebugLabel_ = debugLabel }
  final def poolDebugLabel: String = poolDebugLabel_
}

trait Pool[T <: Poolable] {

  private val pool = new MStackOf[T]

  /**
   * Keeps track of pool objects that have not been freed for debug
   * purposes.
   */
  private val inUse = new mutable.HashSet[T]

  private var numOutstanding: Int = 0

  protected def allocate: T

  final def getFromPool(debugLabel: String): T = {
    numOutstanding += 1
    val instance =
      if (pool.isEmpty) {
        allocate
      } else {
        pool.pop.asInstanceOf[T]
      }
    instance.setPoolDebugLabel(debugLabel)
    inUse += instance
    instance
  }

  final def returnToPool(thing: T): Unit = {
    numOutstanding -= 1
    pool.push(thing)
    inUse -= thing
    // Do Not reset the pool debug label.
    // That way if something is double-returned, we can look at the
    // pool debug label to see what it previously was, as a clue to how
    // it was erroneously double returned.
    // thing.setPoolDebugLabel(null)
  }

  final def isInUse(thing: T) = {
    inUse.contains(thing)
  }

  /**
   * Call this at end of execution to be sure all pooled items
   * have been returned.
   *
   * This is to help find resource leaks where items are taken from
   * the pool, but then dropped.
   */
  final def finalCheck(): Unit = {
    if (!(numOutstanding =#= 0)) {
      val msg =
        "Pool " + Misc.getNameFromClass(this) + " leaked " + numOutstanding + " instance(s)." +
          "\n" + inUse.map { item => "poolDebugLabel = " + item.poolDebugLabel }.mkString("\n")
      Assert.invariantFailed(msg)
    }
  }

}
