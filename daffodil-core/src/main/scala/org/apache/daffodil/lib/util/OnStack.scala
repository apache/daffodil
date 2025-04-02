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

import java.util.regex.Matcher
import scala.collection.mutable

sealed abstract class LocalStackBase[T](constructorFunc: => T, optionalResetFunc: (T => Unit)) {

  protected def stack: mutable.Stack[T]

  /**
   * This must be inlined to achieve what we're trying to achieve with OnStack/LocalStack.
   * If not inlined, then this will cause a closure to get allocated corresponding to the passed
   * function. The whole point of this is NOT to allocate objects. I.e., one uses OnStack to avoid
   * allocation of "little crud objects".
   */
  @inline final def apply[R](body: T => R): R = {
    val thing =
      if (stack.isEmpty) constructorFunc
      else stack.pop()
    val result =
      try {
        body(thing)
      } finally {
        optionalResetFunc(thing) // reset before we stack it.
        stack.push(thing)
      }
    result
  }
}

/**
 * Create a thread-local resource pool.
 *
 * It's a stack, so that we can even use it in recursive programs
 * (which the DFDL compiler IS most definitely)
 */
class OnStack[T](constructorFunc: => T, optionalResetFunc: (T => Unit))
  extends LocalStackBase[T](constructorFunc, optionalResetFunc) {

  /**
   * Can specify just the allocator, or you can
   * specify both an allocator, and a resetter which will be used to reset
   * objects popped from the stack.
   */
  def this(constructorFunc: => T) = this(constructorFunc, x => {})

  private val tl = new ThreadLocal[mutable.Stack[T]] {
    protected final override def initialValue(): mutable.Stack[T] = {
      val stack = new mutable.Stack[T]
      stack
    }
  }

  override final protected def stack = tl.get()

}

/**
 * Use this when you already have access to thread-specific state someplace.
 *
 * So you can add a member to your thread-local state object like
 * `val withMatcherOnStack = new LocalStack[Matcher](pattern.matcher(""))`
 * and use it like:
 * `withMatcherOnStack{ m => ... }`
 *
 * This just avoids the access penalty associated with a ThreadLocal vs. just
 * a data member. In many cases however, that hash lookup is more expensive than
 * what you want to do with the local object.
 */
class LocalStack[T](constructorFunc: => T, optionalResetFunc: (T => Unit))
  extends LocalStackBase[T](constructorFunc, optionalResetFunc) {

  /**
   * Can specify just the allocator, or you can
   * specify both an allocator, and a resetter which will be used to reset
   * objects popped from the stack.
   */
  def this(constructorFunc: => T) = this(constructorFunc, x => {})

  final protected val stack = new mutable.Stack[T]

}

private[util] object An_example_of_OnStack_use {

  /**
   * In this example, we have a regex pattern
   * that we want to compile, and then we have
   * lots of matchers for it that we're using.
   *
   * We need these matchers (which are stateful)
   * to be thread local, but also we don't want
   * to be allocating new ones all the time. We
   * should just reset them onto new strings
   * and use them over and over.
   */
  val pattern = "ab*".r.pattern // imagine a really big expensive pattern to compile.

  object withMatcher extends OnStack[Matcher](pattern.matcher(""))

  def foo(str: String): Unit = {
    withMatcher { m =>
      //
      // we reset it here in our own code
      // because in case of a matcher, the reset needs an argument
      //
      m.reset(str)

      while (m.find()) {
        m.regionStart() // and so on
      }
    }
  }
}
