/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package org.apache.daffodil.util

import scala.collection.mutable
import java.util.regex.Matcher

sealed abstract class LocalStackBase[T](
    constructorFunc: => T,
    optionalResetFunc: (T => Unit)) {

  protected def stack: mutable.ArrayStack[T]

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
class OnStack[T](
    constructorFunc: => T,
    optionalResetFunc: (T => Unit)) extends LocalStackBase[T](constructorFunc, optionalResetFunc) {

  /**
   * Can specify just the allocator, or you can
   * specify both an allocator, and a resetter which will be used to reset
   * objects popped from the stack.
   */
  def this(constructorFunc: => T) = this(constructorFunc, x => {})

  private val tl = new ThreadLocal[mutable.ArrayStack[T]] {
    protected final override def initialValue(): mutable.ArrayStack[T] = {
      val stack = new mutable.ArrayStack[T]
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
class LocalStack[T](
    constructorFunc: => T,
    optionalResetFunc: (T => Unit)) extends LocalStackBase[T](constructorFunc, optionalResetFunc) {

  /**
   * Can specify just the allocator, or you can
   * specify both an allocator, and a resetter which will be used to reset
   * objects popped from the stack.
   */
  def this(constructorFunc: => T) = this(constructorFunc, x => {})

  final protected val stack = new mutable.ArrayStack[T]

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

  def foo(str: String) {
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
