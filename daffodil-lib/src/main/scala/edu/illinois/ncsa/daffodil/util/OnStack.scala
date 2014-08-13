package edu.illinois.ncsa.daffodil.util

import scala.collection.mutable.Stack
import java.util.regex.Matcher

/**
 * Create a thread-local resource pool.
 *
 * It's a stack, so that we can even use it in recursive programs
 * (which the DFDL compiler IS most definitely)
 */

class OnStack[T](constructorFunc: => T, optionalResetFunc: (T => Unit)) extends ThreadLocal[Stack[T]] {

  /**
   * Can specify just the allocator, or you can
   * specify both an allocator, and a resetter which will be used to reset
   * objects popped from the stack.
   */
  def this(constructorFunc: => T) = this(constructorFunc, x => {})

  final protected def constructor = constructorFunc

  protected final override def initialValue(): Stack[T] = {
    val stack = new Stack[T]
    stack
  }

  def apply[R](body: T => R): R = {
    val stack = this.get()
    val thing =
      if (stack.isEmpty) constructor
      else stack.pop()
    optionalResetFunc(thing)
    val result =
      try {
        body(thing)
      } finally {
        stack.push(thing)
      }
    result
  }
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
