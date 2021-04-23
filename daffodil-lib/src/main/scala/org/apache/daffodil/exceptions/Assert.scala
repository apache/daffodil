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

package org.apache.daffodil.exceptions

import org.apache.daffodil.util.Misc

/**
 * Lightweight exception that doesn't construct stack traces, and
 * doesn't construct a format string unless the message is needed.
 *
 * It still has to allocate an argument list, if there are message args,
 * but that may exist anyway.
 *
 * The dummy first argument in the protected constructor allows us to have
 * the public constructors with the signatures we want.
 */
abstract class ThinException protected (dummy: Int, cause: Throwable, fmt: String, args: Any*)
  extends Exception(null, cause, false, false) {

  private lazy val msg_ =
    if (fmt ne null) fmt.format(args: _*)
    else if (cause ne null) cause.getMessage()
    else Misc.getNameFromClass(this)

  override def getMessage() = msg_

  def this() = this(1, null, null)
  def this(msg: String) = this(1, null, msg)
  def this(fmt: String, args: Any*) = this(1, null, fmt, args: _*)
  def this(cause: Throwable) = this(1, cause, null)
}

// $COVERAGE-OFF$ These exception objects should never be created by tests.
abstract class UnsuppressableException(m: String, th: Throwable) extends Exception(m, th) {
  def this(msg: String) = this(msg, null)
  def this(th: Throwable) = this(null, th)
}

class UsageException(m: String) extends UnsuppressableException(m)
class NotYetImplementedException(m: String) extends UnsuppressableException("Not yet implemented: " + m)
class Abort(m: String, th: Throwable) extends UnsuppressableException(m, th) {
  def this(th: Throwable) = this(null, th)
  def this(m: String) = this(m, null)
}

class Assert {
  def shortBacktrace = {
    val frames = Thread.currentThread().getStackTrace().toList.take(6).tail.tail
    frames.map { _.toString }.mkString("\n")
  }

  def toss(x: Throwable) = {
    throw x
  }
}
// $COVERAGE-ON$

object Assert extends Assert {

  /*
   * Note that in macro definitions, the argument names here must match the argument names
   * in the macro implementations.
   */

  /**
   * Verbose name helps you get the sense of the predicate right.
   */
  def usageErrorUnless(testAbortsIfFalse: Boolean, message: String): Unit = macro AssertMacros.usageMacro2
  def usageErrorUnless(testAbortsIfFalse: Boolean): Unit = macro AssertMacros.usageMacro1
  /**
   * Brief form
   */
  def usage(testAbortsIfFalse: Boolean, message: String): Unit = macro AssertMacros.usageMacro2
  def usage(testAbortsIfFalse: Boolean): Unit = macro AssertMacros.usageMacro1

  /**
   * test for something that the program is supposed to be ensuring.
   *
   * This is for more complex invariants than the simple 'impossible' case.
   */
  def invariant(testAbortsIfFalse: Boolean): Unit = macro AssertMacros.invariantMacro1

  /**
   * test for something that the program is supposed to be ensuring, with a custom error message.
   *
   * This is for more complex invariants than the simple 'impossible' case.
   *
   * The msg parameter is only evaluated if the test fails
   */
  def invariant(testAbortsIfFalse: Boolean, msg: String): Unit = macro AssertMacros.invariantMacro2

  /**
   * Conditional behavior for NYIs
   */
  def notYetImplemented(testThatWillThrowIfTrue: Boolean): Unit = macro AssertMacros.notYetImplementedMacro1
  def notYetImplemented(testThatWillThrowIfTrue: Boolean, msg: String): Unit = macro AssertMacros.notYetImplementedMacro2

  // $COVERAGE-OFF$ These unconditional assertions should never get executed by tests.

  def notYetImplemented(): Nothing = macro AssertMacros.notYetImplementedMacro0
  //
  // Throughout this file, specifying return type Nothing
  // gets rid of many spurious (scala compiler bug) dead code
  // warnings. It doesn't hurt to have them, so they're in now
  // which allows the scala compiler checking for dead-code pass
  // to be enabled.
  //

  def usageError(message: String = "Usage error."): Nothing = {
    abort(message)
  }

  def nyi(info: String): Nothing = {
    toss(new NotYetImplementedException(info + "\n" + shortBacktrace))
  }

  def nyi(): Nothing = {
    toss(new NotYetImplementedException(shortBacktrace))
  }

  def abort(message: String = ""): Nothing = {
    toss(new Abort(message + "\n" + shortBacktrace))
  }

  /**
   * Like abort, but takes 2nd argument that is expected to be the text
   * of the test expression (as captured by macro.
   */
  def abort2(message: String, testAsString: String): Nothing = {
    abort(message + " (" + testAsString + ")")
  }

  def abort(th: Throwable): Nothing = {
    toss(new Abort(th))
  }

  def impossible(message: String = "impossible! this code path is supposed to be unreachable."): Nothing = {
    abort(message)
  }

  /**
   * use when a match/case has exhausted all possibles.
   *
   * Sometimes, if you are just dispatching on an enum, scala can prove you've exhausted all
   * possibles. In other cases, use this. Eg., exhaustive case analysis on unsealed case classes (so
   * the compiler can't assure you, but you still believe you are being exhaustive). Under program
   * maintenance people break these things. Hence, use this to catch those kinds of fall throughs.
   */
  def impossibleCase(x: Any = null) = {
    val extra =
      if (x == null) ""
      else " Value was: " + x + "."
    impossible("Should be no fall through to this case." + extra)
  }

  def impossibleCase: Nothing = impossibleCase(null)

  /**
   * Use when a case or if/then analysis has fallen through to a situation that
   * a program invariant should be assuring doesn't happen. That is, where
   * the case analysis has exhaused all the situations that are consistent with
   * the invariant.
   *
   * This is different from an impossible - those are for situations which
   * are simpler to show are impossible.
   */
  def invariantFailed(msg: String = "") = {
    abort("Invariant broken. " + msg)
  }
  // $COVERAGE-ON$

}
