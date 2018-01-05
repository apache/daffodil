/* Copyright (c) 2013 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil

import scala.util.DynamicVariable
import edu.illinois.ncsa.daffodil.exceptions.Assert

/**
 * This uses a scala DynamicVariable to create a dynamically scoped binding of
 * the Execution mode information (are we compiling, or are we at runtime).
 *
 * This works as a thread-local variable. So different threads can have independent dynamic
 * bindings.
 *
 * Supposedly this inherits so if one thread creates another, then it will inherit
 * the current value of the dynamic variable. (So, if we use parallelism using say, futures,
 * which is a natural idiom for functional programming, then threads they create would
 * work right.)
 *
 */
object ExecutionMode {

  private sealed class ExecutionModeBase

  // The point of unknown mode is that it allows us to catch
  // situations where we neglected to wrap usingCompilerMode or 
  // usingRuntimeMode around something.

  private case object UnknownMode extends ExecutionModeBase
  private object CompileMode extends ExecutionModeBase
  private object RuntimeMode extends ExecutionModeBase
  private object UnrestrictedMode extends ExecutionModeBase

  private val executionMode = new DynamicVariable[ExecutionModeBase](UnknownMode)

  /**
   * Use this by doing:
   * <pre>
   *     usingCompilerMode {
   *     ... everything in this dynamic scope can call
   *     requireCompilerMode, and it will not do anything
   *     but requireRuntimeMode will abort.
   *     ...
   *     }
   * </pre>
   * Symmetrically for runtime mode.
   */

  final def usingCompilerMode[S] = executionMode.withValue[S](CompileMode) _
  final def usingRuntimeMode[S] = executionMode.withValue[S](RuntimeMode) _
  final def usingUnrestrictedMode[S] = executionMode.withValue[S](UnrestrictedMode) _

  // private final def isCompilerMode = executionMode.value == CompileMode
  private final def isRuntimeMode = executionMode.value == RuntimeMode
  private final def isUnknownMode = executionMode.value == UnknownMode
  private final def isUnrestrictedMode = executionMode.value == UnrestrictedMode

  private def notUnknown = {
    // val msg = "Warning code is not wrapped with either usingCompilerMode or usingRuntimeMode"
    // Assert.usageErrorUnless(!isUnknownMode, msg)
    // if (isUnknownMode) System.err.println(msg)
    !isUnknownMode
  }

  final def requireCompilerMode = {
    //
    // Removed this as it was causing errors when the infoset code wanted
    // to issue an SDE. That code doesn't have a PState around to issue 
    // a runtime SDE, and it isn't clear whether it wants to issue a 
    // regular SDE or a runtime one, because expression evaluation occurs
    // both at compile time and runtime. The ERD object available as a Throws
    // SDE object in the infoset throws regular SDE regardless of when
    //
    // TODO: consider removing this entire mechanism as no longer necessary.
    // 
    //    if (notUnknown && !isUnrestrictedMode)
    //      Assert.invariant(isCompilerMode)
    // if (!isCompilerMode) System.err.println("Doing a compile time thing at runtime!")
  }

  final def requireRuntimeMode = {
    if (notUnknown && !isUnrestrictedMode)
      Assert.invariant(isRuntimeMode)
  }

}
