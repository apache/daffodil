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

package org.apache.daffodil

import scala.util.DynamicVariable
import org.apache.daffodil.exceptions.Assert

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
