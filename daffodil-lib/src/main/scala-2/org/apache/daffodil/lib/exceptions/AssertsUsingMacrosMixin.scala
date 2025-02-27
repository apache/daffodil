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

package org.apache.daffodil.lib.exceptions

trait AssertsUsingMacrosMixin {

  /**
   * Verbose name helps you get the sense of the predicate right.
   */
  def usageErrorUnless(testAbortsIfFalse: Boolean, message: String): Unit =
    macro AssertMacros.usageMacro2

  def usageErrorUnless(testAbortsIfFalse: Boolean, cause: Throwable): Unit =
    macro AssertMacros.usageMacro2Cause

  def usageErrorUnless(testAbortsIfFalse: Boolean): Unit = macro AssertMacros.usageMacro1

  /**
   * Brief form
   */
  def usage(testAbortsIfFalse: Boolean, message: String): Unit = macro AssertMacros.usageMacro2

  def usageWithCause(testAbortsIfFalse: Boolean, cause: Throwable): Unit =
    macro AssertMacros.usageMacro2Cause

  def usage(testAbortsIfFalse: Boolean): Unit =
    macro AssertMacros.usageMacro1

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
  def invariant(testAbortsIfFalse: Boolean, msg: String): Unit =
    macro AssertMacros.invariantMacro2

  /**
   * Conditional behavior for NYIs
   */
  def notYetImplemented(testThatWillThrowIfTrue: Boolean): Unit =
    macro AssertMacros.notYetImplementedMacro1

  def notYetImplemented(testThatWillThrowIfTrue: Boolean, msg: String): Unit =
    macro AssertMacros.notYetImplementedMacro2
}
