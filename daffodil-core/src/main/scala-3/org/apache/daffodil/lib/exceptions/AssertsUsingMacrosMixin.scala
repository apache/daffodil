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

import scala.quoted.*

trait AssertsUsingMacrosMixin {
  this: org.apache.daffodil.lib.exceptions.Assert =>
  import AssertMacros.*

  inline def usage(inline testAbortsIfFalse: Boolean, inline message: String): Unit =
    ${ usageMacro2('testAbortsIfFalse, 'message) }

  inline def usageWithCause(testAbortsIfFalse: Boolean, inline cause: Throwable): Unit =
    if (!testAbortsIfFalse) Assert.usageError(cause)

  inline def usage(inline testAbortsIfFalse: Boolean): Unit = {
    ${ usageMacro1('testAbortsIfFalse) }
  }

  /**
   * test for something that the program is supposed to be ensuring.
   *
   * This is for more complex invariants than the simple 'impossible' case.
   */
  inline def invariant(inline testAbortsIfFalse: Boolean): Unit =
    ${ invariantMacro1('testAbortsIfFalse) }

  /**
   * test for something that the program is supposed to be ensuring, with a custom error message.
   *
   * This is for more complex invariants than the simple 'impossible' case.
   *
   * The msg parameter is only evaluated if the test fails
   */
  inline def invariant(inline testAbortsIfFalse: Boolean, inline msg: String): Unit =
    ${ invariantMacro2('testAbortsIfFalse, 'msg) }

  inline def notYetImplemented(inline testAbortsIfFalse: Boolean): Unit =
    ${ notYetImplementedMacro1('testAbortsIfFalse) }

  inline def notYetImplemented(inline testAbortsIfFalse: Boolean, inline msg: String): Unit =
    ${ notYetImplementedMacro2('testAbortsIfFalse, 'msg) }
}

/**
 * These macros need to be defined within an Object otherwise scala 3 complains about
 * anywhere they're referenced being malformed.We can't define them in macro-lib as
 * they need access to Assert which is lib class and is not accessible from macro-lib
 */
object AssertMacros {
  /*
   * Note that in macro definitions, the argument names here must match the argument names
   * in the macro implementations.
   */

  /**
   * Macro captures the test expression as a string for use in the diagnostic message.
   */
  def usageMacro1(testAbortsIfFalse: Expr[Boolean])(using Quotes): Expr[Unit] = {
    val testAsStringExpr: Expr[String] = Expr(testAbortsIfFalse.show)
    '{ if (!($testAbortsIfFalse)) Assert.usageError($testAsStringExpr) }
  }

  /**
   * Macro captures the test expression as a string for use in the diagnostic message.
   */
  def usageMacro2(testAbortsIfFalse: Expr[Boolean], message: Expr[String])(using
    Quotes
  ): Expr[Unit] = {
    val testAsString: Expr[String] = Expr(testAbortsIfFalse.show)
    '{ if (!($testAbortsIfFalse)) Assert.usageError2($message, $testAsString) }
  }

  /**
   * Macro captures the test expression as a string for use in the diagnostic message.
   */
  def invariantMacro1(testAbortsIfFalse: Expr[Boolean])(using Quotes): Expr[Unit] = {
    val testAsStringExpr: Expr[String] = Expr(testAbortsIfFalse.show)
    '{ if (!($testAbortsIfFalse)) Assert.abort("Invariant broken: " + $testAsStringExpr) }
  }

  /**
   * Macro captures the test expression as a string for use in the diagnostic message.
   */
  def invariantMacro2(testAbortsIfFalse: Expr[Boolean], msg: Expr[String])(using
    Quotes
  ): Expr[Unit] = {
    val testAsStringExpr: Expr[String] = Expr(testAbortsIfFalse.show)
    '{
      if (!($testAbortsIfFalse))
        Assert.abort("Invariant broken: " + $msg + "(" + $testAsStringExpr + ")")
    }
  }

  def notYetImplementedMacro1(
    testThatWillThrowIfTrue: Expr[Boolean]
  )(using Quotes): Expr[Unit] = {
    val testAsString: Expr[String] = Expr(testThatWillThrowIfTrue.show)

    '{
      if ($testThatWillThrowIfTrue) {
        Assert.nyi($testAsString)
      }
    }
  }

  def notYetImplementedMacro2(
    testThatWillThrowIfTrue: Expr[Boolean],
    msg: Expr[String]
  )(using Quotes): Expr[Unit] = {

    val testAsString: Expr[String] = Expr(testThatWillThrowIfTrue.show)

    '{
      if ($testThatWillThrowIfTrue) {
        Assert.nyi($msg + " (" + $testAsString + ")")
      }
    }
  }
}
