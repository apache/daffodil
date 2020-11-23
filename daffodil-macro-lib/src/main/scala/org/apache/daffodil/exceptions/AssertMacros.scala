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

import scala.reflect.macros.blackbox.Context

object AssertMacros {

  def usageMacro2(c: Context)(testAbortsIfFalse: c.Tree, message: c.Tree): c.Tree = {
    import c.universe._

    val testAsString = testAbortsIfFalse.toString

    q"""
    if (!($testAbortsIfFalse)) {
         Assert.abort2("Usage error: " + $message, $testAsString)
    }
    """
  }

  def usageMacro1(c: Context)(testAbortsIfFalse: c.Tree): c.Tree = {
    import c.universe._

    val testAsString = testAbortsIfFalse.toString

    q"""
    if (!($testAbortsIfFalse)) {
         Assert.abort("Usage error: " + $testAsString)
    }
    """
  }

  def invariantMacro1(c: Context)(testAbortsIfFalse: c.Tree): c.Tree = {
    import c.universe._

    val testAsString = testAbortsIfFalse.toString

    q"""
    if (!($testAbortsIfFalse)) {
         Assert.abort("Invariant broken: " + $testAsString)
    }
    """
  }

  def invariantMacro2(c: Context)(testAbortsIfFalse: c.Tree, msg: c.Tree): c.Tree = {
    import c.universe._

    q"""
    if (!($testAbortsIfFalse)) {
         Assert.abort("Invariant broken. " + { $msg })
    }
    """
  }

  def notYetImplementedMacro0(c: Context)(): c.Tree = {
    import c.universe._

    q"""
         Assert.nyi()
    """
  }

  def notYetImplementedMacro1(c: Context)(testThatWillThrowIfTrue: c.Tree): c.Tree = {
    import c.universe._

    val testAsString = testThatWillThrowIfTrue.toString

    q"""
    if ($testThatWillThrowIfTrue){
         Assert.nyi($testAsString)
    }
    """
  }

  def notYetImplementedMacro2(c: Context)(testThatWillThrowIfTrue: c.Tree, msg: c.Tree): c.Tree = {
    import c.universe._

    val testAsString = testThatWillThrowIfTrue.toString

    q"""
    if ($testThatWillThrowIfTrue){
         Assert.nyi($msg + " (" + $testAsString + ")")
    }
    """
  }

}
