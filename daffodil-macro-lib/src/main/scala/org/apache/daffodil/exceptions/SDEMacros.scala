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

object SDEMacros {

  def schemaDefinitionUnlessMacro(c: Context)(testThatWillThrowIfFalse: c.Expr[Boolean], str: c.Expr[String], args: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    val selfExp = c.prefix

    c.Expr(q"""
    {
      if (!($testThatWillThrowIfFalse)) {
        $selfExp.SDE($str, ..$args)
      }
    }
    """)
  }

  def schemaDefinitionWhenMacro(c: Context)(testThatWillThrowIfTrue: c.Expr[Boolean], str: c.Expr[String], args: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    val selfExp = c.prefix

    c.Expr(q"""
    {
      if ($testThatWillThrowIfTrue) {
        $selfExp.SDE($str, ..$args)
      }
    }
    """)
  }

  def schemaDefinitionWarningUnlessMacro(c: Context)(testThatWillWarnIfFalse: c.Expr[Boolean], str: c.Expr[String], args: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    val selfExp = c.prefix

    c.Expr(q"""
    {
      if (!($testThatWillWarnIfFalse)) {
        $selfExp.SDW($str, ..$args)
      }
    }
    """)
  }

  def schemaDefinitionWarningUnlessSuppressMacro(c: Context)(warnID: c.Tree, testThatWillWarnIfFalse: c.Expr[Boolean], str: c.Expr[String], args: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    val selfExp = c.prefix

    c.Expr(q"""
    {
      if (!($testThatWillWarnIfFalse)) {
        $selfExp.SDW($warnID, $str, ..$args)
      }
    }
    """)
  }

  def schemaDefinitionWarningWhenMacro(c: Context)(testThatWillWarnIfTrue: c.Expr[Boolean], str: c.Expr[String], args: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    val selfExp = c.prefix

    c.Expr(q"""
    {
      if ($testThatWillWarnIfTrue) {
        $selfExp.SDW($str, ..$args)
      }
    }
    """)
  }

  def schemaDefinitionWarningWhenSuppressMacro(c: Context)(warnID: c.Tree, testThatWillWarnIfTrue: c.Expr[Boolean], str: c.Expr[String], args: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    val selfExp = c.prefix

    c.Expr(q"""
    {
      if ($testThatWillWarnIfTrue) {
        $selfExp.SDW($warnID, $str, ..$args)
      }
    }
    """)
  }
}
