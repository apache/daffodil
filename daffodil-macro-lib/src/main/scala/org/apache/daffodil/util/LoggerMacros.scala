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

package org.apache.daffodil.util

import scala.reflect.macros.blackbox.Context

object LoggerMacros {

  def logMacro(c: Context)(lvl: c.Tree, msg: c.Tree, args: c.Tree*) = {
    import c.universe._

    val level = TermName(c.freshName)
    val l = TermName(c.freshName)

    /*
     *  ths is the stand in for 'this', and it means not the object where
     *  log is called on. E.g., in
     *  {{{
     *
     *  class Foo(...) extends Logging {
     *  ...
     *     bar.baz.log(...)
     *
     *  ...
     *  }
     *  }}}
     *  In the above, the expansion of log is going to have 'this' mean the
     *  instance of class Foo where the macro is being expanded. This is not
     *  at all like a method, where this means the object where the method is
     *  being called. But this is the nature of macros.
     *
     *  If you think of just text substitution, then the binding of the name 'this'
     *  comes from the surrounding context of the code text, not the object
     *  whose type provided the source of the macro.
     *
     *  TBD: what is the idiom for getting the object instance on which the
     *  log "macro method" was called? That is, the other possible thing that
     *  one might want that one might think of as 'this'.
     */

    val ths = This(typeNames.EMPTY)
    q"""
    {
      val $level = $lvl
      val $l = $level.lvl
      if ($ths.getLoggingLevel().lvl >= $l)
        $ths.doLogging($level, $msg, Seq[Any](..$args))
    }
    """
  }

  /**
   * Use to make debug printing over small code regions convenient. Turns on
   * your logging level of choice over a lexical region of code. Makes sure it is reset
   * to whatever it was on the exit, even if it throws.
   */
  def withLoggingLevelMacro(c: Context)(newLevel: c.Tree)(body: c.Tree) = {

    import c.universe._

    val previousLogLevel = TermName(c.freshName)

    q"""{
    val $previousLogLevel = logLevel
    logLevel = One($newLevel)
    try {
      $body
    }
    finally {
      logLevel = $previousLogLevel
    }
    }"""
  }

}
