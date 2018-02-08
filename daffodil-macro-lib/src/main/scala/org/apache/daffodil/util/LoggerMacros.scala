/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.util

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
        $ths.doLogging($level, $msg, Seq(..$args))
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
