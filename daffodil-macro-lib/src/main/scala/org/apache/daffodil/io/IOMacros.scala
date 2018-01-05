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

package org.apache.daffodil.io

import scala.reflect.macros.blackbox.Context

object IOMacros {

  /**
   * For Data Input Streams
   *
   * Used to temporarily vary the bit length limit.
   *
   * Implementing as a macro eliminates the creation of a downward function object every time this
   * is called.
   *
   */
  def withBitLengthLimitMacroForInput(c: Context)(lengthLimitInBits: c.Tree)(body: c.Tree) = {

    import c.universe._

    val dStream = TermName(c.freshName)
    val newLengthLimit = TermName(c.freshName)
    val savedLengthLimit = TermName(c.freshName)
    // c.prefix is the expression this macro was expanded on. Not quite same thing as 'this' because we have to be
    // careful not to use it more than once or it will evaluate more than once.
    val selfExp = c.prefix

    q"""{
    import org.apache.daffodil.util.MaybeULong

    val $dStream = $selfExp
    val $newLengthLimit = $lengthLimitInBits
    val $savedLengthLimit = $dStream.bitLimit0b

    if (!$dStream.setBitLimit0b(MaybeULong($dStream.bitPos0b + $newLengthLimit))) false
    else {
      try {
        $body
      } finally {
        $dStream.resetBitLimit0b($savedLengthLimit)
      }
      true
    }
  }"""
  }

  /**
   * For Data output streams
   *
   * Used to temporarily vary the bit length limit.
   *
   * Implementing as a macro eliminates the creation of a downward function object every time this
   * is called.
   *
   */
  //  def withBitLengthLimitMacroForOutput(c: Context)(lengthLimitInBits: c.Tree)(body: c.Tree) = {
  //
  //    import c.universe._
  //
  //    val dStream = TermName(c.freshName)
  //    val newLengthLimit = TermName(c.freshName)
  //    val savedLengthLimit = TermName(c.freshName)
  //    // c.prefix is the expression this macro was expanded on. Not quite same thing as 'this' because we have to be
  //    // careful not to use it more than once or it will evaluate more than once.
  //    val selfExp = c.prefix
  //
  //    q"""{
  //    import org.apache.daffodil.util.MaybeULong
  //    import org.apache.daffodil.io.DataOutputStream
  //
  //    val $dStream: DataOutputStream = $selfExp
  //    val $newLengthLimit = $lengthLimitInBits
  //    val $savedLengthLimit = $dStream.maybeRelBitLimit0b
  //
  //    if (!$dStream.setMaybeRelBitLimit0b(MaybeULong($dStream.relBitPos0b + $newLengthLimit))) false
  //    else {
  //      try {
  //        $body
  //      } finally {
  //        $dStream.resetMaybeRelBitLimit0b($savedLengthLimit)
  //      }
  //      true
  //    }
  //  }"""
  //  }

}
