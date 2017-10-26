/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.grammar.primitives

import org.apache.daffodil.dsom._
import org.apache.daffodil.grammar.Terminal
import org.apache.daffodil.grammar.HasNoUnparser

abstract class Primitive(e: Term, guard: Boolean = false)
    extends Terminal(e, guard) {
  override def toString = "Prim[" + name + "]"
}

/**
 * For stubbing out primitives that are placeholders
 */
abstract class UnimplementedPrimitive(e: Term, guard: Boolean = false)
    extends Primitive(e, guard)
    with HasNoUnparser {
  override final lazy val parser = hasNoParser
}

// base stub classes

case class NoValue(e: GlobalElementDecl, guard: Boolean = true) extends UnimplementedPrimitive(e, guard)

case class SaveInputStream(e: ElementBase, guard: Boolean = true) extends UnimplementedPrimitive(e, guard)

case class SetEmptyInputStream(e: ElementBase, guard: Boolean = true) extends UnimplementedPrimitive(e, guard)

case class RestoreInputStream(e: ElementBase, guard: Boolean = true) extends UnimplementedPrimitive(e, guard)

case class NotStopValue(e: ElementBase with LocalElementMixin) extends UnimplementedPrimitive(e, e.hasStopValue)

case class StopValue(e: ElementBase with LocalElementMixin) extends UnimplementedPrimitive(e, e.hasStopValue)

case class TheDefaultValue(e: ElementBase) extends UnimplementedPrimitive(e, e.isDefaultable)

case class UnicodeByteOrderMark(e: GlobalElementDecl) extends UnimplementedPrimitive(e, false)
