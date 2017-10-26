/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.grammar

import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.dsom.Term
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.grammar.primitives.EncodingChange

trait EncodingChangeMixin extends GrammarMixin { self: Term =>

  private lazy val enclosingEncoding = enclosingTerm.map(_.encodingEv)
  private lazy val priorSiblingEncoding = priorSibling.map(_.encodingEv)
  private lazy val encodingBefore: Evaluatable[String] = priorSiblingEncoding.getOrElse(enclosingEncoding.getOrElse(encodingEv))

  protected lazy val isKnownSameEncoding: Boolean = {
    if (enclosingTerm.isEmpty) false // root always gets a encoding change.
    else {
      val thisEncoding = encodingEv
      val otherEncoding = encodingBefore
      if (thisEncoding.isConstant && otherEncoding.isConstant &&
        thisEncoding.optConstant.get =:= otherEncoding.optConstant.get) {
        if (priorSiblingEncoding.isDefined) {
          // in addition to having the same encoding as the prior sibling, all
          // children of that prior sibling must have the same encoding.
          // Otherwise, a child could change the encoding and it might not be
          // the same as ours.
          priorSibling.get.hasUniformEncodingThroughout
        } else {
          true
        }
      } else {
        false
      }
    }
  }

  protected lazy val hasUniformEncodingThroughout: Boolean = termChildren.map { t => t.isKnownSameEncoding && t.hasUniformEncodingThroughout }.forall(x => x)

  protected final lazy val encodingChange = prod("encodingChange",
    !isKnownSameEncoding ||
      (isArray && !hasUniformEncodingThroughout)) { EncodingChange(this) }
}
