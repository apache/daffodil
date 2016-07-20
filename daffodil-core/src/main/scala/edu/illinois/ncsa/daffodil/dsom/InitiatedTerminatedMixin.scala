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

package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.grammar.GrammarMixin

trait InitiatedTerminatedMixin
  extends GrammarMixin
  with AnnotatedMixin
  with DelimitedRuntimeValuedPropertiesMixin { self: Term =>

  private lazy val parentSaysInitiatedContent = {
    val parentSays = self.immediatelyEnclosingModelGroup match {
      case Some(s) if (s.initiatedContent == YesNo.Yes) => true
      case _ => false
    }
    parentSays
  }

  final lazy val hasInitiator = {
    val hasOne = initiatorParseEv.isKnownNonEmpty
    if (parentSaysInitiatedContent)
      schemaDefinitionUnless(hasOne, "Enclosing group has initiatedContent='yes', but initiator is not defined.")
    hasOne
  }

  final lazy val hasTerminator = terminatorParseEv.isKnownNonEmpty

  lazy val initiatorDiscriminator = prod("initiatorDiscriminator", parentSaysInitiatedContent) { InitiatedContent(this) }

  lazy val initiatorRegion = prod("initiatorRegion", hasInitiator) { initiatorItself ~ initiatorDiscriminator }
  lazy val initiatorItself = delimMTA ~ Initiator(this)

  lazy val terminatorRegion = prod("terminatorRegion", hasTerminator) { delimMTA ~ Terminator(this) }

  /**
   * True if this term has initiator, terminator, or separator that are either statically
   * present, or there is an expression. (Such expressions are not allowed to evaluate to "" - you
   * can't turn off a delimiter by providing "" at runtime. Minimum length is 1 for these at runtime.
   * <p>
   * Override in Sequence to also check for separator.
   */
  lazy val hasDelimiters = hasInitiator || hasTerminator

}
