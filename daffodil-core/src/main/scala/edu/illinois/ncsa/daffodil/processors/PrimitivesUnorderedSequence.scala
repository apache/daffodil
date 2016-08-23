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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.dsom.Term
import edu.illinois.ncsa.daffodil.dsom.Sequence
import edu.illinois.ncsa.daffodil.grammar.UnaryGram
import edu.illinois.ncsa.daffodil.grammar.HasNoUnparser

object UnorderedSequence {
  def apply(context: Term, eGram: => Gram) = {
    // mandatory little optimization here. If there are no statements (most common case), then let's
    // shortcut and just use the guts parser.

    Assert.usageErrorUnless(context.isInstanceOf[Sequence], "The context passed to UnorderedSequence must be a Sequence.")

    val ctxt = context.asInstanceOf[Sequence]

    new UnorderedSequence(ctxt, eGram)
  }
}

class UnorderedSequence private (context: Sequence, eGramArg: => Gram) // private to force use of the object as factory
  extends UnaryGram(context, eGramArg) with HasNoUnparser {

  lazy val eGram = eGramArg // once only
  
  // Forced as part of required evaluations in Sequence
  //context.checkIfValidUnorderedSequence

  val uoSeqParser = eGram.parser
  val sortOrder = {
    val members = context.groupMembers.map(t => {
      t match {
        case s: Sequence => s.groupMembers
        case _ => Seq(t)
      }
    }).flatten

    members.map(t => {
      val erd = t.runtimeData.asInstanceOf[ElementRuntimeData]
      val name = erd.name
      val ns = erd.targetNamespace
      (name, ns)
    })
  }

  val scalarMembers =
    context.groupMembers.filter(t => t.isInstanceOf[ElementBase]).filter {
      case eb: ElementBase => eb.isScalar
    }.map {
      case eb: ElementBase => {
        val erd = eb.runtimeData.asInstanceOf[ElementRuntimeData]
        (erd.name, erd.path, erd.targetNamespace)
      }
    }

  lazy val parser: Parser = ??? // new UnorderedSequenceParser(context.modelGroupRuntimeData, sortOrder, scalarMembers, uoSeqParser)

}
