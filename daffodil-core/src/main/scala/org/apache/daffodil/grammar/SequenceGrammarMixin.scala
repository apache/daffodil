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

package org.apache.daffodil.grammar
import org.apache.daffodil.schema.annotation.props.gen._
import org.apache.daffodil.grammar.primitives.SequenceCombinator
import org.apache.daffodil.dsom.SequenceTermBase

trait SequenceGrammarMixin extends GrammarMixin { self: SequenceTermBase =>

  final override lazy val groupContent = prod("groupContent") {
    self.sequenceKind match {
      case SequenceKind.Ordered => orderedSequenceContent
      case SequenceKind.Unordered => subsetError("Unordered sequences are not supported.") // unorderedSequenceContent
    }
  }

  private lazy val orderedSequenceContent = prod("sequenceContent") {
    SequenceCombinator(this, terms)
  }

  //  private lazy val unorderedSequenceContent = prod("unorderedSequenceContent") {
  //    val uoseq = self.unorderedSeq.get
  //    UnorderedSequenceCombinator(this, uoseq.terms)
  //  }

  protected lazy val terms = groupMembers.map { _.asTermInSequence }

  /**
   * These are static properties even though the delimiters can have runtime-computed values.
   * The existence of an expression to compute a delimiter is assumed to imply a non-zero-length, aka a real delimiter.
   */
  final lazy val hasPrefixSep = sepExpr(SeparatorPosition.Prefix)

  final lazy val hasInfixSep = sepExpr(SeparatorPosition.Infix)

  final lazy val hasPostfixSep = sepExpr(SeparatorPosition.Postfix)

  // note use of pass by value. We don't want to even need the SeparatorPosition property unless there is a separator.
  private def sepExpr(pos: => SeparatorPosition): Boolean = {
    if (hasSeparator) if (separatorPosition eq pos) true else false
    else false
  }

  final lazy val hasSeparator = separatorParseEv.isKnownNonEmpty
}

