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

package edu.illinois.ncsa.daffodil.grammar.primitives

import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.dsom.Term
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.grammar.Terminal
import edu.illinois.ncsa.daffodil.processors.parsers.AlignmentFillParser
import edu.illinois.ncsa.daffodil.processors.parsers.MandatoryTextAlignmentParser
import edu.illinois.ncsa.daffodil.processors.parsers.Parser
import edu.illinois.ncsa.daffodil.processors.parsers.SkipRegionParser
import edu.illinois.ncsa.daffodil.processors.unparsers.AlignmentFillUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.MandatoryTextAlignmentUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.SkipRegionUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.Unparser
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthKind

abstract class SkipRegion(e: Term, skipLengthInBits: Int, propName: String) extends Terminal(e, skipLengthInBits > 0) {

  e.schemaDefinitionUnless(skipLengthInBits < e.tunable.maxSkipLengthInBytes * 8,
    "Property %s %s(bits) is larger than limit %s(bits).", propName, skipLengthInBits, e.tunable.maxSkipLengthInBytes * 8)

  final lazy val parser: Parser = new SkipRegionParser(skipLengthInBits, e.runtimeData)
  final lazy val unparser: Unparser = new SkipRegionUnparser(skipLengthInBits, e.runtimeData, e.fillByteEv)
}

case class LeadingSkipRegion(e: Term) extends SkipRegion(e, e.leadingSkipInBits, "leadingSkip")

case class TrailingSkipRegion(e: Term) extends SkipRegion(e, e.trailingSkipInBits, "trailingSkip") {

  val lengthKindContext = e match {
    case eb: ElementBase => eb
    case _ => {
      Assert.invariant(e.nearestEnclosingElement != None) //root element is an ElementBase, all others have a nearestEnclosingElement
      e.nearestEnclosingElement.get
    }
  }
  e.schemaDefinitionWhen(e.trailingSkip > 0 && lengthKindContext.lengthKind == LengthKind.Delimited && !e.hasTerminator,
    "Property terminator must be defined when trailingSkip > 0 and lengthKind='delimited'")
}

case class AlignmentFill(e: Term) extends Terminal(e, !e.isKnownToBeAligned) {

  private val alignment = e.alignmentValueInBits

  lazy val parser: Parser = new AlignmentFillParser(alignment, e.runtimeData)
  lazy val unparser: Unparser = new AlignmentFillUnparser(alignment, e.runtimeData, e.fillByteEv)
}

case class MandatoryTextAlignment(e: Term, alignmentInBits: Int, forDelimiter: Boolean) extends Terminal(e,
  if (forDelimiter)
    !e.isDelimiterKnownToBeTextAligned
  else
    !e.isKnownToBeTextAligned) {
  Assert.invariant(alignmentInBits > 0)

  lazy val parser: Parser = new MandatoryTextAlignmentParser(alignmentInBits, e.runtimeData)
  lazy val unparser: Unparser = new MandatoryTextAlignmentUnparser(alignmentInBits, e.runtimeData, e.fillByteEv)
}
