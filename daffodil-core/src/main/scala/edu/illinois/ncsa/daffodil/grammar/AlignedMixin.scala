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

import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.dsom.Term
import edu.illinois.ncsa.daffodil.exceptions.Assert

trait AlignedMixin extends GrammarMixin { self: Term =>

  lazy val leadingSkipRegion = prod("leadingSkipRegion", leadingSkip > 0) { LeadingSkipRegion(this) }
  lazy val trailingSkipRegion = prod("trailingSkipRegion", trailingSkip > 0) { TrailingSkipRegion(this) }
  lazy val alignmentFill = prod("alignmentFill", !isKnownPreAligned) { AlignmentFill(this) }

  /**
   * true if we can statically determine that the start of this
   * will be properly aligned by where the prior thing left us positioned.
   * Hence we are guaranteed to be properly aligned.
   */
  // TODO: make this actually do the position analysis - that however, requires computing
  // known alignment information based on the starting known alignment and known length
  // of prior things (recursively). I.e., it's a bit tricky.
  private lazy val isKnownPreAligned = self.isScannable || (alignment == 1 && alignmentUnits == AlignmentUnits.Bits)

  // TODO: deal with case of a bit field that is not a multiple of bytes wide
  // but has a terminator which is text and so has mandatory alignment.
  //  /**
  //   * Region of up to 7 bits to get us to a byte boundary for text.
  //   */
  //  lazy val initiatorAlign = Prod("initiatorAlign", this, !isInitiatorPreAligned, TextAlign(mandatoryAlignment))
  //  lazy val terminatorAlign = Prod("terminatorAlign", this, !isTerminatorPreAligned, TextAlign(mandatoryAlignment))
  //  lazy val separatorAlign = Prod("separatorAlign", this, !isSeparatorPreAligned, TextAlign(mandatoryAlignment))
  //  
  //  lazy val isInitiatorPreAligned = {
  //    if (!hasInitiator) true
  //    else {
  //      alignmentCompatible(precedingTermAlignment, mandatoryAlignment)
  //    }
  //  }

  final lazy val hasNoSkipRegions = leadingSkip == 0 && trailingSkip == 0

}

