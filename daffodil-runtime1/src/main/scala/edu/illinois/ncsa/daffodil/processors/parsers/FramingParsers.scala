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

package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.AlignmentUnits
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.RuntimeData

class LeadingSkipRegionParser(
  alignment: Int,
  leadingSkip: Int,
  e: RuntimeData)
  extends PrimParser(e) {
  def parse(pstate: PState) = {
    // Is there a reason why we can't do alignment * leadingSkip before this step?
    // Doesn't follow PEMDAS?
    val newBitPos = alignment * leadingSkip + pstate.bitPos
    pstate.withPos(newBitPos, -1, Nope)
  }
}

class TrailingSkipRegionParser(
  alignment: Int,
  trailingSkip: Int,
  e: RuntimeData)
  extends PrimParser(e) {
  def parse(pstate: PState) = {
    val newBitPos = alignment * trailingSkip + pstate.bitPos
    pstate.withPos(newBitPos, -1, Nope)
  }
}

class AlignmentFillParser(
  alignment: Any,
  alignmentInBits: Int,
  e: RuntimeData)
  extends PrimParser(e) {

  def isAligned(currBitPos: Long): Boolean = {
    if (alignmentInBits == 0 || currBitPos == 0) return true
    if ((currBitPos - alignmentInBits) < 0) return false
    if ((currBitPos % alignmentInBits) == 0) return true
    return false
  }

  def parse(pstate: PState) = {
    if (!isAligned(pstate.bitPos)) {
      val maxBitPos = pstate.bitPos + alignmentInBits - 1
      val newBitPos = maxBitPos - maxBitPos % alignmentInBits
      pstate.withPos(newBitPos, -1, Nope)
    } else
      pstate
  }
}