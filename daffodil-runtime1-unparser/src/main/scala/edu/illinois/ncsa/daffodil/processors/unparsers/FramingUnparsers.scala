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

package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.FillByteEv
import edu.illinois.ncsa.daffodil.processors.SuspendableOperation

class SkipRegionUnparser(
  skipInBits: Int,
  e: RuntimeData,
  fillByteEv: FillByteEv)
    extends PrimUnparserObject(e) {

  override def runtimeDependencies = List(fillByteEv)

  override def unparse(state: UState) = {
    val dos = state.dataOutputStream
    if (!dos.skip(skipInBits)) UE(state, "Unable to skip %s(bits).", skipInBits)
  }
}

class AlignmentFillUnparser(
  alignmentInBits: Int,
  override val rd: RuntimeData,
  fillByteEv: FillByteEv)
    extends PrimUnparserObject(rd)
    with SuspendableOperation {

  override def runtimeDependencies = List(fillByteEv)

  override def test(ustate: UState) = {
    val dos = ustate.dataOutputStream
    if (dos.maybeAbsBitPos0b.isEmpty) {
      System.err.println(this.toString + " Unable to align to " + alignmentInBits + " bits becasue there is no absolute bit position.")
    }
    dos.maybeAbsBitPos0b.isDefined
  }

  override def continuation(state: UState) {
    val dos = state.dataOutputStream
    val fb = fillByteEv.evaluate(state)
    dos.setFillByte(fb)
    if (!dos.align(alignmentInBits))
      UE(state, "Unable to align to %s(bits).", alignmentInBits)
  }

  override def unparse(state: UState): Unit = {
    run(state)
  }
}

class MandatoryTextAlignmentUnparser(
  alignmentInBits: Int,
  e: RuntimeData,
  fillByteEv: FillByteEv)
    extends AlignmentFillUnparser(alignmentInBits, e, fillByteEv)
