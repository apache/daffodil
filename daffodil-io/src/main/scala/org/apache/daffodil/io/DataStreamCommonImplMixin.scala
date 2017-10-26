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

package edu.illinois.ncsa.daffodil.io

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BinaryFloatRep
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.BitOrder
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.UTF16Width
import java.nio.charset.StandardCharsets
import java.nio.charset.Charset
import edu.illinois.ncsa.daffodil.util.MaybeInt
import edu.illinois.ncsa.daffodil.util.MaybeChar
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.api.DataStreamLimits

trait DataStreamCommonState {

  var binaryFloatRep: BinaryFloatRep = BinaryFloatRep.Ieee
  var bitOrder: BitOrder = BitOrder.MostSignificantBitFirst
  var maybeCharWidthInBits: MaybeInt = MaybeInt.Nope
  var encodingMandatoryAlignmentInBits: Int = 8
  var maybeUTF16Width: Maybe[UTF16Width] = Maybe(UTF16Width.Fixed)
  var debugging: Boolean = false
  var limits_ : DataStreamLimits = BBSLimits
  //
  // These are for dealing with 4-byte UTF-8 codepoints
  // that require 2 16-bit charaters.
  //
  // This only comes up in an incredibly obscure case
  // when fillCharBuffer is called with a char buffer having
  // room for only a single 16-bit codepoint, and the
  // data's first byte is 0xF0, which indicates 4-bytes
  // need to be consumed, to create two 16 bit code units
  // aka a surrogate-pair.
  //
  var maybeTrailingSurrogateForUTF8: MaybeChar = MaybeChar.Nope
  var priorEncoding: Charset = StandardCharsets.UTF_8
  var priorBitPos: Long = 0L

  def resetUTF8SurrogatePairCapture {
    this.maybeTrailingSurrogateForUTF8 = MaybeChar.Nope
    this.priorBitPos = -1
  }

  def assignFrom(other: DataStreamCommonState): Unit = {
    this.binaryFloatRep = other.binaryFloatRep
    this.bitOrder = other.bitOrder
    this.maybeCharWidthInBits = other.maybeCharWidthInBits
    this.encodingMandatoryAlignmentInBits = other.encodingMandatoryAlignmentInBits
    this.maybeUTF16Width = other.maybeUTF16Width
    this.debugging = other.debugging
    this.limits_ = other.limits_
    this.maybeTrailingSurrogateForUTF8 = other.maybeTrailingSurrogateForUTF8
    this.priorEncoding = other.priorEncoding
    this.priorBitPos = other.priorBitPos
  }

}

/**
 * Shared by both DataInputStream and DataOutputStream implementations
 */
trait DataStreamCommonImplMixin extends DataStreamCommon with Logging {

  protected def cst: DataStreamCommonState

  final override def setBinaryFloatRep(binaryFloatRep: BinaryFloatRep): Unit = {
    Assert.invariant(binaryFloatRep == BinaryFloatRep.Ieee)
    cst.binaryFloatRep = binaryFloatRep
  }

  final override def setBitOrder(bitOrder: BitOrder): Unit = { cst.bitOrder = bitOrder }
  //  final override def setCharWidthInBits(charWidthInBits: Maybe[Int]): Unit = { cst.maybeCharWidthInBits = charWidthInBits }
  //  final override def setEncodingMandatoryAlignment(bitAlignment: Int): Unit = { cst.encodingMandatoryAlignmentInBits = bitAlignment }
  final override def setMaybeUTF16Width(maybeUTF16Width: Maybe[UTF16Width]): Unit = { cst.maybeUTF16Width = maybeUTF16Width }

  final def isFixedWidthEncoding = cst.maybeCharWidthInBits.isDefined

  final override def limits: DataStreamLimits = cst.limits_

  final override def setLimits(newLimits: DataStreamLimits) {
    cst.limits_ = newLimits
  }

  /*
   * Debugger support
   */

  final override def areDebugging = cst.debugging

}
