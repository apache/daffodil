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

import edu.illinois.ncsa.daffodil.dpath.AsIntConverters
//import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.Success
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.processors.InfosetNoDataException
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.processors.InfosetNoSuchChildElementException
import edu.illinois.ncsa.daffodil.processors.LengthEv

/**
 * Restricts the bits available for unparsing to just those within
 * the specified length computed.
 *
 * If a unparser (supplied as arg) runs past the available space,
 * that's an unparse error.
 *
 * Truncation of strings - the only case where we truncate, and only when
 * dfdl:truncateSpecifiedLengthString is 'yes', is handled elsewhere.
 */
sealed abstract class SpecifiedLengthUnparserBase(eUnparser: Unparser,
  erd: ElementRuntimeData)
  extends UnparserObject(erd) with TextUnparserRuntimeMixin {

  override lazy val childProcessors = Seq(eUnparser)

  /**
   * Computes number of bits in length. If an error occurs this should
   * modify the state to reflect a processing error.
   */
  protected def getBitLength(s: UState): Long

  override final def unparse(state: UState): Unit = {

    val maybeNBits =
      try {
        MaybeULong(getBitLength(state))
      } catch {
        case noData: InfosetNoDataException => MaybeULong.Nope
        case noChild: InfosetNoSuchChildElementException => MaybeULong.Nope
      }

    if (maybeNBits.isDefined) {
      val nBits = maybeNBits.get
      if (state.status _ne_ Success) return
      val dos = state.dataOutputStream
      //val startingBitPos0b = dos.relBitPos0b
      val isLimitOk = dos.withBitLengthLimit(nBits) {
        eUnparser.unparse1(state, erd)
      }
      if (!isLimitOk) {
        val availBits = if (dos.remainingBits.isDefined) dos.remainingBits.get.toString else "(unknown)"
        UE(state, "Insufficient bits available. Required %s bits, but only %s were available.", nBits, availBits)
      }
      // at this point the recursive parse of the children is finished

      if (state.status ne Success) return

      // We might not have used up all the bits. So some bits may need to
      // be skipped and filled in by fillbyte.
      //
      // In the DFDL data grammar the region being skipped is either the
      // RightFill region, or the ElementUnused region. Skipping this is handled
      // elsewhere, along with insertion of padding before/after a string.
      //

    } else {
      // we couldn't get the explicit length
      //
      // This happens when we're unparsing, and this element depends on a prior element for
      // determining its length via a length expression, but that prior element
      // has dfdl:outputValueCalc that depends on this element, typically by a
      // call to dfdl:valueLength of this, or of some structure that includes
      // this.
      //
      // This breaks the chicken-egg cycle, by just unparsing it without
      // constraint. That produces the value which (ignoring truncation)
      // can be unparsed to produce the dfdl:valueLength of this element.
      //
      // This does assume that the value will get truncated properly for the
      // case where we do truncation (type string, with
      // dfdl:truncateSpecifiedLengthString 'yes') by some other mechanism.
      //
      eUnparser.unparse1(state, erd)
    }
  }
}

final class SpecifiedLengthExplicitUnparser(
  eUnparser: Unparser,
  erd: ElementRuntimeData,
  lengthEv: LengthEv,
  bitsMultiplier: Int)
  extends SpecifiedLengthUnparserBase(eUnparser, erd) {

  override def getBitLength(s: UState): Long = {
    val nBytesAsAny = lengthEv.evaluate(s)
    val nBytes = AsIntConverters.asLong(nBytesAsAny)
    nBytes * bitsMultiplier
  }

}

final class SpecifiedLengthImplicitUnparser(
  eUnparser: Unparser,
  erd: ElementRuntimeData,
  nBits: Long)
  extends SpecifiedLengthUnparserBase(eUnparser, erd) {

  override def getBitLength(s: UState): Long = nBits

}

/**
 * This is used when length is measured in characters, and couldn't be
 * converted to a computation on length in bytes because a character is encoded as a variable number
 * of bytes, e.g., in utf-8 encoding where a character can be 1 to 4 bytes.
 *
 * In addition this is used when the encoding is an expression, so we don't know
 * a priori whether the encoding will be fixed or variable width.
 *
 * This base is used for complex types where we need to know how long the "box"
 * is, that all the complex content must fit within, where that box length is
 * measured in characters. In the complex content case we do not need the string that is all the
 * characters, as we're going to recursively descend and parse it into the complex structure.
 *
 * This is a very uncommon situation. It seems it is really there in DFDL just to provide some
 * orthogonality of the lengthKind property to the type of the element.
 *
 * A possible use case where this would be needed is data which used to be fixed length
 * (such as 80 bytes), but which has been updated to use utf-8, instead of the original
 * single-byte character set. Such data might now specify that there are 80 characters still,
 * allowing for 80 unicode characters. (Alternatively such data format might specify 80
 * bytes still, meaning up to 80 unicode characters, but possibly fewer.)
 */
abstract class SpecifiedLengthCharactersUnparserBase(
  eUnparser: Unparser,
  erd: ElementRuntimeData)
  extends UnparserObject(erd) with TextUnparserRuntimeMixin {

  final override def childProcessors = Seq(eUnparser)

  protected def getCharLength(s: UState): Long

  override final def unparse(state: UState) {

    val maybeNChars =
      try {
        MaybeULong(getCharLength(state))
      } catch {
        case noData: InfosetNoDataException => MaybeULong.Nope
      }
    if (maybeNChars.isDefined) {
      val nChars = maybeNChars.get
      //
      // because we don't know how many bytes a character requires,
      // we can't depend on the regular maybeBitLimit0b to constrain
      // how long the unparsed text can get. So we make a finite
      // char buffer, and a charBufferOutputStream (fails on binary stuff)
      // and we unparse to that.
      //
      // This will either leave some chars unused, or fill them all
      //
      state.charBufferDataOutputStream { cbdos =>
        state.withLocalCharBuffer { lcb =>
          val cb = lcb.getBuf(nChars)
          cbdos.setCharBuffer(cb)
          state.withTemporaryDataOutputStream(cbdos) {
            eUnparser.unparse1(state, erd)
          }
          val charsUnused = cb.remaining()
          //
          // at this point, the char buffer has been filled in
          // with at most nChars of data.
          //
          cb.flip
          val nCharsWritten = state.dataOutputStream.putCharBuffer(cb)
          //
          // Note: it's possible that nCharsWritten is less than nChars
          // because this entire parser could be surrounded by some
          // context that has a bit limit.
          //
          if (nCharsWritten < nChars) {
            //
            // cb might not be full, because the recursive unparse
            // might not use it all up.
            //
            // In that case, we have to fill out any chars, if there is room
            // for them, with fill bytes, which is what skip does.
            val encInfo = erd.encodingInfo
            val dcharset = encInfo.getDFDLCharset(state)
            val charMinWidthInBits = encInfo.encodingMinimumCodePointWidthInBits(dcharset)
            val nSkipBits = charsUnused * charMinWidthInBits
            if (!state.dataOutputStream.skip(nSkipBits)) UE(state, "Insufficient space to write %s characters.", nChars)
          }
        }
      }
    } else {
      // we couldn't get the explicit length
      // ignore constraining the output length. Just unparse it.
      //
      // This happens when we're unparsing, and this element depends on a prior element for
      // determining its length, but that prior element has dfdl:outputValueCalc that depends
      // on this element.
      // This breaks the chicken-egg cycle.
      //
      eUnparser.unparse1(state, erd)
    }
  }
}

final class SpecifiedLengthExplicitCharactersUnparser(
  eUnparser: Unparser,
  erd: ElementRuntimeData,
  lengthEv: LengthEv)
  extends SpecifiedLengthCharactersUnparserBase(eUnparser, erd) {

  override def getCharLength(s: UState): Long = {
    val nCharsAsAny = lengthEv.evaluate(s)
    val nChars = AsIntConverters.asLong(nCharsAsAny)
    nChars
  }
}

final class SpecifiedLengthImplicitCharactersUnparser(
  eUnparser: Unparser,
  erd: ElementRuntimeData,
  nChars: Long)
  extends SpecifiedLengthCharactersUnparserBase(eUnparser, erd) {

  override def getCharLength(s: UState): Long = nChars
}

