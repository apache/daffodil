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

package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.util.Numbers
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.Evaluatable
import edu.illinois.ncsa.daffodil.processors.Success
import edu.illinois.ncsa.daffodil.util.OnStack
import java.util.regex.Matcher
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.MaybeULong
import passera.unsigned.ULong
import edu.illinois.ncsa.daffodil.equality._
import java.lang.{ Long => JLong }

sealed abstract class SpecifiedLengthParserBase(eParser: Parser,
                                                erd: ElementRuntimeData)
  extends ParserObject(erd)
  with CaptureParsingValueLength {

  override lazy val childProcessors = Seq(eParser)

  override def charsetEv = Assert.invariantFailed("Specified Length parsers should not capture value length using the charset")

  /**
   * Computes number of bits in length. If an error occurs this should
   * modify the state to reflect a processing error.
   */
  protected def getBitLength(s: PState): MaybeULong

  final def parse(pState: PState): Unit = {

    val maybeNBits = getBitLength(pState)

    if (pState.processorStatus _ne_ Success) return
    val nBits = maybeNBits.get
    val dis = pState.dataInputStream

    val startingBitPos0b = dis.bitPos0b
    val isLimitOk: Boolean = dis.withBitLengthLimit(nBits) {
      eParser.parse1(pState)
    }
    if (!isLimitOk) {
      val availBits = if (dis.remainingBits.isDefined) dis.remainingBits.get.toString else "(unknown)"
      PE(pState, "Insufficient bits available. Required %s bits, but only %s were available.", nBits, availBits)
      return
    }
    // at this point the recursive parse of the children is finished
    // so if we're still successful we need to advance the position
    // to skip past any bits that the recursive child parse did not
    // consume at the end. That is, the specified length can be an
    // outer constraint, but the children may not use it all up, leaving
    // a section at the end.
    if (pState.processorStatus ne Success) return
    val finalEndPos0b = startingBitPos0b + nBits

    captureValueLength(pState, ULong(startingBitPos0b), ULong(dis.bitPos0b))

    Assert.invariant(dis eq pState.dataInputStream)
    val bitsToSkip = finalEndPos0b - dis.bitPos0b
    Assert.invariant(bitsToSkip >= 0) // if this is < 0, then the parsing of children went past the limit, which it isn't supposed to.
    if (bitsToSkip > 0) {
      // skip left over bits
      dis.skip(bitsToSkip)
    }
  }

}

class SpecifiedLengthPatternParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  pattern: java.util.regex.Pattern)
  extends SpecifiedLengthParserBase(eParser, erd) {

  object withMatcher extends OnStack[Matcher](pattern.matcher(""))

  final override protected def getBitLength(s: PState): MaybeULong = {
    val dis = s.dataInputStream
    val mark = dis.markPos
    withMatcher { m =>
      dis.lookingAt(m)

      // That matched or it didn't. We don't care. We care that
      // the lookingAt call advanced the bitPos to after the match
      // which means not at all if there was no match.
      val endBitLimit = dis.bitPos0b

      dis.resetPos(mark)
      val length = endBitLimit - dis.bitPos0b
      MaybeULong(length)
    }
  }
}

class SpecifiedLengthExplicitParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  lengthEv: Evaluatable[JLong],
  toBits: Int)
  extends SpecifiedLengthParserBase(eParser, erd) {

  final override def getBitLength(s: PState): MaybeULong = {
    val nBytesAsAny = lengthEv.evaluate(s)
    val nBytes = Numbers.asLong(nBytesAsAny)
    MaybeULong(nBytes * toBits)
  }
}

class SpecifiedLengthImplicitParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  nBits: Long)
  extends SpecifiedLengthParserBase(eParser, erd) {

  final override def getBitLength(s: PState): MaybeULong = MaybeULong(nBits)
}

/**
 * This is used when length is measured in characters, and couldn't be
 * converted to a computation on length in bits because a character is encoded as a variable number
 * of bytes, e.g., in utf-8 encoding where a character can be 1 to 4 bytes.
 *
 * Alternatively, this is also used if the encoding is coming from an expression, so we don't
 * know if it will come back as utf-8 (variable width) or ascii (fixed width)
 *
 * This base is used for complex types where we need to know how long the "box"
 * is, that all the complex content must fit within, where that box length is
 * measured in characters. In the complex content case we do not need the string that is all the
 * characters, as we're going to recursively descend and parse it into the complex structure.
 */
sealed abstract class SpecifiedLengthCharactersParserBase(
  eParser: Parser,
  erd: ElementRuntimeData)
  extends SpecifiedLengthParserBase(eParser, erd) {

  private def maybeBitPosAfterNChars(start: PState, nChars: Long): MaybeULong = {
    val dis = start.dataInputStream
    val mark = dis.markPos
    val hasNChars = dis.skipChars(nChars) // will decode up to n characters.
    if (!hasNChars) {
      dis.resetPos(mark)
      MaybeULong.Nope
    } else {
      val bitLimitAfterNChars = dis.bitPos0b
      dis.resetPos(mark)
      MaybeULong(bitLimitAfterNChars)
    }
  }

  protected def getCharLength(s: PState): Long

  final protected override def getBitLength(s: PState): MaybeULong = {
    val nChars = getCharLength(s)
    //
    // TODO: Performance - if the encoding is an expression, but that
    // expression computes a fixed-width encoding, then we can compute
    // nbits more cheaply by just multiplying.
    //
    // We only need this more general code for the real case where
    // the encoding is variable width.
    //
    val mBitLimit = maybeBitPosAfterNChars(s, nChars)
    if (!mBitLimit.isDefined) {
      PE(s, "%s - %s - Parse failed.  Failed to find exactly %s characters.",
        this.toString(), erd.name, nChars)
      MaybeULong.Nope
    } else {
      val bitLength = mBitLimit.get - s.bitPos0b
      MaybeULong(bitLength)
    }
  }
}

final class SpecifiedLengthImplicitCharactersParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  nChars: Long)
  extends SpecifiedLengthCharactersParserBase(eParser, erd) {

  override def getCharLength(s: PState) = nChars

}

final class SpecifiedLengthExplicitCharactersParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  lengthEv: Evaluatable[JLong])
  extends SpecifiedLengthCharactersParserBase(eParser, erd) {

  def getCharLength(s: PState): Long = {
    val nChars = lengthEv.evaluate(s)
    nChars
  }

}

class CaptureStartOfContentLengthParser(override val context: ElementRuntimeData)
  extends PrimParser {

  override val runtimeDependencies = Nil

  override def parse(state: PState) {
    val dis = state.dataInputStream
    val elem = state.infoset
    elem.contentLength.setAbsStartPos0bInBits(ULong(dis.bitPos0b))
  }
}

class CaptureEndOfContentLengthParser(override val context: ElementRuntimeData)
  extends PrimParser {

  override val runtimeDependencies = Nil

  override def parse(state: PState) {
    val dis = state.dataInputStream
    val elem = state.infoset
    elem.contentLength.setAbsEndPos0bInBits(ULong(dis.bitPos0b))
  }
}

class CaptureStartOfValueLengthParser(override val context: ElementRuntimeData)
  extends PrimParser {

  override val runtimeDependencies = Nil

  override def parse(state: PState) {
    val dis = state.dataInputStream
    val elem = state.infoset
    elem.valueLength.setAbsStartPos0bInBits(ULong(dis.bitPos0b))
  }
}

class CaptureEndOfValueLengthParser(override val context: ElementRuntimeData)
  extends PrimParser {

  override val runtimeDependencies = Nil

  override def parse(state: PState) {
    val dis = state.dataInputStream
    val elem = state.infoset
    elem.valueLength.setAbsEndPos0bInBits(ULong(dis.bitPos0b))
  }
}
