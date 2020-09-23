/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.processors.parsers

import java.lang.{ Long => JLong }
import java.util.regex.Matcher

import passera.unsigned.ULong

import org.apache.daffodil.equality._
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.processors.ElementRuntimeData
import org.apache.daffodil.processors.ChoiceRuntimeData
import org.apache.daffodil.processors.RuntimeData
import org.apache.daffodil.processors.Evaluatable
import org.apache.daffodil.processors.Success
import org.apache.daffodil.schema.annotation.props.gen.LengthUnits
import org.apache.daffodil.util.MaybeULong
import org.apache.daffodil.util.Numbers
import org.apache.daffodil.util.OnStack

sealed abstract class SpecifiedLengthParserBase(
  eParser: Parser,
  erd: RuntimeData)
  extends CombinatorParser(erd)
  with CaptureParsingValueLength {

  override lazy val runtimeDependencies = Vector()

  override lazy val childProcessors = Vector(eParser)

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

    val shouldCheckDefinedForLength = erd match {
      case erd : ElementRuntimeData => !erd.isComplexType
      case _ : ChoiceRuntimeData => false
      case _ => true
    }

    if (shouldCheckDefinedForLength && !dis.isDefinedForLength(nBits)) {
      PENotEnoughBits(pState, nBits, dis.remainingBits)
      return
    }

    val startingBitPos0b = dis.bitPos0b
    dis.withBitLengthLimit(nBits) {
      eParser.parse1(pState)
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
      val skipSuccess = dis.skip(bitsToSkip, pState)
      if (!skipSuccess) {
        PENotEnoughBits(pState, bitsToSkip, dis.remainingBits)
      }
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
      dis.lookingAt(m, s)

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

class SpecifiedLengthChoiceParser(
  eParser: Parser,
  erd: RuntimeData,
  choiceLengthInBits: JLong)
  extends SpecifiedLengthParserBase(eParser, erd) {

  final override def getBitLength(s: PState): MaybeULong = {
    MaybeULong(choiceLengthInBits)
  }
}

class SpecifiedLengthImplicitParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  nBits: Long)
  extends SpecifiedLengthParserBase(eParser, erd) {

  final override def getBitLength(s: PState): MaybeULong = MaybeULong(nBits)
}

class SpecifiedLengthPrefixedParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  override val prefixedLengthParser: Parser,
  override val prefixedLengthERD: ElementRuntimeData,
  override val lengthUnits: LengthUnits,
  override val prefixedLengthAdjustmentInUnits: Long)
  extends SpecifiedLengthParserBase(eParser, erd)
  with PrefixedLengthParserMixin {

  override lazy val childProcessors = Vector(eParser, prefixedLengthParser)

  final override def getBitLength(s: PState): MaybeULong = {
    MaybeULong(getPrefixedLengthInBits(s))
  }
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
    val hasNChars = dis.skipChars(nChars, start) // will decode up to n characters.
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

final class SpecifiedLengthPrefixedCharactersParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  override val prefixedLengthParser: Parser,
  override val prefixedLengthERD: ElementRuntimeData,
  override val prefixedLengthAdjustmentInUnits: Long)
  extends SpecifiedLengthCharactersParserBase(eParser, erd)
  with PrefixedLengthParserMixin {

  override lazy val childProcessors = Vector(eParser, prefixedLengthParser)
 
  override lazy val lengthUnits = LengthUnits.Characters
   
  def getCharLength(s: PState): Long = {
    val nChars = getPrefixedLengthInUnits(s)
    nChars
  }
}

class CaptureStartOfContentLengthParser(override val context: ElementRuntimeData)
  extends PrimParser {

  override val runtimeDependencies = Vector()

  override def parse(state: PState): Unit = {
    val dis = state.dataInputStream
    val elem = state.infoset
    elem.contentLength.setAbsStartPos0bInBits(ULong(dis.bitPos0b))
  }
}

class CaptureEndOfContentLengthParser(override val context: ElementRuntimeData)
  extends PrimParser {

  override val runtimeDependencies = Vector()

  override def parse(state: PState): Unit = {
    val dis = state.dataInputStream
    val elem = state.infoset
    elem.contentLength.setAbsEndPos0bInBits(ULong(dis.bitPos0b))
  }
}

class CaptureStartOfValueLengthParser(override val context: ElementRuntimeData)
  extends PrimParser {

  override val runtimeDependencies = Vector()

  override def parse(state: PState): Unit = {
    val dis = state.dataInputStream
    val elem = state.infoset
    elem.valueLength.setAbsStartPos0bInBits(ULong(dis.bitPos0b))
  }
}

class CaptureEndOfValueLengthParser(override val context: ElementRuntimeData)
  extends PrimParser {

  override val runtimeDependencies = Vector()

  override def parse(state: PState): Unit = {
    val dis = state.dataInputStream
    val elem = state.infoset
    elem.valueLength.setAbsEndPos0bInBits(ULong(dis.bitPos0b))
  }
}
