package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.dpath.AsIntConverters
import edu.illinois.ncsa.daffodil.processors.{ ParserObject, Parser }
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.WithParseErrorThrowing
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.Evaluatable
import edu.illinois.ncsa.daffodil.processors.Success
import edu.illinois.ncsa.daffodil.util.OnStack
import java.util.regex.Matcher
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.equality._
import java.lang.{ Long => JLong }

sealed abstract class SpecifiedLengthParserBase(eParser: Parser,
  erd: ElementRuntimeData)
    extends ParserObject(erd)
    with WithParseErrorThrowing {

  override lazy val childProcessors = Seq(eParser)

  /**
   * Computes number of bits in length. If an error occurs this should
   * modify the state to reflect a processing error.
   */
  protected def getBitLength(s: PState): Long

  final def parse(pState: PState): Unit = withParseErrorThrowing(pState) {

    val nBits = getBitLength(pState)
    if (pState.status _ne_ Success) return
    val dis = pState.dataInputStream

    val startingBitPos0b = dis.bitPos0b
    val isLimitOk = dis.withBitLengthLimit(nBits) {
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
    if (pState.status ne Success) return
    val finalEndPos0b = startingBitPos0b + nBits
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

  final override protected def getBitLength(s: PState): Long = {
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
      length
    }
  }
}

class SpecifiedLengthExplicitParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  lengthEv: Evaluatable[JLong],
  toBits: Int)
    extends SpecifiedLengthParserBase(eParser, erd) {

  final override def getBitLength(s: PState): Long = {
    val nBytesAsAny = lengthEv.evaluate(s)
    val nBytes = AsIntConverters.asLong(nBytesAsAny)
    nBytes * toBits
  }
}

class SpecifiedLengthImplicitParser(
  eParser: Parser,
  erd: ElementRuntimeData,
  nBits: Long)
    extends SpecifiedLengthParserBase(eParser, erd) {

  final override def getBitLength(s: PState): Long = nBits
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

  final protected override def getBitLength(s: PState): Long = {
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
    if (!mBitLimit.isDefined)
      PE(s.schemaFileLocation, "%s - %s - Parse failed.  Failed to find exactly %s characters.",
        this.toString(), erd.name, nChars)
    else {
      val bitLength = mBitLimit.get - s.bitPos0b
      bitLength
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
