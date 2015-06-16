package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.dpath.AsIntConverters
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.WithParseErrorThrowing
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.Success
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import java.util.regex.Pattern
import edu.illinois.ncsa.daffodil.util.OnStack
import java.util.regex.Matcher
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.nio.charset.CharsetDecoder

abstract class SpecifiedLengthParserBase(eParser: DaffodilParser,
  erd: ElementRuntimeData)
  extends DaffodilParser(erd)
  with WithParseErrorThrowing {

  override lazy val childProcessors = Seq(eParser)

  /**
   * Computes number of bits in length. If an error occurs this should
   * modify the state to reflect a processing error.
   */
  protected def getBitLength(s: PState): Long

  final def parse(pState: PState): Unit = withParseErrorThrowing(pState) {

    val nBits = getBitLength(pState)
    if (pState.status ne Success) return
    var dis = pState.dataInputStream

    val startingBitPos0b = dis.bitPos0b
    val isLimitOk = dis.withBitLengthLimit(nBits) {
      eParser.parse1(pState)
    }
    if (!isLimitOk) {
      val availBits = dis.remainingBits.map { _.toString }.getOrElse("(unknown)")
      PE(pState, "Insufficient bits available. Required %s bits, but only %s were available.", nBits, availBits)
      return
    }
    // at this point the recursive parse of the children is finished
    // so if we're still successful we need to advance the position
    // to skip past any bits that the recursive child parse did not 
    // consume at the end. That is, the specified length can be an 
    // outer constraint, but the children may not use it all up, leaving
    // a section at the end.
    if (pState.status != Success) return
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
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  pattern: java.util.regex.Pattern)
  extends SpecifiedLengthParserBase(eParser, erd) {

  object withMatcher extends OnStack[Matcher](pattern.matcher(""))

  final override protected def getBitLength(s: PState): Long = {
    val dis = s.dataInputStream
    val mark = dis.mark
    withMatcher { m =>
      val isMatch = dis.lookingAt(m)

      // That matched or it didn't. We don't care. We care that
      // the lookingAt call advanced the bitPos to after the match
      // which means not at all if there was no match.
      val endBitLimit = dis.bitPos0b

      dis.reset(mark)
      val length = endBitLimit - dis.bitPos0b
      length
    }
  }
}

class SpecifiedLengthExplicitBitsParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  length: CompiledExpression,
  toBits: Int)
  extends SpecifiedLengthParserBase(eParser, erd) {

  final override def getBitLength(s: PState): Long = {
    val nBytesAsAny = length.evaluate(s)
    val nBytes = AsIntConverters.asLong(nBytesAsAny)
    nBytes * toBits
  }
}

class SpecifiedLengthExplicitBitsFixedParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  nBits: Long)
  extends SpecifiedLengthParserBase(eParser, erd) {

  final override def getBitLength(s: PState): Long = nBits
}

class SpecifiedLengthExplicitBytesParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  length: CompiledExpression)
  extends SpecifiedLengthParserBase(eParser, erd) {

  final override def getBitLength(s: PState): Long = {
    val nBytesAsAny = length.evaluate(s)
    val nBytes = AsIntConverters.asLong(nBytesAsAny)
    nBytes * 8
  }
}

class SpecifiedLengthExplicitBytesFixedParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  nBytes: Long)
  extends SpecifiedLengthParserBase(eParser, erd) {

  final override def getBitLength(s: PState): Long = nBytes * 8
}

/**
 * This is used when length is measured in characters, and couldn't be
 * converted to a computation on length in bytes because a character is encoded as a variable number
 * of bytes, e.g., in utf-8 encoding where a character can be 1 to 4 bytes.
 *
 * This base is used for complex types where we need to know how long the "box"
 * is, that all the complex content must fit within, where that box length is
 * measured in characters. In the complex content case we do not need the string that is all the
 * characters, as we're going to recursively descend and parse it into the complex structure.
 *
 * TODO: Idea - this base also ends up being used for nilLiterals (as of this
 * comment being written 2015-06-30), and there, these two passes, one to
 * measure, and then one to parse, really are redundant. Could change the way nilLiterals are
 * parsed to not use this base, and that could boost performance (maybe...) for nilLiteral-intensive formats.
 */
abstract class SpecifiedLengthExplicitCharactersParserBase(
  eParser: DaffodilParser,
  erd: ElementRuntimeData)
  extends SpecifiedLengthParserBase(eParser, erd) {

  private def maybeBitPosAfterNChars(start: PState, nChars: Long): Maybe[Long] = {
    val dis = start.dataInputStream
    val mark = dis.mark
    val hasNChars = dis.skipChars(nChars)
    if (!hasNChars) {
      dis.reset(mark)
      Nope
    } else {
      val bitLimitAfterNChars = dis.bitPos0b
      dis.reset(mark)
      Maybe(bitLimitAfterNChars)
    }
  }

  protected def getLength(s: PState): Long

  final protected override def getBitLength(s: PState): Long = {
    val nChars = getLength(s)
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

final class SpecifiedLengthExplicitCharactersFixedParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  nChars: Long)
  extends SpecifiedLengthExplicitCharactersParserBase(eParser, erd) {

  override def getLength(s: PState) = nChars

}

final class SpecifiedLengthExplicitCharactersParser(
  eParser: DaffodilParser,
  erd: ElementRuntimeData,
  length: CompiledExpression)
  extends SpecifiedLengthExplicitCharactersParserBase(eParser, erd) {

  def getLength(s: PState): Long = {
    val nBytesAsAny = length.evaluate(s)
    val nBytes = AsIntConverters.asLong(nBytesAsAny)
    nBytes
  }

}
