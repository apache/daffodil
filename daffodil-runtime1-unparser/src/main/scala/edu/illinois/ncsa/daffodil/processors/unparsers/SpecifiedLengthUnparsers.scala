package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.charset.DFDLCharset
import edu.illinois.ncsa.daffodil.dpath.AsIntConverters
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.WithParseErrorThrowing
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
import java.io.ByteArrayOutputStream
import edu.illinois.ncsa.daffodil.io.BasicDataOutputStream
import java.nio.charset.CharsetEncoder
import edu.illinois.ncsa.daffodil.io.CharBufferDataOutputStream
import edu.illinois.ncsa.daffodil.io.DataStreamCommon

abstract class SpecifiedLengthUnparserBase(eUnparser: Unparser,
  erd: ElementRuntimeData)
  extends Unparser(erd) with TextUnparserRuntimeMixin
  with WithParseErrorThrowing {

  override lazy val childProcessors = Seq(eUnparser)

  /**
   * Computes number of bits in length. If an error occurs this should
   * modify the state to reflect a processing error.
   */
  protected def getBitLength(s: UState): Long

  final def unparse(state: UState): Unit = {

    setupEncoding(state, erd)

    val nBits = getBitLength(state)
    if (state.status ne Success) return
    var dos = state.dataOutputStream
    val startingBitPos0b = dos.bitPos0b
    val isLimitOk = dos.withBitLengthLimit(nBits) {
      eUnparser.unparse1(state, erd)
    }
    if (!isLimitOk) {
      val availBits = dos.remainingBits.map { _.toString }.getOrElse("(unknown)")
      UE(state, "Insufficient bits available. Required %s bits, but only %s were available.", nBits, availBits)
    }
    // at this point the recursive parse of the children is finished
    // so if we're still successful we need to advance the position
    // to skip past any bits that the recursive child parse did not 
    // consume at the end. That is, the specified length can be an 
    // outer constraint, but the children may not use it all up, leaving
    // a section at the end.
    if (state.status != Success) return
    val finalEndPos0b = startingBitPos0b + nBits
    val bitsToSkip = finalEndPos0b - dos.bitPos0b
    Assert.invariant(bitsToSkip >= 0)
    if (bitsToSkip > 0) {
      // skip left over bits
      Assert.invariant(dos.skip(bitsToSkip))
    }
  }

}

class SpecifiedLengthExplicitBitsUnparser(
  eUnparser: Unparser,
  erd: ElementRuntimeData,
  length: CompiledExpression)
  extends SpecifiedLengthUnparserBase(eUnparser, erd) {

  final override def getBitLength(s: UState): Long = {
    val nBitsAsAny = length.evaluate(s)
    val nBits = AsIntConverters.asLong(nBitsAsAny)
    nBits
  }
}

class SpecifiedLengthExplicitBitsFixedUnparser(
  eUnparser: Unparser,
  erd: ElementRuntimeData,
  nBits: Long)
  extends SpecifiedLengthUnparserBase(eUnparser, erd) {

  final override def getBitLength(s: UState): Long = nBits
}

class SpecifiedLengthExplicitBytesUnparser(
  eUnparser: Unparser,
  erd: ElementRuntimeData,
  length: CompiledExpression)
  extends SpecifiedLengthUnparserBase(eUnparser, erd) {

  final override def getBitLength(s: UState): Long = {
    val nBytesAsAny = length.evaluate(s)
    val nBytes = AsIntConverters.asLong(nBytesAsAny)
    nBytes * 8
  }
}

class SpecifiedLengthExplicitBytesFixedUnparser(
  eUnparser: Unparser,
  erd: ElementRuntimeData,
  nBytes: Long)
  extends SpecifiedLengthUnparserBase(eUnparser, erd) {

  final override def getBitLength(s: UState): Long = nBytes * 8
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
 * This is a very uncommon situation. It seems it is really there in DFDL just to provide some
 * orthogonality of the lengthKind property to the type of the element.
 *
 * A possible use case where this would be needed is data which used to be fixed length
 * (such as 80 bytes), but which has been updated to use utf-8, instead of the original
 * single-byte character set. Such data might now specify that there are 80 characters still,
 * allowing for 80 unicode characters. (Alternatively such data format might specify 80
 * bytes still, meaning up to 80 unicode characters, but possibly fewer.)
 */
abstract class SpecifiedLengthExplicitCharactersUnparserBase(
  eUnparser: Unparser,
  erd: ElementRuntimeData)
  extends Unparser(erd) with TextUnparserRuntimeMixin {

  final override protected def childProcessors = Seq(eUnparser)

  protected def getCharLength(s: UState): Long

  object charBufferDataOutputStream extends OnStack[CharBufferDataOutputStream](new CharBufferDataOutputStream)

  override final def unparse(state: UState) {
    import DataStreamCommon._

    setupEncoding(state, erd)

    val nChars = getCharLength(state)

    charBufferDataOutputStream { cbdos =>
      withLocalCharBuffer { lcb =>
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
          val charset = state.dataOutputStream.encoder.charset
          val charMinWidthInBits = encInfo.encodingMinimumCodePointWidthInBits(charset)
          val nSkipBits = charsUnused * charMinWidthInBits
          if (!state.dataOutputStream.skip(nSkipBits)) UE(state, "Insufficient space to write %s characters.", nChars)
        }
      }
    }
  }
}

final class SpecifiedLengthExplicitCharactersFixedUnparser(
  eUnparser: Unparser,
  erd: ElementRuntimeData,
  nChars: Long)
  extends SpecifiedLengthExplicitCharactersUnparserBase(eUnparser, erd) {

  override def getCharLength(s: UState) = nChars

}

final class SpecifiedLengthExplicitCharactersUnparser(
  eUnparser: Unparser,
  erd: ElementRuntimeData,
  length: CompiledExpression)
  extends SpecifiedLengthExplicitCharactersUnparserBase(eUnparser, erd) {

  override def getCharLength(s: UState): Long = {
    val nCharsAsAny = length.evaluate(s)
    val nChars = AsIntConverters.asLong(nCharsAsAny)
    nChars
  }

}
