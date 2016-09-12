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

package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.TextTruncationType
import edu.illinois.ncsa.daffodil.processors.CharsetEv
import edu.illinois.ncsa.daffodil.processors.UnparseTargetLengthInBitsEv
import edu.illinois.ncsa.daffodil.util.MaybeJULong
import edu.illinois.ncsa.daffodil.processors.LengthEv
import edu.illinois.ncsa.daffodil.processors.Evaluatable

sealed abstract class StringSpecifiedLengthUnparserBase(
  val erd: ElementRuntimeData)
  extends PrimUnparser {

  override def context = erd

  /**
   * override in nil specified length unparsers
   */
  protected def contentString(state: UState) =
    state.currentInfosetNode.asSimple.dataValueAsString

}

class StringNoTruncateUnparser(
  erd: ElementRuntimeData)
  extends StringSpecifiedLengthUnparserBase(erd) {

  override def runtimeDependencies: Seq[Evaluatable[AnyRef]] = Nil

  override def unparse(state: UState) {
    val dos = state.dataOutputStream
    val valueToWrite = contentString(state)
    val nCharsWritten = dos.putString(valueToWrite)
    Assert.invariant(nCharsWritten == valueToWrite.length)
  }

}

sealed abstract class StringSpecifiedLengthUnparserTruncateBase(
  stringTruncationType: TextTruncationType.Type,
  erd: ElementRuntimeData)
  extends StringSpecifiedLengthUnparserBase(erd) {

  Assert.usage(stringTruncationType ne TextTruncationType.None)

  /**
   * We only truncate strings, and only if textStringJustification is left or
   * right, and only if truncateSpecifiedLengthString is yes.
   */
  protected final def truncateByJustification(ustate: UState, str: String, nChars: Long): String = {
    Assert.invariant(erd.optTruncateSpecifiedLengthString.isDefined)
    val nCharsToTrim = str.length - nChars.toInt
    val result = stringTruncationType match {
      case TextTruncationType.Right => {
        str.substring(nCharsToTrim)
      }
      case TextTruncationType.Left => {
        str.substring(0, str.length - nCharsToTrim)
      }
      case _ =>
        Assert.invariantFailed("cannot be TextTruncationType.None")
    }
    result
  }
}
/**
 * Truncates strings to the right length measured in bits.
 * LengthUnits is Bits, but we still don't know whether the encoding
 * is fixed width or variable width.
 */
class StringMaybeTruncateBitsUnparser(
  targetLengthInBitsEv: UnparseTargetLengthInBitsEv,
  stringTruncationType: TextTruncationType.Type,
  erd: ElementRuntimeData,
  charsetEv: CharsetEv)
  extends StringSpecifiedLengthUnparserTruncateBase(
    stringTruncationType,
    erd) {

  override lazy val runtimeDependencies = List(targetLengthInBitsEv, charsetEv)

  private def getLengthInBits(str: String, state: UState): (Long, Long) = {
    val cs = charsetEv.evaluate(state)
    val mfw = cs.maybeFixedWidth
    val sl = str.length.toLong
    val res =
      if (mfw.isDefined) {
        // fixed width encoding so we can get the length in bits by calculation
        //
        // DFDL workgroup discussed whether one must scan the characters here so as
        // to detect encoding errors, and it was decided one does not have
        // to do so for the fixed length case. This encoding error will get
        // found when the string is *actually* encoded later.
        //
        (sl * mfw.get, sl)
      } else {
        //
        // variable width encoding. (most important example is utf-8 encoding)
        // We have to measure.
        // It would be nice to save the result of this, if the string is
        // very long (which is certainly possible), but the overhead of
        // doing so for short strings is probably not worth it.
        //
        // Almost certainly the right thing is to add complexity only if
        // profiling shows this to be a bottleneck.
        //
        // We do this measurment is reusing the I/O system, this has the advantage
        // of whatever the I/O system's encoding error behavior is, this will
        // reuse that. However, we're not, per-se, required to detect that here,
        // but we do need to get the exact same number of bits here as will
        // be output to the actual data output stream later, so reusing the
        // DOS insures that we're using the exact same encoder initialized
        // the exact same way.
        //
        // TODO: PERFORMANCE:
        // We also do have the option of recycling everything here from pools,
        // or, since each unparser is single threaded and has its own state,
        // we could put temp space for this in the UState and just directly
        // reuse it.
        //
        state.withByteArrayOutputStream {
          case (_, dos) =>
            dos.setEncoder(state.dataOutputStream.encoder)
            val nChars = dos.putString(str)
            val nBits = dos.relBitPos0b.toLong
            (nBits, nChars)
        }

      }
    res
  }

  override def unparse(state: UState) {

    //
    // We have to stage the bits of the value just so as to be able to count them
    // Then we can figure out the number of padChars to add because a padChar must
    // be a minimum-width character.
    //
    val dos = state.dataOutputStream
    val valueString = contentString(state)
    val valueToWrite = {
      //
      // Historic note about lessons learned.
      //
      // We used to think unparsing would be symmetric to parsing, and there
      // would be this ability to bind the length limit, and then use it
      // to know how much to unparse, depending on the length limit to detect
      // overruns (too much data), and to tell us the size for the truncation
      // case.
      //
      // This is depending on some outer unparser binding the length limit
      // so that we know how long to truncate to.
      // And that we're not even being called until that target length is known
      // and has been bound.
      //
      // But data output streams don't do binding of length limits the
      // same way as the parser, because in many situations the data output
      // stream before and after aren't the same object.
      //
      // So this concept doesn't work.
      //
      // Really we want to just get the target length in bits, knowing that
      // by the time we get here it DOES have a value.
      //
      val maybeTargetLengthInBits: MaybeJULong = targetLengthInBitsEv.evaluate(state)
      Assert.invariant(maybeTargetLengthInBits.isDefined)
      val targetLengthInBits = maybeTargetLengthInBits.get
      val (nBits, nChars) = getLengthInBits(valueString, state)
      val targetLengthDiff = nBits - targetLengthInBits // positive if we need to truncate
      val nBitsToTrim = targetLengthDiff
      val dcs = erd.encInfo.getDFDLCharset(state)
      val minBitsPerChar = erd.encodingInfo.encodingMinimumCodePointWidthInBits(dcs)
      // padChar must be a minimum-width char
      val nCharsToTrim = nBitsToTrim / minBitsPerChar // positive if we need to truncate.
      Assert.invariant(nCharsToTrim <= nChars)
      val truncatedValue = truncateByJustification(state, valueString, nChars - nCharsToTrim.toInt)
      Assert.invariant(truncatedValue.length <= valueString.length)
      truncatedValue
    }

    val nCharsWritten = dos.putString(valueToWrite)
    Assert.invariant(nCharsWritten == valueToWrite.length) // assertion because we figured this out above based on available space.
    //
    // Filling of unused bits is done elsewhere now
    //
  }
}

/**
 * Truncates strings to the right length measured in characters.
 *
 * LengthUnits is 'characters', but we still don't know what
 * encoding so whether it is fixed or variable width.
 *
 * What's interesting is that we don't care whether the encoding is
 * fixed or variable width in this case.
 *
 * This is more efficient for variable-width encodings than
 * lengthUnits 'bytes' (or bits), because we don't need a pass to measure
 * the number of bits.
 *
 * So, for utf-8, we should recommend lengthUnits 'characters' ? Maybe so.
 */
class StringMaybeTruncateCharactersUnparser(
  lengthInCharactersEv: LengthEv,
  stringTruncationType: TextTruncationType.Type,
  erd: ElementRuntimeData)
  extends StringSpecifiedLengthUnparserTruncateBase(
    stringTruncationType,
    erd) {

  override lazy val runtimeDependencies: Seq[Evaluatable[AnyRef]] = List(lengthInCharactersEv)

  override def unparse(state: UState) {
    val dos = state.dataOutputStream
    val valueString = contentString(state)
    val targetLengthInCharacters =
      lengthInCharactersEv.evaluate(state)
    val valueToWrite = {
      if (targetLengthInCharacters >= valueString.length) {
        valueString
      } else {
        //
        // We might need to truncate
        //
        val nChars = valueString.length.toLong
        val nCharsToTrim = nChars - targetLengthInCharacters
        Assert.invariant(nCharsToTrim <= nChars)
        val truncatedValue = truncateByJustification(state, valueString, nChars - nCharsToTrim.toInt)
        Assert.invariant(truncatedValue.length <= valueString.length)
        truncatedValue
      }
    }
    //
    val nCharsWritten = dos.putString(valueToWrite)
    Assert.invariant(nCharsWritten == valueToWrite.length)
  }
}
