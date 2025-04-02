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

package org.apache.daffodil.unparsers.runtime1

import java.nio.charset.MalformedInputException
import java.nio.charset.UnmappableCharacterException

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.MaybeJULong
import org.apache.daffodil.runtime1.processors.CharsetEv
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Evaluatable
import org.apache.daffodil.runtime1.processors.LengthEv
import org.apache.daffodil.runtime1.processors.TextTruncationType
import org.apache.daffodil.runtime1.processors.UnparseTargetLengthInBitsEv
import org.apache.daffodil.runtime1.processors.unparsers._

sealed abstract class StringSpecifiedLengthUnparserBase(val erd: ElementRuntimeData)
  extends TextPrimUnparser {

  override def context = erd

  /**
   * override in nil specified length unparsers
   */
  protected def contentString(state: UState) =
    state.currentInfosetNode.asSimple.dataValueAsString

}

class StringNoTruncateUnparser(erd: ElementRuntimeData)
  extends StringSpecifiedLengthUnparserBase(erd) {

  override def runtimeDependencies: Vector[Evaluatable[AnyRef]] = Vector()

  override def unparse(state: UState): Unit = {
    val dos = state.getDataOutputStream
    val valueToWrite = contentString(state)
    val nCharsWritten =
      try {
        dos.putString(valueToWrite, state)
      } catch {
        case m: MalformedInputException => {
          UE(state, "%s - MalformedInputException: \n%s", nom, m.getMessage())
        }
        case u: UnmappableCharacterException => {
          UE(state, "%s - UnmappableCharacterException: \n%s", nom, u.getMessage())
        }
      }
    Assert.invariant(nCharsWritten == valueToWrite.length)
  }

}

sealed abstract class StringSpecifiedLengthUnparserTruncateBase(
  stringTruncationType: TextTruncationType.Type,
  erd: ElementRuntimeData
) extends StringSpecifiedLengthUnparserBase(erd) {

  Assert.usage(stringTruncationType ne TextTruncationType.None)

  /**
   * We only truncate strings, and only if textStringJustification is left or
   * right, and only if truncateSpecifiedLengthString is yes.
   */
  protected final def truncateByJustification(
    ustate: UState,
    str: String,
    nChars: Long
  ): String = {
    Assert.invariant(erd.optTruncateSpecifiedLengthString.isDefined)
    val nCharsToTrim = str.length - nChars.toInt
    val result = stringTruncationType match {
      case TextTruncationType.Right => {
        str.substring(nCharsToTrim)
      }
      case TextTruncationType.Left => {
        str.substring(0, str.length - nCharsToTrim)
      }
      case TextTruncationType.ErrorIfNeeded => {
        // justification type was "center", which cannot be truncated, so
        // should be an error
        UE(
          ustate,
          "Truncation required but disallowed when dfdl:truncateSpecifiedLengthString=\"yes\" and dfdl:textStringJustification=\"center\""
        )
      }
      case TextTruncationType.None => {
        Assert.invariantFailed("cannot be TextTruncationType.None")
      }
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
  charsetEv: CharsetEv
) extends StringSpecifiedLengthUnparserTruncateBase(stringTruncationType, erd) {

  override def runtimeDependencies = Vector(targetLengthInBitsEv, charsetEv)

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
        state.withByteArrayOutputStream { case (_, dos) =>
          val nChars = dos.putString(str, state)
          val nBits = dos.relBitPos0b.toLong
          (nBits, nChars)
        }

      }
    res
  }

  override def unparse(state: UState): Unit = {

    //
    // We have to stage the bits of the value just so as to be able to count them
    // Then we can figure out the number of padChars to add because a padChar must
    // be a minimum-width character.
    //
    val dos = state.getDataOutputStream
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
      val targetLengthDiff = nBits - targetLengthInBits
      if (targetLengthDiff <= 0) {
        // truncation is not needed, just write the original string
        valueString
      } else {
        val nBitsToTrim = targetLengthDiff
        val dcs = erd.encInfo.getDFDLCharset(state)
        val minBitsPerChar = erd.encodingInfo.encodingMinimumCodePointWidthInBits(dcs)
        // padChar must be a minimum-width char
        val nCharsToTrim = nBitsToTrim / minBitsPerChar // positive if we need to truncate.
        Assert.invariant(nCharsToTrim <= nChars)
        val truncatedValue =
          truncateByJustification(state, valueString, nChars - nCharsToTrim.toInt)
        Assert.invariant(truncatedValue.length <= valueString.length)
        truncatedValue
      }
    }

    val nCharsWritten = dos.putString(valueToWrite, state)
    Assert.invariant(
      nCharsWritten == valueToWrite.length
    ) // assertion because we figured this out above based on available space.
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
  erd: ElementRuntimeData
) extends StringSpecifiedLengthUnparserTruncateBase(stringTruncationType, erd) {

  override def runtimeDependencies = Vector(lengthInCharactersEv)

  override def unparse(state: UState): Unit = {
    val dos = state.getDataOutputStream
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
        val truncatedValue =
          truncateByJustification(state, valueString, nChars - nCharsToTrim.toInt)
        Assert.invariant(truncatedValue.length <= valueString.length)
        truncatedValue
      }
    }
    //
    val nCharsWritten = dos.putString(valueToWrite, state)
    Assert.invariant(nCharsWritten == valueToWrite.length)
  }
}
